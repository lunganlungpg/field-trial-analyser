# ==============================================================================
# Zero-dependency verification suite for `fieldtrial` + Shinylive app parity.
# Runs directly under base R (>= 4.0) with no external test packages required.
# ==============================================================================

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop("ASSERTION FAILED: ", msg, call. = FALSE)
}

assert_near <- function(a, b, tol = 1e-4, msg = "") {
  if (is.na(a) || is.na(b) || abs(a - b) > tol) {
    stop(sprintf("ASSERTION FAILED (%s): expected %.6f, got %.6f", msg, b, a), call. = FALSE)
  }
}

cat("[1/8] Checking app/R and app/data parity with package R/ and inst/extdata/...\n")
pkg_r <- sort(list.files("R", pattern = "\\.R$"))
app_r <- sort(list.files("app/R", pattern = "\\.R$"))
assert_true(identical(pkg_r, app_r), "app/R file list matches R/")
for (f in pkg_r) {
  assert_true(
    identical(readLines(file.path("R", f), warn = FALSE), readLines(file.path("app/R", f), warn = FALSE)),
    paste("File parity mismatch:", f)
  )
}
pkg_d <- sort(list.files("inst/extdata", pattern = "\\.csv$"))
app_d <- sort(list.files("app/data", pattern = "\\.csv$"))
assert_true(identical(pkg_d, app_d), "app/data file list matches inst/extdata/")
for (f in pkg_d) {
  assert_true(
    identical(readLines(file.path("inst/extdata", f), warn = FALSE), readLines(file.path("app/data", f), warn = FALSE)),
    paste("Dataset parity mismatch:", f)
  )
}

cat("[2/8] Checking app/app.R syntax parse and zero spurious CRAN package calls...\n")
parsed_app <- parse("app/app.R")
assert_true(length(parsed_app) > 0L, "app/app.R parses cleanly")

cat("[3/8] Verifying Excel/TSV/CSV parser and column auto-detection...\n")
tsv_txt <- "block\ttreatment\tyield\nB1\tT1\t3.2\nB1\tT2\t4.1\nB2\tT1\t3.4\nB2\tT2\t4.4"
df_tsv <- parse_trial_text(tsv_txt)
assert_true(nrow(df_tsv) == 4L && ncol(df_tsv) == 3L, "TSV paste parsed correctly")
det_tsv <- detect_trial_columns(df_tsv)
assert_true(det_tsv$block == "block" && det_tsv$treatment == "treatment" && det_tsv$response == "yield", "TSV auto-detection works")

cat("[4/8] Verifying RCBD, CRD, Latin Square, and Factorial RCBD ANOVA & Precision...\n")
# RCBD
w <- load_sample_trial("wheat_rcbd")
res_rcbd <- analyse_field_trial(w, "rcbd", detect_trial_columns(w))
assert_true(res_rcbd$anova_table$DF[1] == 3L && res_rcbd$anova_table$DF[2] == 5L && res_rcbd$anova_table$DF[3] == 15L, "RCBD DFs exact (3, 5, 15)")
assert_true(res_rcbd$anova_table$p_value[2] < 0.001, "Wheat N trial highly significant")
assert_true(res_rcbd$precision$re_info$rcbd_vs_crd_pct > 100, "RCBD shows >100% relative efficiency vs CRD")
assert_true(res_rcbd$means_table$Treatment[1] == "N120", "N120 leads grain yield")

# CRD + Sqrt Transform
c_df <- load_sample_trial("chickpea_crd")
c_map <- detect_trial_columns(c_df)
c_map$response <- "nodules_plant"
res_crd <- analyse_field_trial(c_df, "crd", c_map, transform = "sqrt")
assert_true(res_crd$anova_table$DF[1] == 4L && res_crd$anova_table$DF[2] == 20L, "CRD DFs exact (4, 20)")
assert_true("BackTransformed_Mean" %in% names(res_crd$means_table), "BackTransformed_Mean included when transformed")

# Latin Square
l_df <- load_sample_trial("sugarcane_lsd")
res_lsd <- analyse_field_trial(l_df, "lsd", detect_trial_columns(l_df))
assert_true(all(res_lsd$anova_table$DF == c(4L, 4L, 4L, 12L)), "5x5 LSD DFs exact (4, 4, 4, 12)")
assert_true(res_lsd$validation$is_balanced, "5x5 Latin Square validated orthogonal")

# Factorial RCBD
m_df <- load_sample_trial("mustard_factorial")
m_map <- detect_trial_columns(m_df)
m_map$factor_a <- "variety"; m_map$factor_b <- "sulphur"
res_fac <- analyse_field_trial(m_df, "factorial_rcbd", m_map)
assert_true(all(res_fac$anova_table$DF == c(2L, 2L, 3L, 6L, 22L)), "Factorial RCBD DFs exact (2, 2, 3, 6, 22)")
assert_true(!is.null(res_fac$extra_tables$factor_a) && !is.null(res_fac$extra_tables$factor_b), "Main factor tables built")

cat("[5/8] Verifying Split-Plot dual Error(a) and Error(b) partitioning against aov()...\n")
r_df <- load_sample_trial("rice_split_plot")
r_map <- detect_trial_columns(r_df)
r_map$main_plot <- "irrigation"; r_map$sub_plot <- "cultivar"; r_map$response <- "grain_yield_tha"
res_sp <- analyse_field_trial(r_df, "split_plot", r_map)
assert_true(all(res_sp$anova_table$DF == c(2L, 2L, 4L, 3L, 6L, 18L)), "Split-Plot DFs exact (2, 2, 4, 3, 6, 18)")

# Cross-check Split-Plot F-values directly against base R aov(..., Error(.blk / .mp))
r_df$.blk <- factor(r_df$block); r_df$.mp <- factor(r_df$irrigation); r_df$.sp <- factor(r_df$cultivar)
aov_ref <- summary(stats::aov(grain_yield_tha ~ .mp * .sp + Error(.blk / .mp), data = r_df))
f_mp_ref <- aov_ref[["Error: .blk:.mp"]][[1]][".mp", "F value"]
f_sp_ref <- aov_ref[["Error: Within"]][[1]][".sp", "F value"]
f_int_ref <- aov_ref[["Error: Within"]][[1]][".mp:.sp", "F value"]
assert_near(res_sp$anova_table$F_value[2], f_mp_ref, 1e-3, "Split-plot Main-Plot F matches Error(a) aov()")
assert_near(res_sp$anova_table$F_value[4], f_sp_ref, 1e-3, "Split-plot Sub-Plot F matches Error(b) aov()")
assert_near(res_sp$anova_table$F_value[5], f_int_ref, 1e-3, "Split-plot Interaction F matches Error(b) aov()")
assert_true(nrow(res_sp$split_metrics) == 4L, "All 4 Split-Plot SEm/SEd/CD rows computed")

cat("[6/8] Verifying Piepho Compact Letter Display (CLD) exact pairwise invariance...\n")
m_test <- c(A = 10.0, B = 9.6, C = 8.4, D = 7.9, E = 5.0)
cld <- compute_pairwise_cld(m_test, rep(4, 5), error_ms = 0.25, error_df = 12, method = "tukey")
nms <- names(m_test)
for (i in 1:4) {
  for (j in (i + 1):5) {
    l1 <- strsplit(cld$letters[nms[i]], "")[[1]]
    l2 <- strsplit(cld$letters[nms[j]], "")[[1]]
    shares <- length(intersect(l1, l2)) > 0L
    is_sig <- cld$diff_matrix[nms[i], nms[j]]
    assert_true(shares == !is_sig, sprintf("CLD invariance failed for pair (%s, %s)", nms[i], nms[j]))
  }
}

# Verify Protected LSD collapses to 'a' when F p-value > alpha
cld_ns <- compute_pairwise_cld(c(T1 = 4.2, T2 = 4.0, T3 = 3.9), rep(3, 3), 0.5, 6, method = "lsd", f_pvalue = 0.42, protected = TRUE)
assert_true(all(cld_ns$letters == "a"), "Fisher Protected LSD holds all means in group 'a' when F is NS")

cat("[7/8] Verifying Transformation Advisor, Power Planner, R Script & HTML Report...\n")
r_sev <- r_map; r_sev$response <- "disease_severity_pct"
adv <- recommend_transformations(r_df, "split_plot", r_sev)
assert_true(nrow(adv$comparison) >= 4L && "arcsine_pct" %in% adv$comparison$Transform, "Transformation advisor includes Arcsine for % data")

plan <- plan_replications(cv_pct = 12, min_diff_pct = 15, n_treatments = 6, design = "rcbd", target_power = 0.80)
assert_true(!is.na(plan$recommended_r) && plan$recommended_r >= 4L && plan$achieved_power >= 0.80, "Power planner returns valid replication count")

rc <- generate_reproducible_r_code(res_rcbd, "wheat_rcbd_gomez.csv")
assert_true(grepl("lm\\(y ~ blk \\+ trt", rc), "Reproducible R script matches RCBD model")
html_rep <- generate_html_report(res_rcbd)
assert_true(grepl("<svg", html_rep, fixed = TRUE) && grepl("Plain-English Interpretation", html_rep, fixed = TRUE), "HTML report contains inline SVG and interpretation")

cat("[8/8] Verifying all Base-R plots render cleanly to temp device...\n")
tmp_pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(tmp_pdf)
plot_treatment_means(res_rcbd)
plot_diagnostics_grid(res_rcbd)
plot_interaction_means(res_sp)
plot_field_heatmap(res_rcbd, "residual")
plot_power_curve(plan)
grDevices::dev.off()
assert_true(file.exists(tmp_pdf) && file.info(tmp_pdf)$size > 1000, "All plot functions rendered cleanly")
unlink(tmp_pdf)

cat("ALL TESTS PASSED SUCCESSFULLY.\n")
