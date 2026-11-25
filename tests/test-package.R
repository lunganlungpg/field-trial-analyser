# Package-level check script executed by R CMD check (uses installed `fieldtrial`)
library(fieldtrial)

stopifnot(nrow(list_sample_trials()) == 5L)

for (id in list_sample_trials()$id) {
  df <- load_sample_trial(id)
  cat_row <- list_sample_trials()[list_sample_trials()$id == id, ]
  mp <- detect_trial_columns(df)
  mp$response <- cat_row$default_response
  if (cat_row$design == "factorial_rcbd") {
    mp$factor_a <- "variety"
    mp$factor_b <- "sulphur"
  } else if (cat_row$design == "split_plot") {
    mp$main_plot <- "irrigation"
    mp$sub_plot <- "cultivar"
  }
  res <- analyse_field_trial(df, design = cat_row$design, mapping = mp)
  stopifnot(inherits(res, "fieldtrial_result"))
  stopifnot(nrow(res$means_table) >= 4L)
  stopifnot(nzchar(generate_interpretation_text(res)))
  stopifnot(grepl("<svg", svg_bar_chart(res), fixed = TRUE))
}

plan <- plan_replications(cv_pct = 12, min_diff_pct = 15, n_treatments = 6)
stopifnot(!is.na(plan$recommended_r), plan$achieved_power >= 0.80)
cat("Package tests passed.\n")
