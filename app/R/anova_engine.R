# ==============================================================================
# anova_engine.R
# Core pure base-R experimental design ANOVA, Split-Plot dual-error partitioning,
# Piepho (2004) compact letter display (CLD), agricultural precision indices,
# assumption diagnostics, outlier screening, and plain-English interpretation.
# ==============================================================================

.format_sig_stars <- function(p) {
  vapply(p, function(val) {
    if (is.na(val)) return("")
    if (val < 0.001) return("***")
    if (val < 0.01) return("**")
    if (val < 0.05) return("*")
    if (val < 0.10) return(".")
    "NS"
  }, character(1))
}

.make_letters_from_index <- function(k) {
  if (k <= 26L) return(letters[k])
  paste0(letters[((k - 1L) %/% 26L)], letters[((k - 1L) %% 26L) + 1L])
}

#' Compute Pairwise Comparisons and Compact Letter Display (CLD)
#'
#' Implements Tukey's HSD (\code{qtukey}), Fisher's Protected/Unprotected LSD
#' (\code{qt}), and Bonferroni-adjusted pairwise t-tests with Piepho's (2004)
#' insert-absorb sweep algorithm for exact compact letter groupings.
#'
#' @param means Named numeric vector of group means.
#' @param n_rep Named or aligned numeric vector of group replicate counts.
#' @param error_ms Numeric mean square error from the appropriate ANOVA stratum.
#' @param error_df Numeric residual degrees of freedom.
#' @param alpha Significance level (default \code{0.05}).
#' @param method One of \code{"tukey"}, \code{"lsd"}, or \code{"bonferroni"}.
#' @param f_pvalue Optional omnibus ANOVA F-test p-value used when \code{protected = TRUE}.
#' @param protected Logical; if \code{TRUE} and \code{method == "lsd"} with
#'   \code{f_pvalue > alpha}, all treatments receive the same letter \code{"a"}.
#' @return A list containing \code{letters} (named character vector ordered like input),
#'   \code{p_matrix} (pairwise adjusted p-values), \code{diff_matrix} (logical),
#'   and \code{note}.
#' @examples
#' m <- c(N0 = 2.8, N60 = 4.2, N120 = 5.3, N150 = 5.2)
#' compute_pairwise_cld(m, rep(4, 4), error_ms = 0.04, error_df = 15)
#' @export
compute_pairwise_cld <- function(means,
                                 n_rep,
                                 error_ms,
                                 error_df,
                                 alpha = 0.05,
                                 method = c("tukey", "lsd", "bonferroni"),
                                 f_pvalue = NULL,
                                 protected = TRUE) {
  method <- match.arg(method)
  k <- length(means)
  nms <- names(means)
  if (is.null(nms)) nms <- paste0("G", seq_len(k))

  if (k <= 1L || is.na(error_ms) || is.na(error_df) || error_df <= 0 || error_ms <= 0) {
    return(list(
      letters = stats::setNames(rep("a", k), nms),
      p_matrix = matrix(1, k, k, dimnames = list(nms, nms)),
      diff_matrix = matrix(FALSE, k, k, dimnames = list(nms, nms)),
      note = "Insufficient levels or zero residual variance for pairwise separation."
    ))
  }

  # Sort descending by mean for top-down letter assignment
  ord <- order(means, decreasing = TRUE)
  m_ord <- means[ord]
  n_ord <- n_rep[ord]
  nms_ord <- nms[ord]

  p_mat <- matrix(1, nrow = k, ncol = k, dimnames = list(nms_ord, nms_ord))
  diff_mat <- matrix(FALSE, nrow = k, ncol = k, dimnames = list(nms_ord, nms_ord))
  n_pairs <- choose(k, 2)

  note <- ""
  if (protected && method == "lsd" && !is.null(f_pvalue) && !is.na(f_pvalue) && f_pvalue > alpha) {
    note <- paste0("Omnibus F-test is non-significant (p = ", round(f_pvalue, 4),
                   " > ", alpha, "); Fisher's Protected LSD retains all means in group 'a'.")
    out_letters <- stats::setNames(rep("a", k), nms)
    return(list(letters = out_letters, p_matrix = p_mat, diff_matrix = diff_mat, note = note))
  }

  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      se_diff <- sqrt(error_ms * (1 / n_ord[i] + 1 / n_ord[j]))
      diff_val <- abs(m_ord[i] - m_ord[j])
      if (se_diff == 0) {
        pval <- if (diff_val == 0) 1 else 0
      } else if (method == "tukey") {
        q_stat <- diff_val / sqrt(error_ms * 0.5 * (1 / n_ord[i] + 1 / n_ord[j]))
        pval <- stats::ptukey(q_stat, nmeans = k, df = error_df, lower.tail = FALSE)
      } else {
        t_stat <- diff_val / se_diff
        raw_p <- 2 * stats::pt(t_stat, df = error_df, lower.tail = FALSE)
        pval <- if (method == "bonferroni") min(1, raw_p * n_pairs) else raw_p
      }
      p_mat[i, j] <- p_mat[j, i] <- pval
      is_diff <- !is.na(pval) && pval < alpha
      diff_mat[i, j] <- diff_mat[j, i] <- is_diff
    }
  }

  # Piepho (2004) insert-absorb sweep algorithm
  cols_mat <- matrix(TRUE, nrow = k, ncol = 1L)
  absorb_cols <- function(mat) {
    if (ncol(mat) <= 1L) return(mat)
    keep <- rep(TRUE, ncol(mat))
    for (c1 in seq_len(ncol(mat))) {
      if (!keep[c1]) next
      for (c2 in seq_len(ncol(mat))) {
        if (c1 == c2 || !keep[c2]) next
        # Remove c1 if it is a subset of c2 (including duplicates where c1 < c2 keeps c1)
        if (all(mat[, c1] <= mat[, c2])) {
          if (all(mat[, c1] == mat[, c2]) && c1 < c2) {
            keep[c2] <- FALSE
          } else if (!all(mat[, c1] == mat[, c2])) {
            keep[c1] <- FALSE
            break
          }
        }
      }
    }
    mat[, keep, drop = FALSE]
  }

  for (i in seq_len(k - 1L)) {
    for (j in (i + 1L):k) {
      if (!diff_mat[i, j]) next
      conflict_cols <- which(cols_mat[i, ] & cols_mat[j, ])
      if (length(conflict_cols) == 0L) next
      new_cols <- list()
      for (cc in conflict_cols) {
        c_drop_j <- cols_mat[, cc]
        c_drop_j[j] <- FALSE
        c_drop_i <- cols_mat[, cc]
        c_drop_i[i] <- FALSE
        new_cols[[length(new_cols) + 1L]] <- c_drop_j
        new_cols[[length(new_cols) + 1L]] <- c_drop_i
      }
      cols_mat <- cols_mat[, -conflict_cols, drop = FALSE]
      if (length(new_cols) > 0L) {
        cols_mat <- cbind(cols_mat, do.call(cbind, new_cols))
      }
      cols_mat <- absorb_cols(cols_mat)
    }
  }

  # Sort letter columns top-down so highest mean always starts with 'a'
  first_pos <- apply(cols_mat, 2, function(col) {
    w <- which(col)
    if (length(w) == 0L) Inf else min(w)
  })
  last_pos <- apply(cols_mat, 2, function(col) {
    w <- which(col)
    if (length(w) == 0L) Inf else max(w)
  })
  cols_mat <- cols_mat[, order(first_pos, last_pos), drop = FALSE]

  letter_labels <- vapply(seq_len(ncol(cols_mat)), .make_letters_from_index, character(1))
  cld_ord <- apply(cols_mat, 1, function(row_flags) {
    paste(letter_labels[row_flags], collapse = "")
  })
  names(cld_ord) <- nms_ord
  out_letters <- cld_ord[nms]

  list(
    letters = out_letters,
    p_matrix = p_mat[nms, nms, drop = FALSE],
    diff_matrix = diff_mat[nms, nms, drop = FALSE],
    note = note
  )
}

# Median-centered Brown-Forsythe test for homoscedasticity (pure base R)
.brown_forsythe_test <- function(y, group_factor) {
  grp <- as.factor(group_factor)
  if (nlevels(grp) < 2L || length(y) <= nlevels(grp)) {
    return(list(statistic = NA_real_, p_value = NA_real_, method = "Brown-Forsythe (Levene median)"))
  }
  meds <- tapply(y, grp, stats::median, na.rm = TRUE)
  z <- abs(y - meds[as.character(grp)])
  if (all(z == 0, na.rm = TRUE)) {
    return(list(statistic = 0, p_value = 1, method = "Brown-Forsythe (Levene median)"))
  }
  fit <- stats::lm(z ~ grp)
  tab <- stats::anova(fit)
  list(
    statistic = as.numeric(tab[["F value"]][1]),
    p_value = as.numeric(tab[["Pr(>F)"]][1]),
    method = "Brown-Forsythe (Levene median)"
  )
}

.build_means_table <- function(y_trans,
                               y_orig,
                               group_vec,
                               error_ms,
                               error_df,
                               alpha,
                               posthoc,
                               f_pvalue,
                               protected,
                               transform,
                               scale_param) {
  grp <- factor(group_vec, levels = unique(as.character(group_vec)))
  lvls <- levels(grp)
  n_rep <- as.numeric(table(grp)[lvls])
  names(n_rep) <- lvls
  m_trans <- as.numeric(tapply(y_trans, grp, mean))
  names(m_trans) <- lvls
  sd_trans <- as.numeric(tapply(y_trans, grp, stats::sd))
  sd_orig <- as.numeric(tapply(y_orig, grp, stats::sd))
  m_orig_raw <- as.numeric(tapply(y_orig, grp, mean))

  cld_res <- compute_pairwise_cld(
    means = m_trans,
    n_rep = n_rep,
    error_ms = error_ms,
    error_df = error_df,
    alpha = alpha,
    method = posthoc,
    f_pvalue = f_pvalue,
    protected = protected
  )

  se_vec <- sqrt(error_ms / n_rep)
  t_crit <- if (!is.na(error_df) && error_df > 0) stats::qt(1 - alpha / 2, df = error_df) else NA_real_
  ci_low <- m_trans - t_crit * se_vec
  ci_high <- m_trans + t_crit * se_vec
  gm <- mean(y_trans)
  pct_vs_gm <- if (gm != 0) ((m_trans - gm) / abs(gm)) * 100 else rep(0, length(m_trans))

  out <- data.frame(
    Treatment = lvls,
    n = as.integer(n_rep),
    Mean = round(m_trans, 4),
    SE = round(se_vec, 4),
    SD_sample = round(sd_trans, 4),
    CI_Lower = round(ci_low, 4),
    CI_Upper = round(ci_high, 4),
    Pct_vs_GrandMean = round(pct_vs_gm, 2),
    Group = unname(cld_res$letters[lvls]),
    stringsAsFactors = FALSE
  )

  if (transform != "none") {
    out$BackTransformed_Mean <- round(back_transform_mean(m_trans, transform, scale_param), 4)
    out$Raw_Arithmetic_Mean <- round(m_orig_raw, 4)
    out$Raw_SD <- round(sd_orig, 4)
  }

  out <- out[order(-out$Mean), , drop = FALSE]
  out$Rank <- seq_len(nrow(out))
  out <- out[, c("Rank", setdiff(names(out), "Rank")), drop = FALSE]
  rownames(out) <- NULL
  list(table = out, cld_info = cld_res)
}

#' Run Complete Field Trial Statistical Analysis
#'
#' Fits the selected experimental design model (\code{crd}, \code{rcbd}, \code{lsd},
#' \code{factorial_rcbd}, or \code{split_plot}), builds an agronomy-standard ANOVA
#' table (including dual Error(a)/Error(b) partitioning for Split-Plot), computes
#' SE(m), SE(d), Critical Difference (CD / LSD), CV%, Relative Efficiency,
#' Compact Letter Display (CLD) treatment rankings, residual diagnostics, and
#' outlier screening.
#'
#' @param df Input \code{data.frame}.
#' @param design Experimental design identifier.
#' @param mapping Column mapping list (e.g. from \code{\link{detect_trial_columns}}).
#' @param transform Data transformation: \code{"none"}, \code{"log10"},
#'   \code{"log1p"}, \code{"sqrt"}, or \code{"arcsine_pct"}.
#' @param alpha Significance level (default \code{0.05}).
#' @param posthoc Pairwise comparison test: \code{"tukey"}, \code{"lsd"}, or \code{"bonferroni"}.
#' @param protected Logical; enforce Fisher's Protected LSD rule when \code{posthoc == "lsd"}.
#' @return A rich S3-like list of class \code{"fieldtrial_result"} containing
#'   ANOVA tables, precision metrics, means & CLD tables, diagnostics, outliers,
#'   spatial data (if row/col available), and metadata.
#' @examples
#' df <- load_sample_trial("wheat_rcbd")
#' res <- analyse_field_trial(df, "rcbd", detect_trial_columns(df))
#' res$anova_table
#' res$means_table
#' @export
analyse_field_trial <- function(df,
                                design = c("rcbd", "crd", "lsd", "factorial_rcbd", "split_plot"),
                                mapping,
                                transform = c("none", "log10", "log1p", "sqrt", "arcsine_pct"),
                                alpha = 0.05,
                                posthoc = c("tukey", "lsd", "bonferroni"),
                                protected = TRUE) {
  design <- match.arg(design)
  transform <- match.arg(transform)
  posthoc <- match.arg(posthoc)

  val <- validate_trial_design(df, design, mapping)
  if (!val$ok) {
    stop(paste(val$errors, collapse = " "), call. = FALSE)
  }

  req_cols <- switch(
    design,
    crd = c("response", "treatment"),
    rcbd = c("response", "treatment", "block"),
    lsd = c("response", "treatment", "row", "col"),
    factorial_rcbd = c("response", "factor_a", "factor_b", "block"),
    split_plot = c("response", "main_plot", "sub_plot", "block")
  )

  y_raw <- suppressWarnings(as.numeric(df[[mapping$response]]))
  keep <- !is.na(y_raw)
  for (rc in setdiff(req_cols, "response")) {
    v <- as.character(df[[mapping[[rc]]]])
    keep <- keep & !is.na(v) & nzchar(trimws(v))
  }

  sub <- df[keep, , drop = FALSE]
  y_orig <- suppressWarnings(as.numeric(sub[[mapping$response]]))
  tr_obj <- apply_transformation(y_orig, transform)
  y <- tr_obj$values
  sub$.y <- y
  sub$.y_orig <- y_orig

  grand_mean <- mean(y)
  extra_tables <- list()
  re_info <- list()
  split_metrics <- NULL

  if (design == "crd") {
    sub$.trt <- factor(as.character(sub[[mapping$treatment]]),
                       levels = unique(as.character(sub[[mapping$treatment]])))
    fit <- stats::lm(.y ~ .trt, data = sub)
    atab <- stats::anova(fit)
    anova_df <- data.frame(
      Source = c(mapping$treatment, "Residuals (Error)"),
      DF = as.integer(atab$Df),
      SS = round(atab[["Sum Sq"]], 5),
      MS = round(atab[["Mean Sq"]], 5),
      F_value = round(atab[["F value"]], 4),
      p_value = signif(atab[["Pr(>F)"]], 5),
      Sig = .format_sig_stars(atab[["Pr(>F)"]]),
      stringsAsFactors = FALSE
    )
    err_df <- atab$Df[2]
    err_ms <- atab[["Mean Sq"]][2]
    f_p <- atab[["Pr(>F)"]][1]
    primary_grp <- sub$.trt
    diag_grp <- sub$.trt
    resid_vals <- stats::residuals(fit)
    fitted_vals <- stats::fitted(fit)
    rstd_vals <- suppressWarnings(stats::rstandard(fit))
    cooks_vals <- suppressWarnings(stats::cooks.distance(fit))
  } else if (design == "rcbd") {
    sub$.blk <- factor(as.character(sub[[mapping$block]]),
                       levels = unique(as.character(sub[[mapping$block]])))
    sub$.trt <- factor(as.character(sub[[mapping$treatment]]),
                       levels = unique(as.character(sub[[mapping$treatment]])))
    fit <- stats::lm(.y ~ .blk + .trt, data = sub)
    atab <- stats::anova(fit)
    anova_df <- data.frame(
      Source = c(paste0("Block (", mapping$block, ")"),
                 paste0("Treatment (", mapping$treatment, ")"),
                 "Residuals (Error)"),
      DF = as.integer(atab$Df),
      SS = round(atab[["Sum Sq"]], 5),
      MS = round(atab[["Mean Sq"]], 5),
      F_value = round(atab[["F value"]], 4),
      p_value = signif(atab[["Pr(>F)"]], 5),
      Sig = .format_sig_stars(atab[["Pr(>F)"]]),
      stringsAsFactors = FALSE
    )
    err_df <- atab$Df[3]
    err_ms <- atab[["Mean Sq"]][3]
    f_p <- atab[["Pr(>F)"]][2]
    blk_df <- atab$Df[1]
    blk_ms <- atab[["Mean Sq"]][1]
    trt_df <- atab$Df[2]

    # Cochran & Cox (1957) Relative Efficiency of RCBD vs CRD with Fisher DF correction
    if (!is.na(err_ms) && err_ms > 0 && (blk_df + trt_df + err_df) > 0) {
      ms_crd_est <- (blk_df * blk_ms + (trt_df + err_df) * err_ms) / (blk_df + trt_df + err_df)
      df_crd <- trt_df + err_df + blk_df - trt_df
      df_adj <- ((err_df + 1) * (df_crd + 3)) / ((err_df + 3) * (df_crd + 1))
      re_pct <- (ms_crd_est / err_ms) * df_adj * 100
      re_info$rcbd_vs_crd_pct <- round(re_pct, 1)
    }

    primary_grp <- sub$.trt
    diag_grp <- sub$.trt
    resid_vals <- stats::residuals(fit)
    fitted_vals <- stats::fitted(fit)
    rstd_vals <- suppressWarnings(stats::rstandard(fit))
    cooks_vals <- suppressWarnings(stats::cooks.distance(fit))
  } else if (design == "lsd") {
    sub$.row <- factor(as.character(sub[[mapping$row]]),
                       levels = unique(as.character(sub[[mapping$row]])))
    sub$.col <- factor(as.character(sub[[mapping$col]]),
                       levels = unique(as.character(sub[[mapping$col]])))
    sub$.trt <- factor(as.character(sub[[mapping$treatment]]),
                       levels = unique(as.character(sub[[mapping$treatment]])))
    fit <- stats::lm(.y ~ .row + .col + .trt, data = sub)
    atab <- stats::anova(fit)
    anova_df <- data.frame(
      Source = c(paste0("Row (", mapping$row, ")"),
                 paste0("Column (", mapping$col, ")"),
                 paste0("Treatment (", mapping$treatment, ")"),
                 "Residuals (Error)"),
      DF = as.integer(atab$Df),
      SS = round(atab[["Sum Sq"]], 5),
      MS = round(atab[["Mean Sq"]], 5),
      F_value = round(atab[["F value"]], 4),
      p_value = signif(atab[["Pr(>F)"]], 5),
      Sig = .format_sig_stars(atab[["Pr(>F)"]]),
      stringsAsFactors = FALSE
    )
    err_df <- atab$Df[4]
    err_ms <- atab[["Mean Sq"]][4]
    f_p <- atab[["Pr(>F)"]][3]
    row_df <- atab$Df[1]; row_ms <- atab[["Mean Sq"]][1]
    col_df <- atab$Df[2]; col_ms <- atab[["Mean Sq"]][2]
    trt_df <- atab$Df[3]
    if (!is.na(err_ms) && err_ms > 0) {
      # Relative efficiency of LSD vs RCBD (with Rows as blocks, and with Cols as blocks)
      re_rows <- ((col_df * col_ms + (trt_df + err_df) * err_ms) / ((col_df + trt_df + err_df) * err_ms)) * 100
      re_cols <- ((row_df * row_ms + (trt_df + err_df) * err_ms) / ((row_df + trt_df + err_df) * err_ms)) * 100
      re_info$lsd_vs_rcbd_rows_pct <- round(re_rows, 1)
      re_info$lsd_vs_rcbd_cols_pct <- round(re_cols, 1)
    }

    primary_grp <- sub$.trt
    diag_grp <- sub$.trt
    resid_vals <- stats::residuals(fit)
    fitted_vals <- stats::fitted(fit)
    rstd_vals <- suppressWarnings(stats::rstandard(fit))
    cooks_vals <- suppressWarnings(stats::cooks.distance(fit))
  } else if (design == "factorial_rcbd") {
    sub$.blk <- factor(as.character(sub[[mapping$block]]),
                       levels = unique(as.character(sub[[mapping$block]])))
    sub$.fa <- factor(as.character(sub[[mapping$factor_a]]),
                      levels = unique(as.character(sub[[mapping$factor_a]])))
    sub$.fb <- factor(as.character(sub[[mapping$factor_b]]),
                      levels = unique(as.character(sub[[mapping$factor_b]])))
    sub$.cell <- factor(paste(sub$.fa, sub$.fb, sep = " × "),
                        levels = unique(paste(sub$.fa, sub$.fb, sep = " × ")))

    fit <- stats::lm(.y ~ .blk + .fa * .fb, data = sub)
    atab <- stats::anova(fit)
    anova_df <- data.frame(
      Source = c(paste0("Block (", mapping$block, ")"),
                 paste0("Factor A (", mapping$factor_a, ")"),
                 paste0("Factor B (", mapping$factor_b, ")"),
                 paste0(mapping$factor_a, " x ", mapping$factor_b),
                 "Residuals (Error)"),
      DF = as.integer(atab$Df),
      SS = round(atab[["Sum Sq"]], 5),
      MS = round(atab[["Mean Sq"]], 5),
      F_value = round(atab[["F value"]], 4),
      p_value = signif(atab[["Pr(>F)"]], 5),
      Sig = .format_sig_stars(atab[["Pr(>F)"]]),
      stringsAsFactors = FALSE
    )
    err_df <- atab$Df[5]
    err_ms <- atab[["Mean Sq"]][5]
    f_p <- atab[["Pr(>F)"]][4] # Interaction F p-value for cell comparisons

    extra_tables$factor_a <- .build_means_table(
      y, y_orig, sub$.fa, err_ms, err_df, alpha, posthoc, atab[["Pr(>F)"]][2],
      protected, transform, tr_obj$pct_scale
    )$table
    extra_tables$factor_b <- .build_means_table(
      y, y_orig, sub$.fb, err_ms, err_df, alpha, posthoc, atab[["Pr(>F)"]][3],
      protected, transform, tr_obj$pct_scale
    )$table

    primary_grp <- sub$.cell
    diag_grp <- sub$.cell
    resid_vals <- stats::residuals(fit)
    fitted_vals <- stats::fitted(fit)
    rstd_vals <- suppressWarnings(stats::rstandard(fit))
    cooks_vals <- suppressWarnings(stats::cooks.distance(fit))
  } else {
    # Split-Plot Design with exact Error(a) and Error(b) partitioning
    sub$.blk <- factor(as.character(sub[[mapping$block]]),
                       levels = unique(as.character(sub[[mapping$block]])))
    sub$.mp <- factor(as.character(sub[[mapping$main_plot]]),
                      levels = unique(as.character(sub[[mapping$main_plot]])))
    sub$.sp <- factor(as.character(sub[[mapping$sub_plot]]),
                      levels = unique(as.character(sub[[mapping$sub_plot]])))
    sub$.cell <- factor(paste(sub$.mp, sub$.sp, sep = " × "),
                        levels = unique(paste(sub$.mp, sub$.sp, sep = " × ")))

    fit_full <- stats::lm(.y ~ .blk + .mp + .blk:.mp + .sp + .mp:.sp, data = sub)
    atab <- stats::anova(fit_full)

    # Extract components by name cleanly
    rn <- rownames(atab)
    get_row <- function(nm) {
      idx <- match(nm, rn)
      if (is.na(idx)) list(df = 0L, ss = 0, ms = NA_real_)
      else list(df = as.integer(atab$Df[idx]), ss = atab[["Sum Sq"]][idx], ms = atab[["Mean Sq"]][idx])
    }
    r_blk <- get_row(".blk")
    r_mp  <- get_row(".mp")
    r_ea  <- get_row(".blk:.mp")
    r_sp  <- get_row(".sp")
    r_int <- get_row(".mp:.sp")
    r_eb  <- get_row("Residuals")

    f_blk <- if (!is.na(r_ea$ms) && r_ea$ms > 0) r_blk$ms / r_ea$ms else NA_real_
    p_blk <- if (!is.na(f_blk) && r_blk$df > 0 && r_ea$df > 0) stats::pf(f_blk, r_blk$df, r_ea$df, lower.tail = FALSE) else NA_real_

    f_mp <- if (!is.na(r_ea$ms) && r_ea$ms > 0) r_mp$ms / r_ea$ms else NA_real_
    p_mp <- if (!is.na(f_mp) && r_mp$df > 0 && r_ea$df > 0) stats::pf(f_mp, r_mp$df, r_ea$df, lower.tail = FALSE) else NA_real_

    f_sp <- if (!is.na(r_eb$ms) && r_eb$ms > 0) r_sp$ms / r_eb$ms else NA_real_
    p_sp <- if (!is.na(f_sp) && r_sp$df > 0 && r_eb$df > 0) stats::pf(f_sp, r_sp$df, r_eb$df, lower.tail = FALSE) else NA_real_

    f_int <- if (!is.na(r_eb$ms) && r_eb$ms > 0) r_int$ms / r_eb$ms else NA_real_
    p_int <- if (!is.na(f_int) && r_int$df > 0 && r_eb$df > 0) stats::pf(f_int, r_int$df, r_eb$df, lower.tail = FALSE) else NA_real_

    p_vec <- c(p_blk, p_mp, NA_real_, p_sp, p_int, NA_real_)
    anova_df <- data.frame(
      Source = c(
        paste0("Block (", mapping$block, ")"),
        paste0("Main Plot A (", mapping$main_plot, ")"),
        "Error (a) [Block x Main Plot]",
        paste0("Sub Plot B (", mapping$sub_plot, ")"),
        paste0("Interaction (", mapping$main_plot, " x ", mapping$sub_plot, ")"),
        "Error (b) [Sub-Plot Residual]"
      ),
      DF = c(r_blk$df, r_mp$df, r_ea$df, r_sp$df, r_int$df, r_eb$df),
      SS = round(c(r_blk$ss, r_mp$ss, r_ea$ss, r_sp$ss, r_int$ss, r_eb$ss), 5),
      MS = round(c(r_blk$ms, r_mp$ms, r_ea$ms, r_sp$ms, r_int$ms, r_eb$ms), 5),
      F_value = round(c(f_blk, f_mp, NA_real_, f_sp, f_int, NA_real_), 4),
      p_value = signif(p_vec, 5),
      Sig = .format_sig_stars(p_vec),
      stringsAsFactors = FALSE
    )

    err_df <- r_eb$df
    err_ms <- r_eb$ms
    f_p <- p_int

    a_lev <- nlevels(sub$.mp)
    b_lev <- nlevels(sub$.sp)
    r_lev <- nlevels(sub$.blk)
    cv_a <- if (grand_mean != 0 && !is.na(r_ea$ms)) (sqrt(r_ea$ms) / abs(grand_mean)) * 100 else NA_real_
    cv_b <- if (grand_mean != 0 && !is.na(r_eb$ms)) (sqrt(r_eb$ms) / abs(grand_mean)) * 100 else NA_real_

    # Standard Gomez & Gomez / ICAR Split-Plot SEm & SEd formulas
    sem_a <- sqrt(r_ea$ms / (r_lev * b_lev))
    sed_a <- sqrt(2 * r_ea$ms / (r_lev * b_lev))
    cd_a <- sed_a * stats::qt(1 - alpha / 2, df = r_ea$df)

    sem_b <- sqrt(r_eb$ms / (r_lev * a_lev))
    sed_b <- sqrt(2 * r_eb$ms / (r_lev * a_lev))
    cd_b <- sed_b * stats::qt(1 - alpha / 2, df = r_eb$df)

    # Sub-plots at same main-plot level
    sed_b_same_a <- sqrt(2 * r_eb$ms / r_lev)
    cd_b_same_a <- sed_b_same_a * stats::qt(1 - alpha / 2, df = r_eb$df)

    # Main plots at same or different sub-plot levels (Satterthwaite weighted t)
    sed_a_diff_b <- sqrt(2 * ((b_lev - 1) * r_eb$ms + r_ea$ms) / (r_lev * b_lev))
    t_a <- stats::qt(1 - alpha / 2, df = r_ea$df)
    t_b <- stats::qt(1 - alpha / 2, df = r_eb$df)
    t_mix <- ((b_lev - 1) * r_eb$ms * t_b + r_ea$ms * t_a) / ((b_lev - 1) * r_eb$ms + r_ea$ms)
    cd_a_diff_b <- sed_a_diff_b * t_mix

    split_metrics <- data.frame(
      Comparison = c(
        paste0("Main Plot A (", mapping$main_plot, ")"),
        paste0("Sub Plot B (", mapping$sub_plot, ")"),
        "Sub Plot B at same Main Plot A",
        "Main Plot A at same/diff Sub Plot B"
      ),
      Error_DF = c(r_ea$df, r_eb$df, r_eb$df, NA_integer_),
      SEm = round(c(sem_a, sem_b, sqrt(r_eb$ms / r_lev), sed_a_diff_b / sqrt(2)), 4),
      SEd = round(c(sed_a, sed_b, sed_b_same_a, sed_a_diff_b), 4),
      CD_alpha = round(c(cd_a, cd_b, cd_b_same_a, cd_a_diff_b), 4),
      stringsAsFactors = FALSE
    )

    extra_tables$factor_a <- .build_means_table(
      y, y_orig, sub$.mp, r_ea$ms, r_ea$df, alpha, posthoc, p_mp,
      protected, transform, tr_obj$pct_scale
    )$table
    extra_tables$factor_b <- .build_means_table(
      y, y_orig, sub$.sp, r_eb$ms, r_eb$df, alpha, posthoc, p_sp,
      protected, transform, tr_obj$pct_scale
    )$table

    primary_grp <- sub$.cell
    diag_grp <- sub$.cell
    resid_vals <- stats::residuals(fit_full)
    fitted_vals <- stats::fitted(fit_full)
    rstd_vals <- suppressWarnings(stats::rstandard(fit_full))
    cooks_vals <- suppressWarnings(stats::cooks.distance(fit_full))
    re_info$cv_a_pct <- round(cv_a, 2)
    re_info$cv_b_pct <- round(cv_b, 2)
  }

  # Primary treatment / interaction cell means + CLD
  m_res <- .build_means_table(
    y_trans = y,
    y_orig = y_orig,
    group_vec = primary_grp,
    error_ms = err_ms,
    error_df = err_df,
    alpha = alpha,
    posthoc = posthoc,
    f_pvalue = f_p,
    protected = protected,
    transform = transform,
    scale_param = tr_obj$pct_scale
  )

  # Precision summary (using harmonic mean n for balanced & unbalanced designs)
  n_per_grp <- as.numeric(table(primary_grp))
  n_harm <- length(n_per_grp) / sum(1 / n_per_grp)
  cv_pct <- if (grand_mean != 0 && !is.na(err_ms)) (sqrt(err_ms) / abs(grand_mean)) * 100 else NA_real_
  sem <- if (!is.na(err_ms)) sqrt(err_ms / n_harm) else NA_real_
  sed <- if (!is.na(err_ms)) sqrt(2 * err_ms / n_harm) else NA_real_
  cd_05 <- if (!is.na(sed) && !is.na(err_df) && err_df > 0) sed * stats::qt(0.975, df = err_df) else NA_real_
  cd_01 <- if (!is.na(sed) && !is.na(err_df) && err_df > 0) sed * stats::qt(0.995, df = err_df) else NA_real_
  hsd_alpha <- if (!is.na(sem) && !is.na(err_df) && err_df > 0) sem * stats::qtukey(1 - alpha, nmeans = length(n_per_grp), df = err_df) else NA_real_

  # Diagnostics: Shapiro-Wilk + Brown-Forsythe + Bartlett
  shapiro_res <- if (length(resid_vals) >= 3L && length(resid_vals) <= 5000L && stats::sd(resid_vals) > 0) {
    tryCatch(stats::shapiro.test(resid_vals), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
  } else {
    list(statistic = NA_real_, p.value = NA_real_)
  }
  bf_res <- .brown_forsythe_test(y, diag_grp)
  bart_res <- tryCatch(stats::bartlett.test(y ~ diag_grp), error = function(e) list(statistic = NA_real_, p.value = NA_real_))

  # Outlier screening (|r_std| > 2.5 or substantial Cook's D > 0.5)
  n_obs <- nrow(sub)
  cook_thresh <- 0.5
  rstd_clean <- ifelse(is.finite(rstd_vals), rstd_vals, 0)
  cook_clean <- ifelse(is.finite(cooks_vals), cooks_vals, 0)
  flag_idx <- which(abs(rstd_clean) > 2.5 | cook_clean > cook_thresh)
  outliers_df <- if (length(flag_idx) > 0L) {
    data.frame(
      Plot_Row = flag_idx,
      Group = as.character(primary_grp[flag_idx]),
      Observed = round(y[flag_idx], 4),
      Fitted = round(fitted_vals[flag_idx], 4),
      Residual = round(resid_vals[flag_idx], 4),
      Std_Resid = round(rstd_clean[flag_idx], 3),
      Cooks_D = round(cook_clean[flag_idx], 3),
      Reason = ifelse(abs(rstd_clean[flag_idx]) > 2.5, "|Std Resid| > 2.5", "High Cook's D"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Plot_Row = integer(0), Group = character(0), Observed = numeric(0),
      Fitted = numeric(0), Residual = numeric(0), Std_Resid = numeric(0),
      Cooks_D = numeric(0), Reason = character(0), stringsAsFactors = FALSE
    )
  }

  # Spatial data if row/col provided or inferable
  sub$.fitted <- fitted_vals
  sub$.resid <- resid_vals
  sub$.rstd <- rstd_clean
  sub$.group <- as.character(primary_grp)

  precision <- list(
    grand_mean = round(grand_mean, 4),
    cv_pct = round(cv_pct, 2),
    error_df = err_df,
    error_ms = round(err_ms, 5),
    sem = round(sem, 4),
    sed = round(sed, 4),
    cd_05 = round(cd_05, 4),
    cd_01 = round(cd_01, 4),
    hsd_alpha = round(hsd_alpha, 4),
    n_harmonic = round(n_harm, 2),
    is_balanced = val$is_balanced,
    re_info = re_info
  )

  diagnostics <- list(
    shapiro_w = unname(as.numeric(shapiro_res$statistic)),
    shapiro_p = as.numeric(shapiro_res$p.value),
    levene_f = bf_res$statistic,
    levene_p = bf_res$p_value,
    bartlett_p = as.numeric(bart_res$p.value),
    n_outliers = nrow(outliers_df)
  )

  out <- list(
    design = design,
    mapping = mapping,
    transform = tr_obj,
    alpha = alpha,
    posthoc = posthoc,
    protected = protected,
    validation = val,
    anova_table = anova_df,
    precision = precision,
    split_metrics = split_metrics,
    means_table = m_res$table,
    cld_info = m_res$cld_info,
    extra_tables = extra_tables,
    diagnostics = diagnostics,
    outliers = outliers_df,
    data = sub
  )
  class(out) <- "fieldtrial_result"
  out
}

#' Compare Candidate Data Transformations on Current Trial
#'
#' Evaluates raw, natural-log, square-root, and arcsine transformations and
#' recommends the best variance-stabilizing option based on Shapiro-Wilk and
#' Brown-Forsythe diagnostic tests.
#'
#' @param df Input \code{data.frame}.
#' @param design Experimental design identifier.
#' @param mapping Column mapping list.
#' @param alpha Significance threshold (default \code{0.05}).
#' @return A list with \code{comparison} (\code{data.frame}) and
#'   \code{recommended} (transformation key).
#' @examples
#' df <- load_sample_trial("chickpea_crd")
#' map <- detect_trial_columns(df)
#' map$response <- "nodules_plant"
#' recommend_transformations(df, "crd", map)
#' @export
recommend_transformations <- function(df, design, mapping, alpha = 0.05) {
  y_raw <- suppressWarnings(as.numeric(df[[mapping$response]]))
  y_valid <- y_raw[!is.na(y_raw)]
  candidates <- c("none", "log1p", "sqrt")
  if (length(y_valid) > 0L && min(y_valid) >= 0 && max(y_valid) <= 100) {
    candidates <- c(candidates, "arcsine_pct")
  }

  rows <- list()
  for (tr in candidates) {
    res <- tryCatch(
      analyse_field_trial(df, design, mapping, transform = tr, alpha = alpha),
      error = function(e) NULL
    )
    if (is.null(res)) next
    sp <- res$diagnostics$shapiro_p
    lp <- res$diagnostics$levene_p
    passed <- !is.na(sp) && !is.na(lp) && sp >= alpha && lp >= alpha
    rows[[length(rows) + 1L]] <- data.frame(
      Transform = tr,
      Label = res$transform$label,
      Shapiro_p = round(sp, 4),
      Levene_p = round(lp, 4),
      CV_pct = res$precision$cv_pct,
      Assumptions_OK = passed,
      Score = (if (is.na(sp)) 0 else min(sp, 0.5)) + (if (is.na(lp)) 0 else min(lp, 0.5)),
      stringsAsFactors = FALSE
    )
  }

  comp <- do.call(rbind, rows)
  if (is.null(comp) || nrow(comp) == 0L) {
    return(list(comparison = data.frame(), recommended = "none"))
  }

  # Keep raw scale unless raw fails assumptions or another transform cleanly repairs both tests
  none_ok <- isTRUE(comp$Assumptions_OK[comp$Transform == "none"][1])
  rec <- if (none_ok) {
    "none"
  } else {
    passing <- comp[comp$Assumptions_OK, , drop = FALSE]
    if (nrow(passing) > 0L) {
      passing$Transform[which.max(passing$Score)]
    } else {
      comp$Transform[which.max(comp$Score)]
    }
  }
  comp$Score <- NULL
  rownames(comp) <- NULL
  list(comparison = comp, recommended = rec)
}

#' Generate Plain-English Agronomic Interpretation Paragraph
#'
#' Produces a publication-style summary of significance, top-performing
#' treatment(s), precision (CV%), blocking efficiency, and assumption health.
#'
#' @param res Output object from \code{\link{analyse_field_trial}}.
#' @return Character scalar summary paragraph.
#' @examples
#' df <- load_sample_trial("wheat_rcbd")
#' res <- analyse_field_trial(df, "rcbd", detect_trial_columns(df))
#' cat(generate_interpretation_text(res))
#' @export
generate_interpretation_text <- function(res) {
  at <- res$anova_table
  mt <- res$means_table
  pr <- res$precision
  dg <- res$diagnostics
  resp <- res$mapping$response

  # Pick primary effect row(s)
  if (res$design == "crd") {
    eff_row <- at[1, ]
    f_phrase <- sprintf("Treatment (%s) showed %s effects (F(%d, %d) = %.2f, p = %.4g)",
                        res$mapping$treatment,
                        ifelse(!is.na(eff_row$p_value) && eff_row$p_value < res$alpha, "statistically significant", "non-significant"),
                        eff_row$DF, pr$error_df, eff_row$F_value, eff_row$p_value)
  } else if (res$design == "rcbd") {
    eff_row <- at[2, ]
    re_txt <- if (!is.null(pr$re_info$rcbd_vs_crd_pct)) {
      sprintf(" Blocking achieved %.1f%% relative efficiency compared with an unblocked CRD.", pr$re_info$rcbd_vs_crd_pct)
    } else ""
    f_phrase <- sprintf("In the RCBD analysis of %s, treatment effects were %s (F(%d, %d) = %.2f, p = %.4g).%s",
                        resp,
                        ifelse(!is.na(eff_row$p_value) && eff_row$p_value < res$alpha, "statistically significant", "non-significant"),
                        eff_row$DF, pr$error_df, eff_row$F_value, eff_row$p_value, re_txt)
  } else if (res$design == "lsd") {
    eff_row <- at[3, ]
    f_phrase <- sprintf("In the Latin Square analysis of %s, treatment effects were %s (F(%d, %d) = %.2f, p = %.4g)",
                        resp,
                        ifelse(!is.na(eff_row$p_value) && eff_row$p_value < res$alpha, "statistically significant", "non-significant"),
                        eff_row$DF, pr$error_df, eff_row$F_value, eff_row$p_value)
  } else if (res$design == "factorial_rcbd") {
    rA <- at[2, ]; rB <- at[3, ]; rAB <- at[4, ]
    f_phrase <- sprintf("Factorial RCBD ANOVA of %s indicated Factor A (%s: p = %.4g), Factor B (%s: p = %.4g), and A x B interaction (p = %.4g)",
                        resp, res$mapping$factor_a, rA$p_value, res$mapping$factor_b, rB$p_value, rAB$p_value)
  } else {
    rA <- at[2, ]; rB <- at[4, ]; rAB <- at[5, ]
    f_phrase <- sprintf("Split-Plot ANOVA of %s showed Main-Plot %s (Error a, p = %.4g), Sub-Plot %s (Error b, p = %.4g), and interaction (p = %.4g)",
                        resp, res$mapping$main_plot, rA$p_value, res$mapping$sub_plot, rB$p_value, rAB$p_value)
  }

  top <- mt[1, ]
  bot <- mt[nrow(mt), ]
  gain_pct <- if (bot$Mean != 0) ((top$Mean - bot$Mean) / abs(bot$Mean)) * 100 else 0
  mean_phrase <- sprintf(
    " Across treatments, the grand mean was %.3f with SE(m) = %.3f and CD (5%%) = %.3f. The highest mean was recorded in '%s' (%.3f, group '%s'), which was %.1f%% higher than '%s' (%.3f, group '%s') under %s comparison (alpha = %.2f).",
    pr$grand_mean, pr$sem, pr$cd_05, top$Treatment, top$Mean, top$Group,
    gain_pct, bot$Treatment, bot$Mean, bot$Group, toupper(res$posthoc), res$alpha
  )

  cv_tier <- if (is.na(pr$cv_pct)) "undetermined" else if (pr$cv_pct <= 10) "high experimental precision" else if (pr$cv_pct <= 20) "acceptable field precision" else "elevated field variability (inspect spatial gradients/outliers)"
  norm_ok <- is.na(dg$shapiro_p) || dg$shapiro_p >= res$alpha
  hom_ok <- is.na(dg$levene_p) || dg$levene_p >= res$alpha
  diag_phrase <- sprintf(
    " Trial CV was %.2f%% (%s). Diagnostic checks indicated %s normality (Shapiro-Wilk p = %.3f) and %s variances (Brown-Forsythe p = %.3f), with %d flagged outlier plot(s).",
    pr$cv_pct, cv_tier,
    ifelse(norm_ok, "acceptable", "departures from"), dg$shapiro_p,
    ifelse(hom_ok, "homogeneous", "heterogeneous"), dg$levene_p,
    dg$n_outliers
  )

  paste0(f_phrase, mean_phrase, diag_phrase)
}
