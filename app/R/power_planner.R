# ==============================================================================
# power_planner.R
# Replicate planning and statistical power curves for agricultural field trials
# using pure base R non-central t / F distributions.
# ==============================================================================

#' Plan Replications Required for Desired Field Trial Power
#'
#' Computes minimum replications per treatment \eqn{r} to detect a pairwise
#' difference of \code{min_diff_pct} (\% of the grand mean) given expected
#' experimental \code{cv_pct}, design residual degrees of freedom, and \code{alpha}.
#'
#' @param cv_pct Expected Coefficient of Variation (\%).
#' @param min_diff_pct Minimum meaningful treatment difference to detect (\% of mean).
#' @param n_treatments Integer number of treatments (\eqn{\ge 2}).
#' @param design Experimental design (\code{"rcbd"}, \code{"crd"}, or \code{"lsd"}).
#' @param alpha Type-I error rate (default \code{0.05}).
#' @param target_power Desired statistical power (default \code{0.80}).
#' @return A list containing \code{recommended_r}, \code{achieved_power},
#'   \code{target_power}, \code{df_formula}, and \code{curve} (\code{data.frame}).
#' @examples
#' plan <- plan_replications(cv_pct = 12, min_diff_pct = 15, n_treatments = 6)
#' plan$recommended_r
#' @export
plan_replications <- function(cv_pct = 12,
                              min_diff_pct = 15,
                              n_treatments = 6L,
                              design = c("rcbd", "crd", "lsd"),
                              alpha = 0.05,
                              target_power = 0.80) {
  design <- match.arg(design)
  t_n <- as.integer(max(2L, n_treatments))

  if (cv_pct <= 0 || min_diff_pct <= 0) {
    stop("'cv_pct' and 'min_diff_pct' must be positive.", call. = FALSE)
  }

  r_grid <- if (design == "lsd") t_n else 2:16
  rows <- vector("list", length(r_grid))

  for (idx in seq_along(r_grid)) {
    r <- r_grid[idx]
    err_df <- switch(
      design,
      crd = t_n * (r - 1L),
      rcbd = (t_n - 1L) * (r - 1L),
      lsd = (t_n - 1L) * (t_n - 2L)
    )
    if (err_df <= 0L) {
      rows[[idx]] <- data.frame(
        Replications = r,
        Total_Plots = r * t_n,
        Error_DF = err_df,
        Power = 0,
        LSD_pct = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }
    se_diff_pct <- sqrt(2 / r) * cv_pct
    ncp <- min_diff_pct / se_diff_pct
    t_crit <- stats::qt(1 - alpha / 2, df = err_df)
    # Two-sided non-central t power via stats::pt
    pwr <- suppressWarnings(
      stats::pt(-t_crit, df = err_df, ncp = ncp) +
        stats::pt(t_crit, df = err_df, ncp = ncp, lower.tail = FALSE)
    )
    lsd_pct <- t_crit * se_diff_pct
    rows[[idx]] <- data.frame(
      Replications = r,
      Total_Plots = r * t_n,
      Error_DF = err_df,
      Power = round(pwr, 4),
      LSD_pct = round(lsd_pct, 2),
      stringsAsFactors = FALSE
    )
  }

  curve <- do.call(rbind, rows)
  hit <- which(curve$Power >= target_power)
  rec_r <- if (length(hit) > 0L) curve$Replications[hit[1]] else NA_integer_
  ach_pwr <- if (length(hit) > 0L) curve$Power[hit[1]] else max(curve$Power, na.rm = TRUE)

  list(
    recommended_r = rec_r,
    achieved_power = ach_pwr,
    target_power = target_power,
    cv_pct = cv_pct,
    min_diff_pct = min_diff_pct,
    n_treatments = t_n,
    design = design,
    alpha = alpha,
    curve = curve
  )
}
