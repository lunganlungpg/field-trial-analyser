# ==============================================================================
# plotting.R
# Publication-grade base R graphics for Shiny/webR canvas AND pure-R inline
# vector SVG generators for standalone reports & interactive field maps.
# ==============================================================================

.theme_palette <- function(dark = FALSE) {
  if (dark) {
    list(
      bg = "#0f172a",
      panel = "#1e293b",
      fg = "#e2e8f0",
      muted = "#94a3b8",
      grid = "#334155",
      primary = "#34d399",
      secondary = "#38bdf8",
      accent = "#fbbf24",
      danger = "#f87171",
      point = "#f8fafc"
    )
  } else {
    list(
      bg = "#ffffff",
      panel = "#f8fafc",
      fg = "#0f172a",
      muted = "#475569",
      grid = "#e2e8f0",
      primary = "#15803d",
      secondary = "#0284c7",
      accent = "#d97706",
      danger = "#dc2626",
      point = "#0f172a"
    )
  }
}

#' Plot Ordered Treatment Means with Error Bars, Raw Observations & CLD Letters
#'
#' @param res Result object from \code{\link{analyse_field_trial}}.
#' @param error_bar Error bar type: \code{"se"}, \code{"ci95"}, or \code{"none"}.
#' @param show_points Logical; overlay individual plot observations.
#' @param dark Logical; apply dark canvas theme.
#' @return Invisibly returns \code{res$means_table}.
#' @export
plot_treatment_means <- function(res,
                                 error_bar = c("se", "ci95", "none"),
                                 show_points = TRUE,
                                 dark = FALSE) {
  error_bar <- match.arg(error_bar)
  pal <- .theme_palette(dark)
  mt <- res$means_table
  ord <- seq_len(nrow(mt))
  n_g <- nrow(mt)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  max_lbl <- max(nchar(mt$Treatment), 8)
  bottom_mar <- if (n_g > 6 || max_lbl > 10) 7.5 else 5.2
  graphics::par(bg = pal$bg, fg = pal$fg, col.axis = pal$muted, col.lab = pal$fg,
                col.main = pal$fg, mar = c(bottom_mar, 5.2, 4.2, 1.8), las = 1)

  # Compute vertical limits cleanly including points & CLD label headroom
  raw_y <- res$data$.y
  raw_g <- res$data$.group
  eb_vals <- if (error_bar == "se") mt$SE else if (error_bar == "ci95") (mt$CI_Upper - mt$Mean) else rep(0, n_g)
  eb_vals[is.na(eb_vals)] <- 0
  top_vals <- c(mt$Mean + eb_vals, if (show_points) raw_y else numeric(0))
  bot_vals <- c(0, mt$Mean - eb_vals, if (show_points) raw_y else numeric(0))
  y_min <- min(0, min(bot_vals, na.rm = TRUE))
  y_max <- max(top_vals, na.rm = TRUE)
  span <- max(1e-6, y_max - y_min)
  ylim <- c(y_min, y_max + 0.18 * span)

  # Highlight top letter group ('a') in deep green, others in slate teal
  is_top <- grepl("^a", mt$Group)
  bar_cols <- ifelse(is_top, grDevices::adjustcolor(pal$primary, alpha.f = 0.88),
                     grDevices::adjustcolor(pal$secondary, alpha.f = 0.72))

  bp <- graphics::barplot(
    height = mt$Mean,
    names.arg = rep("", n_g),
    ylim = ylim,
    col = bar_cols,
    border = NA,
    ylab = paste0(res$mapping$response,
                  if (res$transform$transform != "none") paste0(" [", res$transform$label, "]") else ""),
    main = paste0("Treatment Mean Comparison (CLD: ", toupper(res$posthoc), ", alpha = ", res$alpha, ")"),
    cex.main = 1.08
  )
  graphics::grid(nx = NA, ny = NULL, col = pal$grid, lty = 2)
  graphics::barplot(mt$Mean, names.arg = rep("", n_g), add = TRUE, col = bar_cols, border = NA, axes = FALSE)

  # X-axis labels angled cleanly when dense
  if (n_g > 6 || max_lbl > 10) {
    graphics::text(x = bp, y = ylim[1] - 0.04 * span, labels = mt$Treatment,
                   srt = 32, adj = 1, xpd = TRUE, cex = 0.85, col = pal$fg)
  } else {
    graphics::axis(1, at = bp, labels = mt$Treatment, tick = FALSE, cex.axis = 0.92, col.axis = pal$fg)
  }

  # Raw plot points jittered deterministically
  if (show_points) {
    for (i in ord) {
      pts <- raw_y[raw_g == mt$Treatment[i]]
      if (length(pts) > 0L) {
        offs <- seq(-0.18, 0.18, length.out = length(pts))
        if (length(pts) == 1L) offs <- 0
        graphics::points(bp[i] + offs, pts, pch = 21, bg = pal$bg, col = pal$point, cex = 0.95, lwd = 1.2)
      }
    }
  }

  # Error bars
  if (error_bar != "none") {
    lo <- mt$Mean - eb_vals
    hi <- mt$Mean + eb_vals
    graphics::segments(bp, lo, bp, hi, col = pal$fg, lwd = 1.6)
    cap <- 0.10
    graphics::segments(bp - cap, lo, bp + cap, lo, col = pal$fg, lwd = 1.6)
    graphics::segments(bp - cap, hi, bp + cap, hi, col = pal$fg, lwd = 1.6)
  }

  # CLD letters directly above error bars/points
  lbl_y <- pmax(mt$Mean + eb_vals, vapply(mt$Treatment, function(g) {
    pts <- raw_y[raw_g == g]
    if (length(pts) > 0L) max(pts, na.rm = TRUE) else -Inf
  }, numeric(1))) + 0.05 * span
  graphics::text(bp, lbl_y, labels = mt$Group, font = 2, cex = 1.02, col = pal$fg)

  graphics::mtext(
    sprintf("Grand Mean = %.3f  |  CV = %.2f%%  |  SE(m) = %.3f  |  CD (5%%) = %.3f",
            res$precision$grand_mean, res$precision$cv_pct, res$precision$sem, res$precision$cd_05),
    side = 3, line = 0.35, cex = 0.85, col = pal$muted
  )
  graphics::box(bty = "l", col = pal$grid)
  invisible(mt)
}

#' Plot 2 x 2 Residual & Assumption Diagnostics Grid
#'
#' @param res Result object from \code{\link{analyse_field_trial}}.
#' @param dark Logical; apply dark canvas theme.
#' @export
plot_diagnostics_grid <- function(res, dark = FALSE) {
  pal <- .theme_palette(dark)
  d <- res$data
  fit <- d$.fitted
  resids <- d$.resid
  rstd <- d$.rstd

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(2, 2), bg = pal$bg, fg = pal$fg, col.axis = pal$muted,
                col.lab = pal$fg, col.main = pal$fg, mar = c(4.2, 4.4, 3.0, 1.2), las = 1)

  # 1. Residuals vs Fitted
  graphics::plot(fit, resids, pch = 19, col = grDevices::adjustcolor(pal$secondary, 0.8),
                 xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted")
  graphics::grid(col = pal$grid, lty = 2)
  graphics::abline(h = 0, col = pal$accent, lwd = 1.5, lty = 2)
  graphics::box(bty = "l", col = pal$grid)

  # 2. Normal Q-Q
  stats::qqnorm(rstd, pch = 19, col = grDevices::adjustcolor(pal$primary, 0.85),
                main = sprintf("Normal Q-Q (Shapiro p = %.3f)", res$diagnostics$shapiro_p),
                xlab = "Theoretical Quantiles", ylab = "Standardized Residuals")
  graphics::grid(col = pal$grid, lty = 2)
  if (length(rstd) >= 3 && stats::sd(rstd, na.rm = TRUE) > 0) {
    stats::qqline(rstd, col = pal$accent, lwd = 1.6, lty = 2)
  }
  graphics::box(bty = "l", col = pal$grid)

  # 3. Scale-Location
  sr <- sqrt(abs(rstd))
  graphics::plot(fit, sr, pch = 19, col = grDevices::adjustcolor(pal$secondary, 0.8),
                 xlab = "Fitted Values", ylab = "sqrt(|Std Resid|)",
                 main = sprintf("Scale-Location (Levene p = %.3f)", res$diagnostics$levene_p))
  graphics::grid(col = pal$grid, lty = 2)
  graphics::box(bty = "l", col = pal$grid)

  # 4. Standardized Residuals by Plot Sequence (Outlier Watch)
  idx <- seq_along(rstd)
  pt_cols <- ifelse(abs(rstd) > 2.5, pal$danger, pal$primary)
  ylim_r <- range(c(-3, 3, rstd), na.rm = TRUE)
  graphics::plot(idx, rstd, type = "h", col = pal$muted, ylim = ylim_r,
                 xlab = "Plot Sequence Index", ylab = "Standardized Residual",
                 main = "Outlier Check (|Std Resid| > 2.5 Flagged)")
  graphics::grid(col = pal$grid, lty = 2)
  graphics::abline(h = c(-2.5, 0, 2.5), col = c(pal$danger, pal$muted, pal$danger), lty = c(2, 1, 2), lwd = 1.2)
  graphics::points(idx, rstd, pch = 19, col = pt_cols, cex = 1.05)
  graphics::box(bty = "l", col = pal$grid)
}

#' Plot Two-Factor Interaction Profile (Factorial RCBD / Split-Plot)
#'
#' @param res Result object from \code{\link{analyse_field_trial}}.
#' @param dark Logical; apply dark canvas theme.
#' @export
plot_interaction_means <- function(res, dark = FALSE) {
  pal <- .theme_palette(dark)
  if (!(res$design %in% c("factorial_rcbd", "split_plot"))) {
    plot.new()
    graphics::text(0.5, 0.5, "Interaction plot applies to Factorial RCBD and Split-Plot designs.", cex = 1.1)
    return(invisible(NULL))
  }

  fa_col <- if (res$design == "factorial_rcbd") res$mapping$factor_a else res$mapping$main_plot
  fb_col <- if (res$design == "factorial_rcbd") res$mapping$factor_b else res$mapping$sub_plot
  d <- res$data
  fa <- factor(d[[fa_col]], levels = unique(as.character(d[[fa_col]])))
  fb <- factor(d[[fb_col]], levels = unique(as.character(d[[fb_col]])))
  agg <- stats::aggregate(d$.y, by = list(A = fa, B = fb), FUN = mean)

  lv_a <- levels(fa)
  lv_b <- levels(fb)
  cols <- c("#15803d", "#0284c7", "#d97706", "#9333ea", "#dc2626", "#0d9488")
  if (dark) cols <- c("#34d399", "#38bdf8", "#fbbf24", "#c084fc", "#f87171", "#2dd4bf")

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(bg = pal$bg, fg = pal$fg, col.axis = pal$muted, col.lab = pal$fg,
                col.main = pal$fg, mar = c(5.2, 5.2, 4.2, 2.0), las = 1)

  x_pos <- seq_along(lv_b)
  yr <- range(agg$x, na.rm = TRUE)
  pad <- max(1e-6, diff(yr) * 0.16)
  graphics::plot(NA, xlim = c(0.7, length(lv_b) + 0.3), ylim = c(yr[1] - pad * 0.4, yr[2] + pad),
                 xaxt = "n", xlab = fb_col, ylab = res$mapping$response,
                 main = paste0("Interaction Profile: ", fa_col, " x ", fb_col))
  graphics::grid(col = pal$grid, lty = 2)
  graphics::axis(1, at = x_pos, labels = lv_b, col.axis = pal$fg)

  for (i in seq_along(lv_a)) {
    sub_i <- agg[agg$A == lv_a[i], ]
    y_i <- sub_i$x[match(lv_b, sub_i$B)]
    c_i <- cols[((i - 1L) %% length(cols)) + 1L]
    graphics::lines(x_pos, y_i, type = "b", pch = 15 + ((i - 1L) %% 4L), lwd = 2.4, cex = 1.2, col = c_i)
  }
  graphics::legend("topleft", legend = lv_a, title = fa_col,
                   col = cols[((seq_along(lv_a) - 1L) %% length(cols)) + 1L],
                   lwd = 2.4, pch = 15 + ((seq_along(lv_a) - 1L) %% 4L),
                   bty = "n", text.col = pal$fg)
  graphics::box(bty = "l", col = pal$grid)
}

#' Plot Spatial Field Trial Heatmap
#'
#' Visualizes plot-by-plot yields or model residuals across field rows and columns
#' to uncover fertility gradients, drainage edges, or localized pest patches.
#'
#' @param res Result object from \code{\link{analyse_field_trial}}.
#' @param metric \code{"observed"} or \code{"residual"}.
#' @param dark Logical; apply dark canvas theme.
#' @export
plot_field_heatmap <- function(res, metric = c("observed", "residual"), dark = FALSE) {
  metric <- match.arg(metric)
  pal <- .theme_palette(dark)
  d <- res$data

  row_name <- res$mapping$row
  col_name <- res$mapping$col
  if (!is.null(row_name) && nzchar(row_name) && row_name %in% names(d) &&
      !is.null(col_name) && nzchar(col_name) && col_name %in% names(d)) {
    r_fac <- factor(d[[row_name]], levels = unique(as.character(d[[row_name]])))
    c_fac <- factor(d[[col_name]], levels = unique(as.character(d[[col_name]])))
  } else if (!is.null(res$mapping$block) && nzchar(res$mapping$block) && res$mapping$block %in% names(d)) {
    r_fac <- factor(d[[res$mapping$block]], levels = unique(as.character(d[[res$mapping$block]])))
    c_idx <- stats::ave(seq_len(nrow(d)), r_fac, FUN = seq_along)
    c_fac <- factor(paste0("P", c_idx), levels = paste0("P", seq_len(max(c_idx))))
  } else {
    n <- nrow(d)
    nc <- ceiling(sqrt(n))
    nr <- ceiling(n / nc)
    r_fac <- factor(paste0("R", rep(seq_len(nr), each = nc)[seq_len(n)]))
    c_fac <- factor(paste0("C", rep(seq_len(nc), times = nr)[seq_len(n)]))
  }

  vals <- if (metric == "observed") d$.y else d$.resid
  r_lvls <- levels(r_fac)
  c_lvls <- levels(c_fac)
  nr <- length(r_lvls)
  nc <- length(c_lvls)

  ramp <- if (metric == "observed") {
    grDevices::colorRampPalette(c("#fef3c7", "#86efac", "#15803d"))(64)
  } else {
    grDevices::colorRampPalette(c("#ef4444", "#f8fafc", "#2563eb"))(64)
  }

  v_range <- range(vals, na.rm = TRUE)
  if (metric == "residual") {
    m_abs <- max(abs(v_range), 1e-6)
    v_range <- c(-m_abs, m_abs)
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(bg = pal$bg, fg = pal$fg, col.axis = pal$muted, col.lab = pal$fg,
                col.main = pal$fg, mar = c(4.2, 4.8, 3.8, 1.5), las = 1)

  graphics::plot(NA, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5),
                 xaxt = "n", yaxt = "n", xlab = "Column / Plot Position", ylab = "Row / Block",
                 main = ifelse(metric == "observed",
                               paste0("Spatial Field Map: ", res$mapping$response),
                               "Spatial Residual Map (Fertility & Edge Diagnostics)"))
  graphics::axis(1, at = seq_len(nc), labels = c_lvls, col.axis = pal$fg)
  graphics::axis(2, at = seq_len(nr), labels = rev(r_lvls), col.axis = pal$fg)

  for (k in seq_len(nrow(d))) {
    ri <- nr - as.integer(r_fac[k]) + 1L
    ci <- as.integer(c_fac[k])
    v <- vals[k]
    frac <- if (diff(v_range) > 0) (v - v_range[1]) / diff(v_range) else 0.5
    c_idx <- max(1L, min(64L, round(frac * 63) + 1L))
    graphics::rect(ci - 0.46, ri - 0.44, ci + 0.46, ri + 0.44, col = ramp[c_idx], border = pal$grid, lwd = 1.2)
    lbl <- paste0(substr(d$.group[k], 1, 10), "\n", sprintf("%.2f", v))
    graphics::text(ci, ri, labels = lbl, cex = if (nc > 8) 0.65 else 0.78, col = "#0f172a", font = 2)
  }
}

#' Plot Statistical Power Curve Across Replication Counts
#'
#' @param plan_obj Output from \code{\link{plan_replications}}.
#' @param dark Logical; apply dark canvas theme.
#' @export
plot_power_curve <- function(plan_obj, dark = FALSE) {
  pal <- .theme_palette(dark)
  cv <- plan_obj$curve
  cv <- cv[cv$Error_DF > 0, , drop = FALSE]

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(bg = pal$bg, fg = pal$fg, col.axis = pal$muted, col.lab = pal$fg,
                col.main = pal$fg, mar = c(4.6, 5.0, 3.8, 1.5), las = 1)

  graphics::plot(cv$Replications, cv$Power * 100, type = "b", pch = 19, lwd = 2.6,
                 col = pal$primary, ylim = c(0, 105),
                 xlab = "Replications per Treatment (r)",
                 ylab = "Statistical Power (%)",
                 main = sprintf("Replicate Power Curve (CV = %.1f%%, Min Diff = %.1f%%)",
                                plan_obj$cv_pct, plan_obj$min_diff_pct))
  graphics::grid(col = pal$grid, lty = 2)
  graphics::abline(h = plan_obj$target_power * 100, col = pal$accent, lty = 2, lwd = 1.8)
  if (!is.na(plan_obj$recommended_r)) {
    graphics::abline(v = plan_obj$recommended_r, col = pal$secondary, lty = 3, lwd = 1.8)
    graphics::points(plan_obj$recommended_r, plan_obj$achieved_power * 100, pch = 21, bg = pal$accent, col = pal$fg, cex = 1.6, lwd = 1.5)
  }
  graphics::legend("bottomright",
                   legend = c("Achieved Power", paste0("Target (", round(plan_obj$target_power * 100), "%)")),
                   col = c(pal$primary, pal$accent), lty = c(1, 2), lwd = 2, bty = "n", text.col = pal$fg)
  graphics::box(bty = "l", col = pal$grid)
}

#' Generate Pure-R Vector SVG Treatment Bar Chart (WebR-Safe for HTML Reports)
#'
#' @param res Result object from \code{\link{analyse_field_trial}}.
#' @param width SVG width in pixels.
#' @param height SVG height in pixels.
#' @return Character scalar containing standalone SVG markup.
#' @export
svg_bar_chart <- function(res, width = 760, height = 380) {
  mt <- res$means_table
  n <- nrow(mt)
  ml <- 72; mr <- 28; mt_pad <- 48; mb <- 86
  pw <- width - ml - mr
  ph <- height - mt_pad - mb

  y_max <- max(mt$Mean + ifelse(is.na(mt$SE), 0, mt$SE), na.rm = TRUE) * 1.18
  if (y_max <= 0) y_max <- 1
  y_min <- min(0, min(mt$Mean, na.rm = TRUE))
  span <- max(1e-6, y_max - y_min)

  y_px <- function(val) mt_pad + ph * (1 - (val - y_min) / span)
  y0 <- y_px(0)

  bar_step <- pw / n
  bar_w <- min(56, bar_step * 0.62)

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  parts <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="100%%" style="max-width:%dpx;background:#ffffff;border-radius:10px;border:1px solid #e2e8f0;font-family:system-ui,-apple-system,sans-serif;">', width, height, width),
    sprintf('<text x="%d" y="24" font-size="15" font-weight="700" fill="#0f172a">%s</text>', ml, esc(paste0("Treatment Means & CLD Groups (", res$mapping$response, ")"))),
    sprintf('<text x="%d" y="40" font-size="11.5" fill="#475569">CV = %.2f%% | SE(m) = %.3f | CD (5%%) = %.3f | Test: %s</text>', ml, res$precision$cv_pct, res$precision$sem, res$precision$cd_05, toupper(res$posthoc))
  )

  ticks <- pretty(c(y_min, y_max), n = 5)
  ticks <- ticks[ticks >= y_min & ticks <= y_max]
  for (tk in ticks) {
    yp <- y_px(tk)
    parts <- c(parts,
               sprintf('<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#e2e8f0" stroke-dasharray="3,3"/>', ml, yp, width - mr, yp),
               sprintf('<text x="%d" y="%.1f" text-anchor="end" font-size="11" fill="#64748b">%.2f</text>', ml - 8, yp + 4, tk))
  }

  for (i in seq_len(n)) {
    xc <- ml + (i - 0.5) * bar_step
    x0 <- xc - bar_w / 2
    yt <- y_px(mt$Mean[i])
    bh <- abs(y0 - yt)
    fill_col <- if (grepl("^a", mt$Group[i])) "#15803d" else "#0284c7"
    parts <- c(parts, sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="4" fill="%s" fill-opacity="0.86"/>', x0, min(yt, y0), bar_w, bh, fill_col))
    if (!is.na(mt$SE[i]) && mt$SE[i] > 0) {
      y_hi <- y_px(mt$Mean[i] + mt$SE[i])
      y_lo <- y_px(mt$Mean[i] - mt$SE[i])
      parts <- c(parts,
                 sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#0f172a" stroke-width="1.6"/>', xc, y_lo, xc, y_hi),
                 sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#0f172a" stroke-width="1.6"/>', xc - 5, y_hi, xc + 5, y_hi),
                 sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#0f172a" stroke-width="1.6"/>', xc - 5, y_lo, xc + 5, y_lo))
      lbl_y <- y_hi - 7
    } else {
      lbl_y <- yt - 7
    }
    parts <- c(parts,
               sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-size="12" font-weight="700" fill="#0f172a">%s</text>', xc, lbl_y, esc(mt$Group[i])),
               sprintf('<text x="%.1f" y="%.1f" text-anchor="end" transform="rotate(-28 %.1f %.1f)" font-size="11" fill="#1e293b">%s</text>', xc + 4, y0 + 16, xc + 4, y0 + 16, esc(mt$Treatment[i])))
  }

  parts <- c(parts, sprintf('<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#64748b" stroke-width="1.2"/>', ml, y0, width - mr, y0), '</svg>')
  paste(parts, collapse = "\n")
}

#' Generate Pure-R Interactive SVG Spatial Field Map
#'
#' @param res Result object from \code{\link{analyse_field_trial}}.
#' @param metric \code{"observed"} or \code{"residual"}.
#' @return Character scalar containing inline SVG markup with hover tooltips.
#' @export
svg_field_heatmap <- function(res, metric = c("observed", "residual")) {
  metric <- match.arg(metric)
  d <- res$data
  row_name <- res$mapping$row
  col_name <- res$mapping$col

  if (!is.null(row_name) && nzchar(row_name) && row_name %in% names(d) &&
      !is.null(col_name) && nzchar(col_name) && col_name %in% names(d)) {
    r_fac <- factor(d[[row_name]], levels = unique(as.character(d[[row_name]])))
    c_fac <- factor(d[[col_name]], levels = unique(as.character(d[[col_name]])))
  } else if (!is.null(res$mapping$block) && nzchar(res$mapping$block) && res$mapping$block %in% names(d)) {
    r_fac <- factor(d[[res$mapping$block]], levels = unique(as.character(d[[res$mapping$block]])))
    c_idx <- stats::ave(seq_len(nrow(d)), r_fac, FUN = seq_along)
    c_fac <- factor(paste0("P", c_idx), levels = paste0("P", seq_len(max(c_idx))))
  } else {
    n <- nrow(d); nc <- ceiling(sqrt(n)); nr <- ceiling(n / nc)
    r_fac <- factor(paste0("R", rep(seq_len(nr), each = nc)[seq_len(n)]))
    c_fac <- factor(paste0("C", rep(seq_len(nc), times = nr)[seq_len(n)]))
  }

  r_lvls <- levels(r_fac); c_lvls <- levels(c_fac)
  nr <- length(r_lvls); nc <- length(c_lvls)
  vals <- if (metric == "observed") d$.y else d$.resid
  v_range <- range(vals, na.rm = TRUE)
  if (metric == "residual") {
    m_abs <- max(abs(v_range), 1e-6)
    v_range <- c(-m_abs, m_abs)
  }
  ramp <- if (metric == "observed") {
    grDevices::colorRampPalette(c("#fef3c7", "#86efac", "#15803d"))(64)
  } else {
    grDevices::colorRampPalette(c("#fca5a5", "#f8fafc", "#93c5fd"))(64)
  }

  cell_w <- max(58, min(92, floor(680 / max(nc, 1))))
  cell_h <- 52
  ml <- 56; mt_pad <- 44; mb <- 32; mr <- 20
  w <- ml + nc * cell_w + mr
  h <- mt_pad + nr * cell_h + mb

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }

  parts <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="100%%" style="max-width:%dpx;background:#ffffff;border-radius:10px;border:1px solid #e2e8f0;font-family:system-ui,-apple-system,sans-serif;">', w, h, max(w, 720)),
    sprintf('<text x="%d" y="25" font-size="14" font-weight="700" fill="#0f172a">%s</text>', ml,
            esc(ifelse(metric == "observed", paste("Field Plot Layout:", res$mapping$response), "Field Spatial Residual Map")))
  )

  for (ci in seq_len(nc)) {
    parts <- c(parts, sprintf('<text x="%.1f" y="%d" text-anchor="middle" font-size="11" fill="#475569">%s</text>',
                              ml + (ci - 0.5) * cell_w, mt_pad + nr * cell_h + 18, esc(c_lvls[ci])))
  }
  for (ri in seq_len(nr)) {
    parts <- c(parts, sprintf('<text x="%d" y="%.1f" text-anchor="end" font-size="11" fill="#475569">%s</text>',
                              ml - 8, mt_pad + (ri - 0.5) * cell_h + 4, esc(r_lvls[ri])))
  }

  for (k in seq_len(nrow(d))) {
    ri <- as.integer(r_fac[k])
    ci <- as.integer(c_fac[k])
    v <- vals[k]
    frac <- if (diff(v_range) > 0) (v - v_range[1]) / diff(v_range) else 0.5
    c_idx <- max(1L, min(64L, round(frac * 63) + 1L))
    x0 <- ml + (ci - 1) * cell_w + 3
    y0 <- mt_pad + (ri - 1) * cell_h + 3
    cw <- cell_w - 6; ch <- cell_h - 6
    tip <- sprintf("Row %s, Col %s | %s | Obs: %.3f | Resid: %.3f",
                   r_lvls[ri], c_lvls[ci], d$.group[k], d$.y[k], d$.resid[k])
    parts <- c(parts,
               sprintf('<g><title>%s</title><rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="6" fill="%s" stroke="#cbd5e1"/>',
                       esc(tip), x0, y0, cw, ch, ramp[c_idx]),
               sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-size="10" font-weight="700" fill="#0f172a">%s</text>',
                       x0 + cw / 2, y0 + ch / 2 - 3, esc(substr(d$.group[k], 1, 11))),
               sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-size="10.5" fill="#1e293b">%.2f</text></g>',
                       x0 + cw / 2, y0 + ch / 2 + 12, v))
  }
  parts <- c(parts, '</svg>')
  paste(parts, collapse = "\n")
}
