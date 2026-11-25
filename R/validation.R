# ==============================================================================
# validation.R
# Smart delimiter parsing, column auto-detection, pre-flight design validation,
# and standard agricultural data transformations.
# ==============================================================================

#' Parse CSV or Pasted Spreadsheet Text into a Data Frame
#'
#' Supports comma-separated, semicolon-separated, and tab-separated text pasted
#' directly from Excel, LibreOffice Calc, or Google Sheets.
#'
#' @param text Character scalar containing tabular text with a header row.
#' @return A cleaned \code{data.frame}.
#' @examples
#' txt <- "block\ttreatment\tyield\nB1\tT1\t4.2\nB1\tT2\t5.1"
#' parse_trial_text(txt)
#' @export
parse_trial_text <- function(text) {
  if (!is.character(text) || length(text) == 0L || !nzchar(trimws(text[1]))) {
    stop("Input text is empty. Paste rows with column headers from Excel or CSV.", call. = FALSE)
  }
  raw_lines <- unlist(strsplit(text[1], "\r?\n"))
  raw_lines <- raw_lines[nzchar(trimws(raw_lines))]
  if (length(raw_lines) < 2L) {
    stop("Need at least one header row and one data row.", call. = FALSE)
  }

  header <- raw_lines[1]
  tab_n <- lengths(regmatches(header, gregexpr("\t", header, fixed = TRUE)))
  semi_n <- lengths(regmatches(header, gregexpr(";", header, fixed = TRUE)))
  comma_n <- lengths(regmatches(header, gregexpr(",", header, fixed = TRUE)))

  sep <- if (tab_n >= comma_n && tab_n >= semi_n && tab_n > 0L) {
    "\t"
  } else if (semi_n > comma_n && semi_n > 0L) {
    ";"
  } else {
    ","
  }

  con <- textConnection(paste(raw_lines, collapse = "\n"))
  on.exit(close(con), add = TRUE)
  df <- utils::read.delim(
    con,
    sep = sep,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    strip.white = TRUE,
    na.strings = c("", "NA", "NaN", "NULL", ".", "-")
  )
  names(df) <- trimws(names(df))
  df
}

#' Auto-Detect Likely Field Trial Roles from Column Names
#'
#' Scans column names and data types to suggest default selections for response
#' trait, treatment, block/replicate, factorial factors, split-plot hierarchy,
#' and spatial row/column coordinates.
#'
#' @param df A \code{data.frame} containing trial records.
#' @return A named list of suggested column names (empty string if absent).
#' @examples
#' df <- load_sample_trial("wheat_rcbd")
#' detect_trial_columns(df)
#' @export
detect_trial_columns <- function(df) {
  cols <- names(df)
  low <- tolower(cols)
  is_num <- vapply(df, is.numeric, logical(1))
  num_cols <- cols[is_num]
  cat_cols <- cols[!is_num | vapply(df, function(x) length(unique(x[!is.na(x)])) <= 12L, logical(1))]

  pick_match <- function(patterns, pool) {
    pool_low <- tolower(pool)
    for (pat in patterns) {
      hit <- grep(pat, pool_low)
      if (length(hit) > 0L) return(pool[hit[1]])
    }
    ""
  }

  # Avoid picking plot_id / row / col / rep as the primary numeric yield response
  id_like <- grepl("^(plot|pot|id|rep|replicate|block|blk|row|col|column)$", low)
  resp_pool <- cols[is_num & !id_like]
  if (length(resp_pool) == 0L) resp_pool <- num_cols

  response <- pick_match(c("yield", "grain", "seed", "cane", "biomass", "height", "weight", "brix", "nodule", "severity", "response"), resp_pool)
  if (!nzchar(response) && length(resp_pool) > 0L) response <- resp_pool[1]

  treatment <- pick_match(c("^treat", "^trt", "genotyp", "cultivar", "variety", "fertil", "dose", "inocul"), cat_cols)
  block <- pick_match(c("^block", "^blk", "^rep", "replicate"), cat_cols)
  row_col <- pick_match(c("^row", "range"), cols)
  col_col <- pick_match(c("^col", "column", "pass"), cols)
  main_plot <- pick_match(c("irrig", "main", "tillage", "water", "factor_?a", "variety", "cultivar"), cat_cols)
  sub_plot <- pick_match(c("cultivar", "variety", "sub", "nitrogen", "sulphur", "factor_?b", "treat"), cat_cols)

  factor_candidates <- setdiff(cat_cols, c(block, row_col, col_col, pick_match(c("^plot", "^pot", "^id$"), cat_cols)))
  factor_a <- if (nzchar(main_plot)) main_plot else if (length(factor_candidates) >= 1L) factor_candidates[1] else ""
  rem_factors <- setdiff(factor_candidates, factor_a)
  factor_b <- if (nzchar(sub_plot) && sub_plot != factor_a) sub_plot else if (length(rem_factors) >= 1L) rem_factors[1] else ""

  if (!nzchar(treatment) && length(factor_candidates) >= 1L) treatment <- factor_candidates[1]

  list(
    response = response,
    treatment = treatment,
    block = block,
    factor_a = factor_a,
    factor_b = factor_b,
    main_plot = factor_a,
    sub_plot = factor_b,
    row = row_col,
    col = col_col
  )
}

#' Apply Standard Agricultural Data Transformation
#'
#' Transforms response values prior to ANOVA to address skewed counts
#' (\code{sqrt}, \code{log10}, \code{log1p}) or bounded percentage data
#' (\code{arcsine_pct}).
#'
#' @param y Numeric response vector.
#' @param transform One of \code{"none"}, \code{"log10"}, \code{"log1p"},
#'   \code{"sqrt"}, or \code{"arcsine_pct"}.
#' @return A list with \code{values}, \code{transform}, \code{label},
#'   \code{pct_scale} (for arcsine), and \code{warnings}.
#' @examples
#' apply_transformation(c(4, 12, 25), "sqrt")
#' @export
apply_transformation <- function(y, transform = c("none", "log10", "log1p", "sqrt", "arcsine_pct")) {
  transform <- match.arg(transform)
  y <- suppressWarnings(as.numeric(y))
  warns <- character(0)
  pct_scale <- 100

  if (transform == "none") {
    return(list(values = y, transform = "none", label = "Original scale", pct_scale = 100, warnings = warns))
  }

  valid <- y[!is.na(y)]
  if (transform == "log10") {
    if (any(valid <= 0)) {
      shift <- abs(min(valid)) + 1
      warns <- c(warns, paste0("Non-positive values detected; shifted by +", round(shift, 3), " before log10."))
      vals <- log10(y + shift)
      return(list(values = vals, transform = "log10", label = paste0("log10(y + ", round(shift, 3), ")"), pct_scale = shift, warnings = warns))
    }
    return(list(values = log10(y), transform = "log10", label = "log10(y)", pct_scale = 0, warnings = warns))
  }

  if (transform == "log1p") {
    if (any(valid <= -1)) {
      stop("Natural log(y + 1) requires values > -1.", call. = FALSE)
    }
    return(list(values = log(y + 1), transform = "log1p", label = "ln(y + 1)", pct_scale = 1, warnings = warns))
  }

  if (transform == "sqrt") {
    if (any(valid < -0.5)) {
      stop("Square-root(y + 0.5) requires values >= -0.5.", call. = FALSE)
    }
    return(list(values = sqrt(y + 0.5), transform = "sqrt", label = "sqrt(y + 0.5)", pct_scale = 0.5, warnings = warns))
  }

  # Arcsine / Angular transformation in degrees (standard in plant pathology & germination)
  if (length(valid) > 0L && max(valid) <= 1.0 && min(valid) >= 0.0) {
    pct_scale <- 1
    p <- pmin(pmax(y, 0), 1)
  } else {
    pct_scale <- 100
    if (any(valid < 0 | valid > 100)) {
      warns <- c(warns, "Values outside [0, 100] were clamped before arcsine transformation.")
    }
    p <- pmin(pmax(y / 100, 0), 1)
  }
  vals <- asin(sqrt(p)) * (180 / pi)
  list(values = vals, transform = "arcsine_pct", label = "Arcsine degrees", pct_scale = pct_scale, warnings = warns)
}

#' Back-Transform Mean Estimate to Original Measurement Scale
#'
#' @param val Numeric scalar or vector on transformed scale.
#' @param transform Transformation name used in \code{\link{apply_transformation}}.
#' @param scale_param Optional shift/scale returned as \code{pct_scale}.
#' @return Back-transformed numeric vector.
#' @examples
#' back_transform_mean(apply_transformation(25, "sqrt")$values, "sqrt")
#' @export
back_transform_mean <- function(val, transform = "none", scale_param = 100) {
  if (transform == "none") return(val)
  if (transform == "log10") return(10^val - scale_param)
  if (transform == "log1p") return(exp(val) - 1)
  if (transform == "sqrt") return(pmax(0, val^2 - 0.5))
  if (transform == "arcsine_pct") {
    rad <- pmin(pmax(val, 0), 90) * (pi / 180)
    return((sin(rad)^2) * scale_param)
  }
  val
}

#' Pre-Flight Data Health & Design Structure Check
#'
#' Validates column mappings, missing values, replication counts, balance,
#' Latin Square orthogonality, and Split-Plot nesting before ANOVA runs.
#'
#' @param df Input \code{data.frame}.
#' @param design One of \code{"crd"}, \code{"rcbd"}, \code{"lsd"},
#'   \code{"factorial_rcbd"}, or \code{"split_plot"}.
#' @param mapping Named list with column mappings (\code{response}, \code{treatment},
#'   \code{block}, \code{factor_a}, \code{factor_b}, \code{main_plot}, \code{sub_plot},
#'   \code{row}, \code{col}).
#' @return Structured list containing \code{ok}, \code{errors}, \code{warnings},
#'   \code{info}, \code{is_balanced}, \code{n_obs}, and \code{n_missing}.
#' @examples
#' df <- load_sample_trial("wheat_rcbd")
#' validate_trial_design(df, "rcbd", detect_trial_columns(df))
#' @export
validate_trial_design <- function(df, design = c("rcbd", "crd", "lsd", "factorial_rcbd", "split_plot"), mapping) {
  design <- match.arg(design)
  errors <- character(0)
  warnings <- character(0)
  info <- character(0)

  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(list(ok = FALSE, errors = "Dataset has no rows.", warnings = warnings,
                info = info, is_balanced = FALSE, n_obs = 0L, n_missing = 0L))
  }

  req_cols <- switch(
    design,
    crd = c("response", "treatment"),
    rcbd = c("response", "treatment", "block"),
    lsd = c("response", "treatment", "row", "col"),
    factorial_rcbd = c("response", "factor_a", "factor_b", "block"),
    split_plot = c("response", "main_plot", "sub_plot", "block")
  )

  for (rc in req_cols) {
    cname <- mapping[[rc]]
    if (is.null(cname) || !nzchar(cname) || !(cname %in% names(df))) {
      errors <- c(errors, paste0("Missing or invalid column mapping for '", rc, "'."))
    }
  }

  if (length(errors) > 0L) {
    return(list(ok = FALSE, errors = errors, warnings = warnings, info = info,
                is_balanced = FALSE, n_obs = nrow(df), n_missing = 0L))
  }

  # Check distinct factor columns for multi-factor designs
  if (design == "factorial_rcbd" && length(unique(c(mapping$factor_a, mapping$factor_b, mapping$block))) < 3L) {
    errors <- c(errors, "Factor A, Factor B, and Block must be mapped to three distinct columns.")
  }
  if (design == "split_plot" && length(unique(c(mapping$main_plot, mapping$sub_plot, mapping$block))) < 3L) {
    errors <- c(errors, "Main-Plot, Sub-Plot, and Block must be mapped to three distinct columns.")
  }
  if (design == "lsd" && length(unique(c(mapping$treatment, mapping$row, mapping$col))) < 3L) {
    errors <- c(errors, "Treatment, Row, and Column must be mapped to three distinct columns.")
  }
  if (design == "rcbd" && mapping$treatment == mapping$block) {
    errors <- c(errors, "Treatment and Block cannot be mapped to the same column.")
  }
  if (length(errors) > 0L) {
    return(list(ok = FALSE, errors = errors, warnings = warnings, info = info,
                is_balanced = FALSE, n_obs = nrow(df), n_missing = 0L))
  }

  y <- suppressWarnings(as.numeric(df[[mapping$response]]))
  n_missing <- sum(is.na(y))
  if (all(is.na(y))) {
    errors <- c(errors, paste0("Response column '", mapping$response, "' contains no numeric values."))
    return(list(ok = FALSE, errors = errors, warnings = warnings, info = info,
                is_balanced = FALSE, n_obs = 0L, n_missing = n_missing))
  }

  complete_mask <- !is.na(y)
  for (rc in setdiff(req_cols, "response")) {
    v <- as.character(df[[mapping[[rc]]]])
    complete_mask <- complete_mask & !is.na(v) & nzchar(trimws(v))
  }
  n_missing <- sum(!complete_mask)
  if (n_missing > 0L) {
    warnings <- c(warnings, paste0(n_missing, " row(s) with missing values will be excluded from ANOVA."))
  }

  sub <- df[complete_mask, , drop = FALSE]
  y_clean <- y[complete_mask]
  if (length(y_clean) < 4L) {
    errors <- c(errors, "At least 4 complete plot observations are required.")
  } else if (stats::sd(y_clean) == 0) {
    errors <- c(errors, "Response trait has zero variance (all values are identical).")
  }

  is_balanced <- TRUE
  if (length(errors) == 0L) {
    if (design == "crd") {
      tab <- table(sub[[mapping$treatment]])
      if (length(tab) < 2L) errors <- c(errors, "At least 2 treatment levels are required.")
      if (any(tab < 2L)) warnings <- c(warnings, "Some treatments have only 1 replicate; residual DF may be low.")
      is_balanced <- length(unique(as.integer(tab))) == 1L
      if (!is_balanced) warnings <- c(warnings, "Unbalanced replicate counts across treatments detected; harmonic-mean SE will be used.")
      info <- c(info, paste0(length(tab), " treatments across ", nrow(sub), " experimental units."))
    } else if (design == "rcbd") {
      tab <- table(sub[[mapping$block]], sub[[mapping$treatment]])
      if (nrow(tab) < 2L) errors <- c(errors, "At least 2 blocks/replicates are required for RCBD.")
      if (ncol(tab) < 2L) errors <- c(errors, "At least 2 treatments are required for RCBD.")
      is_balanced <- all(tab == tab[1, 1]) && tab[1, 1] >= 1L
      if (!is_balanced) warnings <- c(warnings, "Design is unbalanced across Block x Treatment cells (missing or extra plots detected).")
      info <- c(info, paste0(ncol(tab), " treatments x ", nrow(tab), " blocks (", nrow(sub), " plots)."))
    } else if (design == "lsd") {
      r_n <- length(unique(sub[[mapping$row]]))
      c_n <- length(unique(sub[[mapping$col]]))
      t_n <- length(unique(sub[[mapping$treatment]]))
      tab_rt <- table(sub[[mapping$row]], sub[[mapping$treatment]])
      tab_ct <- table(sub[[mapping$col]], sub[[mapping$treatment]])
      if (r_n < 3L || c_n < 3L || t_n < 3L) {
        errors <- c(errors, "Latin Square requires at least 3 rows, 3 columns, and 3 treatments so error DF > 0.")
      }
      if (r_n != c_n || r_n != t_n) {
        warnings <- c(warnings, paste0("Non-square layout (", r_n, " rows x ", c_n, " cols x ", t_n, " treatments); verify Row-Column structure."))
        is_balanced <- FALSE
      } else if (!all(tab_rt == 1L) || !all(tab_ct == 1L)) {
        warnings <- c(warnings, "Not a complete orthogonal Latin Square (each treatment should occur once per row and once per column).")
        is_balanced <- FALSE
      }
      info <- c(info, paste0(r_n, " x ", c_n, " Row-Column layout with ", t_n, " treatments."))
    } else if (design == "factorial_rcbd") {
      tab <- table(sub[[mapping$factor_a]], sub[[mapping$factor_b]], sub[[mapping$block]])
      if (dim(tab)[1] < 2L || dim(tab)[2] < 2L || dim(tab)[3] < 2L) {
        errors <- c(errors, "Factorial RCBD requires >= 2 levels of Factor A, >= 2 levels of Factor B, and >= 2 blocks.")
      }
      is_balanced <- all(tab == tab[1, 1, 1]) && tab[1, 1, 1] >= 1L
      if (!is_balanced) warnings <- c(warnings, "Factorial RCBD cells are unbalanced across blocks.")
      info <- c(info, paste0(dim(tab)[1], " x ", dim(tab)[2], " factorial in ", dim(tab)[3], " blocks (", nrow(sub), " plots)."))
    } else if (design == "split_plot") {
      tab <- table(sub[[mapping$main_plot]], sub[[mapping$sub_plot]], sub[[mapping$block]])
      if (dim(tab)[1] < 2L || dim(tab)[2] < 2L || dim(tab)[3] < 2L) {
        errors <- c(errors, "Split-Plot requires >= 2 Main-Plot levels, >= 2 Sub-Plot levels, and >= 2 blocks.")
      }
      is_balanced <- all(tab == tab[1, 1, 1]) && tab[1, 1, 1] >= 1L
      if (!is_balanced) warnings <- c(warnings, "Split-Plot layout is unbalanced; check missing subplots.")
      info <- c(info, paste0(dim(tab)[1], " main plots x ", dim(tab)[2], " subplots x ", dim(tab)[3], " blocks."))
    }
  }

  list(
    ok = length(errors) == 0L,
    errors = errors,
    warnings = warnings,
    info = info,
    is_balanced = is_balanced,
    n_obs = nrow(sub),
    n_missing = n_missing
  )
}
