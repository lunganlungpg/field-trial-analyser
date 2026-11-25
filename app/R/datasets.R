# ==============================================================================
# datasets.R
# Built-in agronomic field trial datasets for teaching, testing, and demos.
# Works seamlessly inside both the `fieldtrial` R package and Shinylive app.
# ==============================================================================

#' List Built-In Agronomic Field Trial Datasets
#'
#' Returns metadata describing five curated agricultural and biological
#' experimental datasets covering CRD, RCBD, Latin Square, Factorial RCBD,
#' and Split-Plot designs.
#'
#' @return A \code{data.frame} with columns \code{id}, \code{title},
#'   \code{design}, \code{crop}, \code{n_plots}, \code{file},
#'   \code{default_response}, \code{default_mapping}, and \code{teaching_note}.
#' @examples
#' list_sample_trials()[, c("id", "title", "design", "n_plots")]
#' @export
list_sample_trials <- function() {
  data.frame(
    id = c(
      "wheat_rcbd",
      "rice_split_plot",
      "mustard_factorial",
      "chickpea_crd",
      "sugarcane_lsd"
    ),
    title = c(
      "Wheat Nitrogen Dose Trial (RCBD, 6 x 4)",
      "Rice Irrigation x Cultivar Trial (Split-Plot, 3 x 4 x 3)",
      "Indian Mustard Variety x Sulphur (Factorial RCBD, 3 x 4 x 3)",
      "Chickpea Bio-Fertilizer Pot Trial (CRD, 5 x 5)",
      "Sugarcane Spacing & Intercrop Trial (5 x 5 Latin Square)"
    ),
    design = c("rcbd", "split_plot", "factorial_rcbd", "crd", "lsd"),
    crop = c("Wheat", "Rice", "Indian Mustard", "Chickpea", "Sugarcane"),
    n_plots = c(24L, 36L, 36L, 25L, 25L),
    file = c(
      "wheat_rcbd_gomez.csv",
      "rice_split_plot.csv",
      "mustard_factorial_rcbd.csv",
      "chickpea_biofertilizer_crd.csv",
      "sugarcane_latin_square.csv"
    ),
    default_response = c(
      "grain_yield_tha",
      "grain_yield_tha",
      "seed_yield_kgha",
      "grain_yield_g_pot",
      "cane_yield_tha"
    ),
    teaching_note = c(
      "Demonstrates fertility-gradient blocking and RCBD relative efficiency vs CRD.",
      "Demonstrates dual Error(a)/Error(b) partitioning and Arcsine transformation on disease_severity_pct.",
      "Demonstrates main effects, A x B interaction profile plots, and oil yield response.",
      "Demonstrates completely randomized pot trials and Sqrt(y + 0.5) count transformation on nodules_plant.",
      "Demonstrates simultaneous two-way row & column blocking in a 5 x 5 orthogonal Latin Square."
    ),
    stringsAsFactors = FALSE
  )
}

#' Load a Built-In Agronomic Trial Dataset
#'
#' Resolves sample CSV datasets across installed package paths (\code{inst/extdata}),
#' local repository roots, and Shinylive app bundles (\code{data/}).
#'
#' @param id Character dataset identifier from \code{\link{list_sample_trials}}.
#' @param data_dir Optional explicit directory containing the CSV files.
#' @return A \code{data.frame} containing the selected trial dataset.
#' @examples
#' df <- load_sample_trial("wheat_rcbd")
#' head(df)
#' @export
load_sample_trial <- function(id = "wheat_rcbd", data_dir = NULL) {
  catalog <- list_sample_trials()
  idx <- match(id, catalog$id)
  if (is.na(idx)) {
    stop("Unknown sample trial id: '", id, "'. Valid ids: ",
         paste(catalog$id, collapse = ", "), call. = FALSE)
  }
  fname <- catalog$file[idx]

  candidates <- character(0)
  if (!is.null(data_dir) && nzchar(data_dir)) {
    candidates <- c(candidates, file.path(data_dir, fname))
  }
  pkg_name <- "fieldtrial"
  candidates <- c(
    candidates,
    file.path("data", fname),
    file.path("inst", "extdata", fname),
    file.path("app", "data", fname),
    system.file("extdata", fname, package = pkg_name)
  )
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(candidates)][1]

  if (is.na(hit) || !nzchar(hit)) {
    stop("Could not locate sample dataset '", fname, "' in local or package paths.",
         call. = FALSE)
  }
  utils::read.csv(hit, stringsAsFactors = FALSE, check.names = FALSE)
}
