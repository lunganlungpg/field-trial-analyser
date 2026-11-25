\name{fieldtrial-package}
\alias{fieldtrial-package}
\alias{fieldtrial}
\alias{list_sample_trials}
\alias{load_sample_trial}
\alias{parse_trial_text}
\alias{detect_trial_columns}
\alias{validate_trial_design}
\alias{apply_transformation}
\alias{back_transform_mean}
\alias{compute_pairwise_cld}
\alias{analyse_field_trial}
\alias{recommend_transformations}
\alias{generate_interpretation_text}
\alias{plan_replications}
\alias{generate_reproducible_r_code}
\alias{generate_html_report}
\alias{plot_treatment_means}
\alias{plot_diagnostics_grid}
\alias{plot_interaction_means}
\alias{plot_field_heatmap}
\alias{plot_power_curve}
\alias{svg_bar_chart}
\alias{svg_field_heatmap}
\docType{package}
\title{Field Trial Experimental Design, ANOVA, and Diagnostic Suite}
\description{
Pure base-R statistical engine and Shinylive (webR) companion application for
agricultural and biological field trial analysis, supporting CRD, RCBD, Latin
Square, Two-Way Factorial RCBD, and Split-Plot designs with Piepho compact
letter display (CLD), data transformations, residual diagnostics, spatial
residual heatmaps, replicate power planning, and reproducible R script export.
}
\usage{
list_sample_trials()
load_sample_trial(id = "wheat_rcbd", data_dir = NULL)
parse_trial_text(text)
detect_trial_columns(df)
validate_trial_design(df, design = c("rcbd", "crd", "lsd", "factorial_rcbd", "split_plot"), mapping)
apply_transformation(y, transform = c("none", "log10", "log1p", "sqrt", "arcsine_pct"))
back_transform_mean(val, transform = "none", scale_param = 100)
compute_pairwise_cld(means, n_rep, error_ms, error_df, alpha = 0.05,
  method = c("tukey", "lsd", "bonferroni"), f_pvalue = NULL, protected = TRUE)
analyse_field_trial(df, design = c("rcbd", "crd", "lsd", "factorial_rcbd", "split_plot"),
  mapping, transform = c("none", "log10", "log1p", "sqrt", "arcsine_pct"),
  alpha = 0.05, posthoc = c("tukey", "lsd", "bonferroni"), protected = TRUE)
recommend_transformations(df, design, mapping, alpha = 0.05)
generate_interpretation_text(res)
plan_replications(cv_pct = 12, min_diff_pct = 15, n_treatments = 6L,
  design = c("rcbd", "crd", "lsd"), alpha = 0.05, target_power = 0.80)
generate_reproducible_r_code(res, source_label = "my_field_trial.csv")
generate_html_report(res)
plot_treatment_means(res, error_bar = c("se", "ci95", "none"), show_points = TRUE, dark = FALSE)
plot_diagnostics_grid(res, dark = FALSE)
plot_interaction_means(res, dark = FALSE)
plot_field_heatmap(res, metric = c("observed", "residual"), dark = FALSE)
plot_power_curve(plan_obj, dark = FALSE)
svg_bar_chart(res, width = 760, height = 380)
svg_field_heatmap(res, metric = c("observed", "residual"))
}
\arguments{
  \item{id}{Character sample dataset identifier.}
  \item{data_dir}{Optional directory path to sample CSV files.}
  \item{text}{Tabular CSV or tab-separated text.}
  \item{df}{A \code{data.frame} containing trial records.}
  \item{design}{Experimental design identifier.}
  \item{mapping}{Named list mapping column roles.}
  \item{y}{Numeric vector of observations.}
  \item{transform}{Transformation type.}
  \item{val}{Numeric transformed mean value.}
  \item{scale_param}{Shift or percentage scale factor.}
  \item{means}{Named numeric vector of treatment means.}
  \item{n_rep}{Replicate counts per treatment.}
  \item{error_ms}{Residual mean square.}
  \item{error_df}{Residual degrees of freedom.}
  \item{alpha}{Significance level.}
  \item{method}{Pairwise comparison method.}
  \item{f_pvalue}{Omnibus F-test p-value.}
  \item{protected}{Enforce Fisher Protected LSD.}
  \item{posthoc}{Pairwise test name.}
  \item{res}{Object returned by \code{analyse_field_trial}.}
  \item{cv_pct}{Expected CV percentage.}
  \item{min_diff_pct}{Minimum meaningful difference percentage.}
  \item{n_treatments}{Number of treatments.}
  \item{target_power}{Desired statistical power.}
  \item{source_label}{Source CSV filename for script generation.}
  \item{error_bar}{Error bar mode.}
  \item{show_points}{Overlay raw observations.}
  \item{dark}{Dark theme flag.}
  \item{metric}{Spatial heatmap metric.}
  \item{plan_obj}{Object returned by \code{plan_replications}.}
  \item{width}{SVG width in pixels.}
  \item{height}{SVG height in pixels.}
}
\value{See individual function source files for detailed return structures.}
\author{Lunganlung P. G. <lunganlung@icar.gov.in>}
