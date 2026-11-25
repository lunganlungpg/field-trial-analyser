# ==============================================================================
# generate_readme_plots.R
# Generates vector SVG showcase figures in plots/ directly from the pure-R
# fieldtrial statistical engine.
# ==============================================================================

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}

dir.create("plots", showWarnings = FALSE, recursive = TRUE)

# 1. Wheat Nitrogen RCBD treatment comparison with CLD letters (pure-R vector SVG)
wheat <- load_sample_trial("wheat_rcbd")
res_wheat <- analyse_field_trial(wheat, "rcbd", detect_trial_columns(wheat))
writeLines(svg_bar_chart(res_wheat, width = 820, height = 390), "plots/wheat_rcbd_means.svg", useBytes = TRUE)

# 2. Rice Split-Plot spatial residual heatmap (pure-R vector SVG)
rice <- load_sample_trial("rice_split_plot")
r_map <- detect_trial_columns(rice)
r_map$main_plot <- "irrigation"
r_map$sub_plot <- "cultivar"
r_map$response <- "grain_yield_tha"
res_rice <- analyse_field_trial(rice, "split_plot", r_map)
writeLines(svg_field_heatmap(res_rice, metric = "observed"), "plots/rice_split_plot_heatmap.svg", useBytes = TRUE)

# 3. Rice Split-Plot interaction profile (grDevices::svg)
grDevices::svg("plots/split_plot_interaction.svg", width = 8.2, height = 4.5)
plot_interaction_means(res_rice, dark = FALSE)
grDevices::dev.off()

# 4. Replicate power planner curve (grDevices::svg)
plan <- plan_replications(cv_pct = 12, min_diff_pct = 15, n_treatments = 6, design = "rcbd", target_power = 0.80)
grDevices::svg("plots/replicate_power_curve.svg", width = 8.2, height = 4.4)
plot_power_curve(plan, dark = FALSE)
grDevices::dev.off()

cat("Generated 4 SVG figures in plots/.\n")
