# ==============================================================================
# Serverless Field Trial Analyser (Shinylive / webR)
# Runs 100% client-side in the user's browser — zero server cost, full privacy.
# ==============================================================================

library(shiny)
library(bslib)

# Ensure pure-R statistical engine is loaded cleanly in both local Shiny and Shinylive
if (!exists("analyse_field_trial", mode = "function")) {
  r_files <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE))
  for (rf in r_files) {
    source(rf, local = FALSE)
  }
}

.nz_or <- function(x, fallback = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) fallback else x[1]
}

# Crisp inline SVG icons (zero external icon package dependency for fast webR boot)
.svg_icon <- function(kind = "chart") {
  path_str <- switch(
    kind,
    leaf = '<path stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" fill="none" d="M11 20A7 7 0 0 1 9.8 6.1C15.5 5 17 4.48 19 2c1 2 2 4.18 2 8 0 5.5-4.78 10-10 10Z"/><path stroke="currentColor" stroke-width="1.8" stroke-linecap="round" d="M2 21c0-3 1.85-5.36 5.08-6C9.5 14.52 12 13 13 12"/>',
    chart = '<path stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" fill="none" d="M3 3v18h18M8 16v-5M13 16V7M18 16v-8"/>',
    target = '<circle cx="12" cy="12" r="9" stroke="currentColor" stroke-width="1.8" fill="none"/><circle cx="12" cy="12" r="5" stroke="currentColor" stroke-width="1.8" fill="none"/><circle cx="12" cy="12" r="1.5" fill="currentColor"/>',
    ruler = '<path stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" fill="none" d="M21 6H3v12h18V6ZM7 6v4M11 6v3M15 6v4M19 6v3"/>',
    shield = '<path stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" fill="none" d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10Z"/><path stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" fill="none" d="m9 12 2 2 4-4"/>',
    '<circle cx="12" cy="12" r="8" stroke="currentColor" stroke-width="1.8" fill="none"/>'
  )
  HTML(sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="28" height="28" viewBox="0 0 24 24">%s</svg>', path_str))
}

.design_descriptions <- list(
  rcbd = "Randomized Complete Block Design: Each block contains every treatment once. Ideal for field slopes or fertility gradients.",
  crd = "Completely Randomized Design: Treatments assigned randomly without blocking. Best for uniform greenhouses, pots, or growth chambers.",
  lsd = "Latin Square Design: Controls two perpendicular spatial gradients (Rows and Columns) simultaneously; requires t x t square layout.",
  factorial_rcbd = "Two-Way Factorial RCBD: Evaluates Factor A, Factor B, and A x B interaction inside randomized complete blocks.",
  split_plot = "Split-Plot Design: Main-plot factor assigned to larger strips within blocks; sub-plot factor randomized inside each main plot (Error a & Error b)."
)

.sample_catalog <- list_sample_trials()

.copy_js <- HTML("
  function copyTextBlock(elementId, btnId) {
    var el = document.getElementById(elementId);
    var btn = document.getElementById(btnId);
    if (!el || !navigator.clipboard) return;
    navigator.clipboard.writeText(el.innerText || el.textContent).then(function() {
      if (btn) {
        var orig = btn.innerText;
        btn.innerText = 'Copied!';
        setTimeout(function() { btn.innerText = orig; }, 1600);
      }
    });
  }
")

ui <- page_navbar(
  id = "main_nav",
  title = span(
    .svg_icon("leaf"),
    "Field Trial Analyser",
    span("Shinylive · webR", class = "ft-badge-serverless ms-1")
  ),
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    primary = "#15803d",
    secondary = "#0284c7",
    success = "#16a34a",
    info = "#0284c7",
    warning = "#d97706",
    danger = "#dc2626",
    navbar_bg = "#14532d"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(.copy_js)
  ),

  # ---------------------------------------------------------------------------
  # TAB 1: Guided Data & Experimental Design Setup
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "1. Data & Design",
    value = "setup",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        div(
          span("Step 1 · Load Trial Data", class = "ft-step-tag"),
          radioButtons(
            "data_source",
            label = NULL,
            choices = c(
              "Demo agronomic datasets (5 crops)" = "demo",
              "Upload CSV / TSV file" = "upload",
              "Paste from Excel / Sheets" = "paste"
            ),
            selected = "demo"
          ),
          conditionalPanel(
            condition = "input.data_source == 'demo'",
            selectInput(
              "demo_id",
              "Choose a trial dataset",
              choices = stats::setNames(.sample_catalog$id, .sample_catalog$title),
              selected = "wheat_rcbd"
            ),
            uiOutput("demo_note_ui")
          ),
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput("upload_file", "Select CSV or TSV file", accept = c(".csv", ".tsv", ".txt")),
            downloadButton("dl_template", "Download RCBD CSV Template", class = "btn-outline-secondary btn-sm w-100 mb-2")
          ),
          conditionalPanel(
            condition = "input.data_source == 'paste'",
            textAreaInput(
              "paste_text",
              "Paste table (with header row)",
              rows = 6,
              value = paste(
                c(
                  "block\ttreatment\tyield",
                  "B1\tControl\t3.10",
                  "B1\tBiochar\t3.85",
                  "B1\tRDF\t4.42",
                  "B2\tControl\t3.25",
                  "B2\tBiochar\t4.02",
                  "B2\tRDF\t4.61",
                  "B3\tControl\t2.98",
                  "B3\tBiochar\t3.74",
                  "B3\tRDF\t4.33"
                ),
                collapse = "\n"
              )
            ),
            actionButton("apply_paste", "Load Pasted Table", class = "btn-primary btn-sm w-100 mb-2")
          )
        ),
        hr(),
        div(
          span("Step 2 · Experimental Design", class = "ft-step-tag"),
          selectInput(
            "design",
            "Design structure",
            choices = c(
              "RCBD (Blocked Field Trial)" = "rcbd",
              "CRD (Pot / Lab / Greenhouse)" = "crd",
              "Latin Square (2-Way Blocking)" = "lsd",
              "Two-Way Factorial RCBD (A x B)" = "factorial_rcbd",
              "Split-Plot (Main x Sub Plot)" = "split_plot"
            ),
            selected = "rcbd"
          ),
          uiOutput("design_help_ui")
        ),
        hr(),
        div(
          span("Step 3 · Map Columns & Run", class = "ft-step-tag"),
          selectInput("col_response", "Response trait (numeric)", choices = NULL),
          conditionalPanel(
            condition = "input.design == 'crd' || input.design == 'rcbd' || input.design == 'lsd'",
            selectInput("col_treatment", "Treatment / Variety", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.design == 'rcbd' || input.design == 'factorial_rcbd' || input.design == 'split_plot'",
            selectInput("col_block", "Block / Replicate", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.design == 'factorial_rcbd'",
            selectInput("col_factor_a", "Factor A", choices = NULL),
            selectInput("col_factor_b", "Factor B", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.design == 'split_plot'",
            selectInput("col_main_plot", "Main-Plot Factor (Error a)", choices = NULL),
            selectInput("col_sub_plot", "Sub-Plot Factor (Error b)", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.design == 'lsd'",
            selectInput("col_row", "Row blocking factor", choices = NULL),
            selectInput("col_col", "Column blocking factor", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.design != 'lsd'",
            layout_columns(
              col_widths = c(6, 6),
              selectInput("col_map_row", "Spatial Row (opt.)", choices = NULL),
              selectInput("col_map_col", "Spatial Col (opt.)", choices = NULL)
            )
          ),
          selectInput(
            "transform",
            "Data transformation",
            choices = c(
              "None (Original Scale)" = "none",
              "Log10(y)" = "log10",
              "Natural ln(y + 1)" = "log1p",
              "Square Root: sqrt(y + 0.5)" = "sqrt",
              "Arcsine / Angular (%)" = "arcsine_pct"
            ),
            selected = "none"
          ),
          layout_columns(
            col_widths = c(6, 6),
            selectInput("posthoc", "Mean separation", choices = c("Tukey HSD" = "tukey", "Protected LSD / CD" = "lsd", "Bonferroni" = "bonferroni"), selected = "tukey"),
            selectInput("alpha", "Alpha level", choices = c("0.05 (95%)" = "0.05", "0.01 (99%)" = "0.01"), selected = "0.05")
          ),
          actionButton("go_results", "View ANOVA & Results ->", class = "btn-success w-100 mt-2 fw-semibold")
        )
      ),
      uiOutput("health_banner_ui"),
      uiOutput("setup_kpis_ui"),
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Field Book Preview"),
            uiOutput("dataset_dims_badge")
          )
        ),
        card_body(
          div(class = "table-responsive-ft", tableOutput("preview_table"))
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # TAB 2: Results, ANOVA, Means & Plain-English Interpretation
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "2. Results & Means",
    value = "results",
    uiOutput("results_kpis_ui"),
    card(
      class = "mb-3",
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          span("Plain-English Results Summary (Ready for Thesis / Manuscript)"),
          tags$button(
            id = "copy_summary_btn",
            type = "button",
            class = "btn btn-sm btn-outline-primary",
            onclick = "copyTextBlock('interp_text_box', 'copy_summary_btn')",
            "Copy Summary"
          )
        )
      ),
      card_body(
        div(id = "interp_text_box", class = "ft-summary-box", uiOutput("interpretation_ui"))
      )
    ),
    navset_card_underline(
      id = "results_tabs",
      nav_panel(
        "Treatment Comparison Plot",
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput("plot_eb", "Error bars", choices = c("Standard Error (SE)" = "se", "95% Confidence Interval" = "ci95", "Hide Error Bars" = "none"), selected = "se"),
            div(class = "pt-4", checkboxInput("plot_points", "Overlay individual plot points", value = TRUE)),
            div(class = "pt-4 text-end text-muted small", "Bars sharing a letter are not significantly different.")
          ),
          plotOutput("means_plot", height = "440px")
        )
      ),
      nav_panel(
        "ANOVA Table",
        card_body(
          div(class = "table-responsive", tableOutput("anova_table_ui")),
          conditionalPanel(
            condition = "input.design == 'split_plot'",
            div(
              class = "mt-3",
              h6("Split-Plot Standard Errors & Critical Differences (Gomez & Gomez Formulas)"),
              div(class = "table-responsive", tableOutput("split_metrics_table"))
            )
          ),
          p(class = "text-muted small mt-2 mb-0",
            "Significance codes: '***' p < 0.001, '**' p < 0.01, '*' p < 0.05, '.' p < 0.10, 'NS' Not Significant.")
        )
      ),
      nav_panel(
        "Means & CLD Letters",
        card_body(
          uiOutput("cld_note_ui"),
          div(class = "table-responsive", uiOutput("means_html_table_ui")),
          conditionalPanel(
            condition = "input.design == 'factorial_rcbd' || input.design == 'split_plot'",
            layout_columns(
              col_widths = c(6, 6),
              card(
                class = "mt-3",
                card_header("Factor A / Main-Plot Means"),
                card_body(div(class = "table-responsive", tableOutput("factor_a_table")))
              ),
              card(
                class = "mt-3",
                card_header("Factor B / Sub-Plot Means"),
                card_body(div(class = "table-responsive", tableOutput("factor_b_table")))
              )
            )
          )
        )
      ),
      nav_panel(
        "Interaction Profile",
        card_body(
          plotOutput("interaction_plot", height = "420px")
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # TAB 3: Diagnostics, Smart Transform Advisor & Spatial Field Map
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "3. Diagnostics & Field Map",
    value = "diagnostics",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Assumption Checks & Smart Transformation Advisor"),
            actionButton("apply_rec_transform", "Apply Recommended Transform", class = "btn-sm btn-outline-success")
          )
        ),
        card_body(
          uiOutput("diag_badges_ui"),
          div(class = "table-responsive mt-2", tableOutput("transform_comp_table"))
        )
      ),
      card(
        card_header("Plot Outlier Watchlist (|Std Resid| > 2.5 or High Cook's D)"),
        card_body(
          uiOutput("outliers_status_ui"),
          div(class = "table-responsive", tableOutput("outliers_table"))
        )
      )
    ),
    navset_card_underline(
      nav_panel(
        "2x2 Residual Diagnostics",
        card_body(plotOutput("diagnostics_plot", height = "520px"))
      ),
      nav_panel(
        "Interactive Spatial Field Map",
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            radioButtons("map_metric", "Color plots by:", choices = c("Trait Value" = "observed", "Model Residual (Fertility Drift)" = "residual"), inline = TRUE),
            div(class = "text-muted small pt-2", "Hover over any field plot tile to inspect treatment assignment, observed value, and residual.")
          ),
          uiOutput("spatial_svg_ui")
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # TAB 4: Teaching Lab & Sample-Size / Replicate Planner
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "4. Teaching & Power",
    value = "teaching",
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Replicate & Statistical Power Planner (Before Planting)"),
        card_body(
          sliderInput("plan_cv", "Expected Field CV (%)", min = 4, max = 35, value = 12, step = 1),
          sliderInput("plan_diff", "Minimum Meaningful Difference to Detect (% of Mean)", min = 5, max = 40, value = 15, step = 1),
          layout_columns(
            col_widths = c(6, 6),
            numericInput("plan_trt", "Number of treatments", value = 6, min = 2, max = 30),
            selectInput("plan_design", "Planned design", choices = c("RCBD" = "rcbd", "CRD" = "crd", "Latin Square" = "lsd"), selected = "rcbd")
          ),
          radioButtons("plan_power", "Target power", choices = c("80% (Standard)" = "0.80", "90% (High)" = "0.90"), inline = TRUE),
          uiOutput("plan_summary_ui")
        )
      ),
      card(
        card_header("Power Curve Across Replications"),
        card_body(
          plotOutput("power_curve_plot", height = "340px"),
          div(class = "table-responsive-ft mt-2", tableOutput("power_table_ui"))
        )
      )
    ),
    card(
      class = "mt-3",
      card_header("Field Experimentation Statistics Guide (For Non-Coders & Students)"),
      card_body(
        accordion(
          open = FALSE,
          accordion_panel(
            "1. Why Block? Understanding RCBD Relative Efficiency",
            "Field soil fertility, moisture, and slope rarely stay uniform across a whole site. Blocking groups similar plots together perpendicular to the gradient so natural field variation is separated from treatment differences. Relative Efficiency > 100% proves blocking reduced experimental error compared with a CRD."
          ),
          accordion_panel(
            "2. What do SE(m), SE(d), CD (LSD), and CV% mean?",
            tags$ul(
              tags$li(tags$strong("CV (%):"), " Residual SD divided by the grand mean x 100. Field crop yields typically aim for CV < 10-15%."),
              tags$li(tags$strong("SE(m):"), " Standard Error of a treatment mean = sqrt(MS_error / r). Measures precision of each mean estimate."),
              tags$li(tags$strong("CD (Critical Difference / LSD):"), " Smallest pairwise difference declared significant at alpha = 0.05 or 0.01.")
            )
          ),
          accordion_panel(
            "3. Tukey HSD vs. Fisher's Protected LSD",
            "Fisher's Protected LSD tests pairs only after a significant omnibus F-test, offering higher sensitivity for small planned comparisons. Tukey's HSD controls the family-wise error rate across all pairwise combinations, preventing false positives when testing many varieties."
          ),
          accordion_panel(
            "4. Why Split-Plot Uses Two Error Terms: Error(a) and Error(b)",
            "In a Split-Plot trial, main-plot treatments (e.g., irrigation or tillage) require larger strips and therefore have fewer experimental units and Error(a) = Block x Main Plot. Sub-plots (e.g., varieties) are smaller, replicated more finely, and tested with higher precision against Error(b)."
          ),
          accordion_panel(
            "5. When to Apply Log, Square-Root, or Arcsine Transformations",
            "Use Square-Root(y + 0.5) for small insect/nodule counts, Log(y + 1) when variance grows proportionally with the mean, and Arcsine (angular) transformation for bounded percentage traits like germination % or foliar disease severity %."
          )
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # TAB 5: One-Click Exports & Reproducible R Script
  # ---------------------------------------------------------------------------
  nav_panel(
    title = "5. Export & R Code",
    value = "export",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Serverless Exports (Generated Locally in Browser)"),
        card_body(
          p(class = "small text-muted",
            "All reports, tables, and reproducible R scripts are synthesized locally inside your browser's WebAssembly R runtime."),
          downloadButton("dl_html_report", "Download Publication Report (.html)", class = "btn-success w-100 mb-2"),
          downloadButton("dl_means_csv", "Download Means & CLD Letters (.csv)", class = "btn-outline-primary w-100 mb-2"),
          downloadButton("dl_anova_csv", "Download ANOVA Table (.csv)", class = "btn-outline-primary w-100 mb-2"),
          downloadButton("dl_r_script", "Download Reproducible R Script (.R)", class = "btn-outline-secondary w-100")
        )
      ),
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Reproducible Base-R Script for RStudio"),
            tags$button(
              id = "copy_rcode_btn",
              type = "button",
              class = "btn btn-sm btn-outline-light bg-dark",
              onclick = "copyTextBlock('rcode_viewer', 'copy_rcode_btn')",
              "Copy R Code"
            )
          )
        ),
        card_body(
          tags$pre(id = "rcode_viewer", class = "ft-code-pre", textOutput("r_code_text"))
        )
      )
    )
  ),

  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),

  footer = div(
    class = "container-fluid px-4 pb-3",
    div(
      class = "ft-footer-note",
      span("100% Client-Side in webR (Shinylive) · Zero Server Cost · Your Field Data Never Leaves Your Browser"),
      span(
        "Built by Lunganlung P. G. · ",
        tags$a(href = "https://github.com/lunganlungpg/field-trial-analyser", target = "_blank", "GitHub Repository")
      )
    )
  )
)

server <- function(input, output, session) {
  pasted_df <- reactiveVal(NULL)

  observeEvent(input$apply_paste, {
    df_p <- tryCatch(
      parse_trial_text(input$paste_text),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 6)
        NULL
      }
    )
    if (!is.null(df_p)) {
      pasted_df(df_p)
      showNotification(sprintf("Loaded pasted table: %d rows x %d columns.", nrow(df_p), ncol(df_p)), type = "message")
    }
  })

  # Active dataset reactive
  raw_data <- reactive({
    src <- input$data_source
    if (src == "demo") {
      load_sample_trial(input$demo_id)
    } else if (src == "upload") {
      req(input$upload_file)
      txt <- paste(readLines(input$upload_file$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      parse_trial_text(txt)
    } else {
      df_p <- pasted_df()
      if (is.null(df_p)) parse_trial_text(input$paste_text) else df_p
    }
  })

  # Auto-set design when demo changes
  observeEvent(input$demo_id, {
    if (input$data_source == "demo") {
      idx <- match(input$demo_id, .sample_catalog$id)
      if (!is.na(idx)) {
        updateSelectInput(session, "design", selected = .sample_catalog$design[idx])
      }
    }
  })

  # Update column selectors whenever dataset changes
  observeEvent(raw_data(), {
    df <- raw_data()
    req(is.data.frame(df), ncol(df) >= 2)
    cols <- names(df)
    det <- detect_trial_columns(df)

    resp_sel <- det$response
    if (input$data_source == "demo") {
      idx <- match(input$demo_id, .sample_catalog$id)
      if (!is.na(idx) && .sample_catalog$default_response[idx] %in% cols) {
        resp_sel <- .sample_catalog$default_response[idx]
      }
    }

    opt_cols <- c("(Auto / None)" = "", stats::setNames(cols, cols))
    updateSelectInput(session, "col_response", choices = cols, selected = resp_sel)
    updateSelectInput(session, "col_treatment", choices = cols, selected = if (nzchar(det$treatment)) det$treatment else cols[1])
    updateSelectInput(session, "col_block", choices = cols, selected = if (nzchar(det$block)) det$block else cols[1])
    updateSelectInput(session, "col_factor_a", choices = cols, selected = if (nzchar(det$factor_a)) det$factor_a else cols[1])
    updateSelectInput(session, "col_factor_b", choices = cols, selected = if (nzchar(det$factor_b)) det$factor_b else cols[min(2, length(cols))])
    updateSelectInput(session, "col_main_plot", choices = cols, selected = if (nzchar(det$main_plot)) det$main_plot else cols[1])
    updateSelectInput(session, "col_sub_plot", choices = cols, selected = if (nzchar(det$sub_plot)) det$sub_plot else cols[min(2, length(cols))])
    updateSelectInput(session, "col_row", choices = cols, selected = if (nzchar(det$row)) det$row else cols[1])
    updateSelectInput(session, "col_col", choices = cols, selected = if (nzchar(det$col)) det$col else cols[min(2, length(cols))])
    updateSelectInput(session, "col_map_row", choices = opt_cols, selected = det$row)
    updateSelectInput(session, "col_map_col", choices = opt_cols, selected = det$col)
  })

  observeEvent(input$go_results, {
    nav_select("main_nav", "results", session = session)
  })

  output$demo_note_ui <- renderUI({
    idx <- match(input$demo_id, .sample_catalog$id)
    if (is.na(idx)) return(NULL)
    p(class = "small text-muted mt-1 mb-0", .sample_catalog$teaching_note[idx])
  })

  output$design_help_ui <- renderUI({
    txt <- .design_descriptions[[input$design]]
    p(class = "small text-muted mb-0", txt)
  })

  current_mapping <- reactive({
    df <- raw_data()
    req(df, input$col_response, input$col_response %in% names(df))
    list(
      response = input$col_response,
      treatment = .nz_or(input$col_treatment),
      block = .nz_or(input$col_block),
      factor_a = .nz_or(input$col_factor_a),
      factor_b = .nz_or(input$col_factor_b),
      main_plot = .nz_or(input$col_main_plot),
      sub_plot = .nz_or(input$col_sub_plot),
      row = if (identical(input$design, "lsd")) .nz_or(input$col_row) else .nz_or(input$col_map_row),
      col = if (identical(input$design, "lsd")) .nz_or(input$col_col) else .nz_or(input$col_map_col)
    )
  })

  validation_res <- reactive({
    df <- raw_data()
    mp <- current_mapping()
    validate_trial_design(df, input$design, mp)
  })

  analysis_res <- reactive({
    val <- validation_res()
    validate(need(val$ok, paste(val$errors, collapse = " ")))
    analyse_field_trial(
      df = raw_data(),
      design = input$design,
      mapping = current_mapping(),
      transform = input$transform,
      alpha = as.numeric(input$alpha),
      posthoc = input$posthoc,
      protected = TRUE
    )
  })

  is_dark <- reactive(identical(input$dark_mode, "dark"))

  # Setup Tab UI
  output$health_banner_ui <- renderUI({
    v <- validation_res()
    if (!v$ok) {
      div(class = "ft-health-banner ft-health-err",
          tags$strong("Check Column Mapping Before Running ANOVA:"),
          tags$ul(class = "mb-0 mt-1", lapply(v$errors, tags$li)))
    } else if (length(v$warnings) > 0) {
      div(class = "ft-health-banner ft-health-warn",
          tags$strong("Ready (with notes): "), paste(v$info, collapse = " "),
          tags$ul(class = "mb-0 mt-1", lapply(v$warnings, tags$li)))
    } else {
      div(class = "ft-health-banner ft-health-ok",
          tags$strong("Pre-Flight Check Passed: "), paste(v$info, collapse = " "),
          " Balanced design ready for analysis.")
    }
  })

  output$setup_kpis_ui <- renderUI({
    df <- raw_data()
    req(input$col_response, input$col_response %in% names(df))
    v <- validation_res()
    y_num <- suppressWarnings(as.numeric(df[[input$col_response]]))
    y_ok <- y_num[!is.na(y_num)]
    mean_str <- if (length(y_ok) > 0) sprintf("%.2f", mean(y_ok)) else "—"
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Plots Loaded", nrow(df), theme = "success"),
      value_box("Complete Rows", v$n_obs, theme = "info"),
      value_box("Design Balance", ifelse(v$is_balanced, "Balanced", "Unbalanced"), theme = ifelse(v$is_balanced, "primary", "warning")),
      value_box("Raw Trait Mean", mean_str, theme = "secondary")
    )
  })

  output$dataset_dims_badge <- renderUI({
    df <- raw_data()
    span(class = "badge text-bg-secondary", sprintf("%d rows x %d cols", nrow(df), ncol(df)))
  })

  output$preview_table <- renderTable({
    utils::head(raw_data(), 100)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs")

  # Results Tab UI
  output$results_kpis_ui <- renderUI({
    res <- analysis_res()
    pr <- res$precision
    at <- res$anova_table
    dg <- res$diagnostics

    f_idx <- switch(res$design, crd = 1, rcbd = 2, lsd = 3, factorial_rcbd = 2, split_plot = 2)
    f_p <- at$p_value[f_idx]
    f_sig <- at$Sig[f_idx]
    f_theme <- if (!is.na(f_p) && f_p < res$alpha) "success" else "secondary"

    cv_theme <- if (is.na(pr$cv_pct)) "secondary" else if (pr$cv_pct <= 10) "success" else if (pr$cv_pct <= 20) "info" else "warning"
    assump_ok <- (is.na(dg$shapiro_p) || dg$shapiro_p >= res$alpha) && (is.na(dg$levene_p) || dg$levene_p >= res$alpha)

    sub_eff <- if (!is.null(pr$re_info$rcbd_vs_crd_pct)) {
      sprintf("RCBD Rel. Eff.: %.1f%% vs CRD", pr$re_info$rcbd_vs_crd_pct)
    } else if (!is.null(pr$re_info$cv_a_pct)) {
      sprintf("CV(a) %.1f%% | CV(b) %.1f%%", pr$re_info$cv_a_pct, pr$re_info$cv_b_pct)
    } else {
      sprintf("Outliers flagged: %d", dg$n_outliers)
    }

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Primary F-Test",
        value = sprintf("p = %s (%s)", format.pval(f_p, digits = 3), f_sig),
        p(class = "small mb-0", sprintf("Grand Mean: %.3f", pr$grand_mean)),
        showcase = .svg_icon("target"),
        theme = f_theme
      ),
      value_box(
        title = "Trial Precision (CV %)",
        value = sprintf("%.2f%%", pr$cv_pct),
        p(class = "small mb-0", ifelse(pr$cv_pct <= 10, "High field precision", ifelse(pr$cv_pct <= 20, "Acceptable precision", "High variability"))),
        showcase = .svg_icon("chart"),
        theme = cv_theme
      ),
      value_box(
        title = "CD (5%) / SE(m)",
        value = sprintf("%.3f", pr$cd_05),
        p(class = "small mb-0", sprintf("SE(m): %.3f | Tukey HSD: %.3f", pr$sem, pr$hsd_alpha)),
        showcase = .svg_icon("ruler"),
        theme = "info"
      ),
      value_box(
        title = "Model Assumptions",
        value = ifelse(assump_ok, "Passed", "Review"),
        p(class = "small mb-0", sub_eff),
        showcase = .svg_icon("shield"),
        theme = ifelse(assump_ok, "success", "warning")
      )
    )
  })

  output$interpretation_ui <- renderUI({
    res <- analysis_res()
    tags$p(class = "mb-0", generate_interpretation_text(res))
  })

  output$means_plot <- renderPlot({
    res <- analysis_res()
    plot_treatment_means(res, error_bar = input$plot_eb, show_points = input$plot_points, dark = is_dark())
  }, res = 110)

  output$anova_table_ui <- renderTable({
    analysis_res()$anova_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 4, na = "—")

  output$split_metrics_table <- renderTable({
    res <- analysis_res()
    req(!is.null(res$split_metrics))
    res$split_metrics
  }, striped = TRUE, bordered = TRUE, digits = 4, na = "—")

  output$cld_note_ui <- renderUI({
    res <- analysis_res()
    if (nzchar(res$cld_info$note)) {
      div(class = "alert alert-info py-2 small", res$cld_info$note)
    } else {
      p(class = "small text-muted",
        sprintf("Means sorted descending. Treatments sharing a common letter do not differ significantly (%s, alpha = %.2f).",
                toupper(res$posthoc), res$alpha))
    }
  })

  output$means_html_table_ui <- renderUI({
    mt <- analysis_res()$means_table
    ths <- lapply(names(mt), tags$th)
    rows <- lapply(seq_len(nrow(mt)), function(i) {
      cells <- lapply(names(mt), function(cn) {
        val <- mt[[cn]][i]
        if (cn == "Group") {
          tags$td(span(class = "ft-cld-pill", as.character(val)))
        } else {
          tags$td(as.character(val))
        }
      })
      tags$tr(cells)
    })
    tags$table(
      class = "table table-striped table-hover table-bordered table-sm align-middle",
      tags$thead(tags$tr(ths)),
      tags$tbody(rows)
    )
  })

  output$factor_a_table <- renderTable({
    res <- analysis_res()
    req(length(res$extra_tables) > 0, !is.null(res$extra_tables$factor_a))
    res$extra_tables$factor_a
  }, striped = TRUE, bordered = TRUE, digits = 3)

  output$factor_b_table <- renderTable({
    res <- analysis_res()
    req(length(res$extra_tables) > 0, !is.null(res$extra_tables$factor_b))
    res$extra_tables$factor_b
  }, striped = TRUE, bordered = TRUE, digits = 3)

  output$interaction_plot <- renderPlot({
    plot_interaction_means(analysis_res(), dark = is_dark())
  }, res = 110)

  # Diagnostics Tab UI
  transform_advice <- reactive({
    res <- analysis_res()
    recommend_transformations(raw_data(), res$design, res$mapping, res$alpha)
  })

  observeEvent(input$apply_rec_transform, {
    rec <- transform_advice()$recommended
    updateSelectInput(session, "transform", selected = rec)
    showNotification(paste0("Applied recommended transformation: ", rec), type = "message")
  })

  output$diag_badges_ui <- renderUI({
    res <- analysis_res()
    dg <- res$diagnostics
    rec <- transform_advice()$recommended
    norm_ok <- is.na(dg$shapiro_p) || dg$shapiro_p >= res$alpha
    hom_ok <- is.na(dg$levene_p) || dg$levene_p >= res$alpha
    div(
      class = "d-flex flex-wrap gap-2 mb-2",
      span(class = paste("badge", ifelse(norm_ok, "text-bg-success", "text-bg-warning")),
           sprintf("Shapiro-Wilk Normality: p = %.4f", dg$shapiro_p)),
      span(class = paste("badge", ifelse(hom_ok, "text-bg-success", "text-bg-warning")),
           sprintf("Brown-Forsythe Variance: p = %.4f", dg$levene_p)),
      span(class = "badge text-bg-info",
           sprintf("Recommended: %s", rec))
    )
  })

  output$transform_comp_table <- renderTable({
    transform_advice()$comparison
  }, striped = TRUE, bordered = TRUE, digits = 4)

  output$outliers_status_ui <- renderUI({
    ol <- analysis_res()$outliers
    if (nrow(ol) == 0L) {
      div(class = "alert alert-success mb-0", "No extreme outlier plots detected (all |Std Resid| <= 2.5 and Cook's D within bounds).")
    } else {
      p(class = "small text-warning fw-semibold mb-2",
        sprintf("%d plot(s) flagged for data-entry or field-edge verification:", nrow(ol)))
    }
  })

  output$outliers_table <- renderTable({
    ol <- analysis_res()$outliers
    req(nrow(ol) > 0L)
    ol
  }, striped = TRUE, bordered = TRUE, digits = 3)

  output$diagnostics_plot <- renderPlot({
    plot_diagnostics_grid(analysis_res(), dark = is_dark())
  }, res = 105)

  output$spatial_svg_ui <- renderUI({
    res <- analysis_res()
    HTML(svg_field_heatmap(res, metric = input$map_metric))
  })

  # Teaching & Power Planner Tab
  power_plan <- reactive({
    plan_replications(
      cv_pct = input$plan_cv,
      min_diff_pct = input$plan_diff,
      n_treatments = input$plan_trt,
      design = input$plan_design,
      alpha = as.numeric(input$alpha),
      target_power = as.numeric(input$plan_power)
    )
  })

  output$plan_summary_ui <- renderUI({
    pl <- power_plan()
    rec_txt <- if (is.na(pl$recommended_r)) "> 16 (Increase diff or reduce CV)" else paste(pl$recommended_r, "replicates / treatment")
    div(
      class = "alert alert-success mt-3 mb-0",
      tags$strong("Recommended Design: "), rec_txt,
      tags$br(),
      sprintf("Total plots needed: %s  |  Achieved power: %.1f%%",
              ifelse(is.na(pl$recommended_r), "—", pl$recommended_r * pl$n_treatments),
              pl$achieved_power * 100)
    )
  })

  output$power_curve_plot <- renderPlot({
    plot_power_curve(power_plan(), dark = is_dark())
  }, res = 110)

  output$power_table_ui <- renderTable({
    pl <- power_plan()
    cv <- pl$curve[pl$curve$Error_DF > 0, , drop = FALSE]
    cv$Power_Pct <- sprintf("%.1f%%", cv$Power * 100)
    cv[, c("Replications", "Total_Plots", "Error_DF", "Power_Pct", "LSD_pct")]
  }, striped = TRUE, bordered = TRUE, digits = 2)

  # Export & R Code Tab
  source_fname <- reactive({
    if (input$data_source == "demo") {
      idx <- match(input$demo_id, .sample_catalog$id)
      .sample_catalog$file[idx]
    } else if (input$data_source == "upload" && !is.null(input$upload_file)) {
      input$upload_file$name
    } else {
      "pasted_field_trial.csv"
    }
  })

  output$r_code_text <- renderText({
    generate_reproducible_r_code(analysis_res(), source_label = source_fname())
  })

  output$dl_template <- downloadHandler(
    filename = function() "rcbd_field_trial_template.csv",
    content = function(file) {
      utils::write.csv(load_sample_trial("wheat_rcbd"), file, row.names = FALSE)
    }
  )

  output$dl_html_report <- downloadHandler(
    filename = function() paste0("field_trial_report_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      writeLines(generate_html_report(analysis_res()), file, useBytes = TRUE)
    }
  )

  output$dl_means_csv <- downloadHandler(
    filename = function() paste0("treatment_means_cld_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      utils::write.csv(analysis_res()$means_table, file, row.names = FALSE)
    }
  )

  output$dl_anova_csv <- downloadHandler(
    filename = function() paste0("anova_table_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      utils::write.csv(analysis_res()$anova_table, file, row.names = FALSE)
    }
  )

  output$dl_r_script <- downloadHandler(
    filename = function() paste0("reproducible_analysis_", format(Sys.Date(), "%Y%m%d"), ".R"),
    content = function(file) {
      writeLines(generate_reproducible_r_code(analysis_res(), source_label = source_fname()), file)
    }
  )
}

shinyApp(ui, server)
