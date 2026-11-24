#' Built-in natural-language study presets for the `specshiny` demo app
#'
#' @return A named list of preset entries, each containing `label` and `prompt`.
#' @export
#' @examples
#' names(spec_presets())
spec_presets <- function() {
  list(
    school_power_mlm = list(
      label = "2-Level School Power Study (Random Slope + MAR)",
      prompt = paste(
        "2-level multilevel power simulation with 60 schools, 25 students per school,",
        "ICC 0.2, treatment effect 0.35, random slope for x1, 15% MAR missingness,",
        "500 replications"
      )
    ),
    cross_level_interaction = list(
      label = "2-Level Cross-Level Interaction + Unbalanced Clusters + Heavy Tails",
      prompt = paste(
        "2-level power analysis for the cross-level interaction between x1 and z1 of 0.30",
        "with 50 schools, 20 students per school, cluster size sd 5, ICC 0.2,",
        "level-2 z1 effect 0.25, random slope for x1, t-distributed errors df 5,",
        "300 replications"
      )
    ),
    single_level_lm = list(
      label = "Single-Level Linear Monte Carlo (Minimal UI)",
      prompt = paste(
        "Single-level Monte Carlo simulation with sample size 250,",
        "predictors x1 and x2, x1 effect 0.4, residual sd 1.2, 300 replications"
      )
    ),
    logistic_mlm = list(
      label = "2-Level Binary Logistic Mixed Model (MCAR)",
      prompt = paste(
        "2-level multilevel study with 50 clinics, 20 patients per clinic, ICC 0.15,",
        "binary outcome with logit link, treatment effect 0.6, 10% MCAR missingness,",
        "250 replications"
      )
    ),
    power_grid_sweep = list(
      label = "Multilevel Power Curve (Cluster & ICC Grid Sweep)",
      prompt = paste(
        "2-level power analysis with 40 schools, 20 students per school, ICC 0.2,",
        "treatment effect 0.4, sweep clusters across 30, 60, 90 and sweep ICC across 0.1, 0.2, 0.3,",
        "200 replications"
      )
    ),
    longitudinal_growth_ar1 = list(
      label = "Longitudinal Growth Curve (AR(1) + Monotone Dropout)",
      prompt = paste(
        "Longitudinal growth curve simulation with 120 subjects across 5 waves,",
        "ICC 0.4, AR(1) correlation 0.35, growth slope 0.3, random slope for time,",
        "treatment effect 0.25, 25% monotone dropout, 250 replications"
      )
    )
  )
}

#' Build the flagship `specshiny` reference demo Shiny app object
#'
#' Constructs a complete `bslib` Shiny application showcasing the full
#' neuro-symbolic pipeline: Natural Language -> Validated `spec_ir` -> Dynamic
#' Minimal Controls with Undo/Redo History -> Bidirectional Sync & Visual Diffs
#' -> Deterministic AST-Verified R & Quarto Compilation.
#'
#' @param translator A `spec_translator` backend (defaults to [translator_rule()]).
#' @param initial_prompt Optional initial prompt compiled on startup. Defaults
#'   to the flagship 2-level school power simulation preset.
#'
#' @return A `shiny.appobj` ready to run with `shiny::runApp()`.
#' @export
#' @examples
#' app <- specshiny_demo_app()
#' inherits(app, "shiny.appobj")
specshiny_demo_app <- function(translator = translator_rule(), initial_prompt = NULL) {
  presets <- spec_presets()
  init_prompt <- initial_prompt %||% presets[[1L]]$prompt
  init_translation <- nl_to_spec(init_prompt, translator = translator, validate = TRUE)

  ui <- bslib::page_sidebar(
    title = "specshiny: Neuro-Symbolic Statistical Study Designer",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#264653", secondary = "#6e7f8d"),
    sidebar = bslib::sidebar(
      width = 390,
      open = "desktop",
      mod_spec_composer_ui("composer", default_prompt = init_prompt)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      mod_spec_controls_ui("controls"),
      mod_spec_preview_ui("preview")
    )
  )

  server <- function(input, output, session) {
    spec_rv <- shiny::reactiveVal(init_translation$spec)
    last_diff_rv <- shiny::reactiveVal(NULL)
    hist_stack <- spec_history(initial_spec = init_translation$spec)

    mod_spec_composer_server("composer", spec_rv = spec_rv, translator = translator, last_diff_rv = last_diff_rv)
    mod_spec_controls_server("controls", spec_rv = spec_rv, last_diff_rv = last_diff_rv, history = hist_stack)
    mod_spec_preview_server("preview", spec_rv = spec_rv, last_diff_rv = last_diff_rv)
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Launch the bundled `specshiny` flagship reference demo Shiny application
#'
#' @param translator A `spec_translator` backend (defaults to [translator_rule()]).
#' @param launch.browser Logical; whether to open the app in a web browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Runs the Shiny application (invisibly returns the result of
#'   [shiny::runApp()]).
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   run_specshiny_demo()
#' }
#' }
run_specshiny_demo <- function(translator = translator_rule(),
                               launch.browser = interactive(),
                               ...) {
  app <- specshiny_demo_app(translator = translator)
  shiny::runApp(app, launch.browser = launch.browser, ...)
}
