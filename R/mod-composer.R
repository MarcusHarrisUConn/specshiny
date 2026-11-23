#' Natural-Language Specification Composer Shiny Module (UI)
#'
#' Provides a `bslib`-styled natural-language composer allowing users to load
#' study presets, compile English descriptions into validated `spec_ir` objects,
#' iteratively refine the specification conversationally, answer structured
#' clarifying questions for underspecified parameters, and inspect transparent
#' rationales for every inferred default.
#'
#' @param id Module namespace ID.
#' @param default_prompt Optional initial prompt pre-filled in the text area.
#' @return A `shiny.tagList` containing the composer UI.
#' @export
#' @examples
#' ui <- mod_spec_composer_ui("composer")
#' inherits(ui, "shiny.tag.list")
mod_spec_composer_ui <- function(id, default_prompt = NULL) {
  ns <- shiny::NS(id)
  presets <- spec_presets()
  prompt_init <- default_prompt %||% presets[[1L]]$prompt

  htmltools::tagList(
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$strong("1. Describe Study in Plain English")),
      bslib::card_body(
        shiny::selectInput(
          inputId = ns("preset_choice"),
          label = "Example Presets",
          choices = stats::setNames(names(presets), vapply(presets, `[[`, character(1), "label")),
          selected = names(presets)[[1L]]
        ),
        shiny::actionButton(
          inputId = ns("load_preset"),
          label = "Load & Compile Preset",
          class = "btn-outline-secondary btn-sm w-100 mb-3"
        ),
        shiny::textAreaInput(
          inputId = ns("prompt"),
          label = "Research Design Description",
          value = prompt_init,
          rows = 4,
          placeholder = "e.g., 2-level power simulation with 60 schools, 25 students per school, ICC 0.2, treatment effect 0.35..."
        ),
        shiny::actionButton(
          inputId = ns("compile"),
          label = "Compile to Verified Spec",
          class = "btn-primary w-100"
        )
      )
    ),
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$strong("2. Conversational Refinement")),
      bslib::card_body(
        shiny::textInput(
          inputId = ns("refine_input"),
          label = "Edit Instruction",
          value = "",
          placeholder = "e.g., increase clusters to 80 and sweep ICC across 0.1, 0.2, 0.3"
        ),
        shiny::actionButton(
          inputId = ns("refine"),
          label = "Apply Refinement",
          class = "btn-success w-100"
        )
      )
    ),
    shiny::uiOutput(ns("status_ui")),
    shiny::uiOutput(ns("clarifications_ui")),
    shiny::uiOutput(ns("rationale_ui"))
  )
}

#' Natural-Language Specification Composer Shiny Module (Server)
#'
#' @param id Module namespace ID.
#' @param spec_rv A `shiny::reactiveVal` holding the current validated `spec_ir`
#'   (shared across composer, controls, and preview modules).
#' @param translator A `spec_translator` backend (defaults to [translator_rule()]).
#' @param last_diff_rv Optional `shiny::reactiveVal` holding the latest `spec_diff`.
#'
#' @return A list of reactive accessors (`spec`, `diff`, `clarifications`,
#'   `status`, `history`).
#' @export
#' @examples
#' spec_rv <- shiny::reactiveVal(NULL)
#' shiny::testServer(
#'   mod_spec_composer_server,
#'   args = list(spec_rv = spec_rv),
#'   {
#'     session$setInputs(prompt = "Single-level study N = 150", compile = 1)
#'     spec_rv()$hierarchy$n_obs
#'   }
#' )
mod_spec_composer_server <- function(id,
                                     spec_rv,
                                     translator = translator_rule(),
                                     last_diff_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(last_diff_rv)) {
      last_diff_rv <- shiny::reactiveVal(NULL)
    }

    clarifications_rv <- shiny::reactiveVal(list())
    status_rv <- shiny::reactiveVal(NULL)
    history_rv <- shiny::reactiveVal(character())

    # Helper to execute compilation cleanly
    do_compile <- function(prompt_txt) {
      old_spec <- shiny::isolate(spec_rv())
      res <- tryCatch(
        nl_to_spec(prompt_txt, translator = translator, validate = TRUE),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        status_rv(list(type = "danger", text = conditionMessage(res)))
        return(invisible(NULL))
      }
      diff_obj <- diff_spec_ir(old_spec, res$spec)
      spec_rv(res$spec)
      last_diff_rv(diff_obj)
      clarifications_rv(res$clarifications)
      history_rv(c(shiny::isolate(history_rv()), paste("Compile:", prompt_txt)))
      status_rv(list(
        type = "success",
        text = sprintf(
          "Compiled valid spec (%s, %d active controls, %d open clarification%s).",
          res$spec$design_type,
          spec_to_controls(res$spec)$control_count,
          length(res$clarifications),
          if (length(res$clarifications) == 1L) "" else "s"
        )
      ))
    }

    # Load preset button
    shiny::observeEvent(input$load_preset, {
      presets <- spec_presets()
      key <- input$preset_choice
      if (!is.null(key) && key %in% names(presets)) {
        pr <- presets[[key]]$prompt
        shiny::updateTextAreaInput(session, "prompt", value = pr)
        do_compile(pr)
      }
    })

    # Compile prompt button
    shiny::observeEvent(input$compile, {
      pr <- trimws(as.character(input$prompt %||% ""))
      if (!nzchar(pr)) {
        status_rv(list(type = "warning", text = "Please enter a natural-language study description."))
        return()
      }
      do_compile(pr)
    })

    # Conversational refinement button
    shiny::observeEvent(input$refine, {
      instr <- trimws(as.character(input$refine_input %||% ""))
      cur <- shiny::isolate(spec_rv())
      if (is.null(cur)) {
        status_rv(list(type = "warning", text = "Compile a base specification before refining."))
        return()
      }
      if (!nzchar(instr)) {
        status_rv(list(type = "warning", text = "Enter a refinement instruction (e.g., 'increase clusters to 80')."))
        return()
      }
      res <- tryCatch(
        refine_spec(cur, instr, translator = translator, validate = TRUE),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        status_rv(list(type = "danger", text = conditionMessage(res)))
        return()
      }
      spec_rv(res$spec)
      last_diff_rv(res$diff)
      history_rv(c(shiny::isolate(history_rv()), paste("Refine:", instr)))
      shiny::updateTextInput(session, "refine_input", value = "")
      if (res$diff$has_changes) {
        status_rv(list(type = "info", text = paste("Refined:", format_diff_summary(res$diff))))
      } else {
        status_rv(list(type = "warning", text = "No recognizable parameter changes found in that instruction."))
      }
    })

    # Apply clarification answers
    shiny::observeEvent(input$apply_clarifications, {
      cur <- shiny::isolate(spec_rv())
      cls <- shiny::isolate(clarifications_rv())
      if (is.null(cur) || length(cls) == 0L) return()

      answers <- list()
      for (cl in cls) {
        inp_val <- input[[paste0("clarif_", cl$id)]]
        if (!is.null(inp_val) && nzchar(as.character(inp_val))) {
          # Coerce numeric strings cleanly when options are numeric
          num_try <- suppressWarnings(as.numeric(inp_val))
          answers[[cl$id]] <- if (!is.na(num_try) && is.numeric(cl$current_default)) num_try else inp_val
        }
      }
      if (length(answers) == 0L) return()

      res <- tryCatch(
        answer_clarifications(cur, answers = answers, clarifications = cls, validate = TRUE),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        status_rv(list(type = "danger", text = conditionMessage(res)))
        return()
      }
      spec_rv(res$spec)
      last_diff_rv(res$diff)
      clarifications_rv(res$clarifications)
      status_rv(list(type = "success", text = paste("Applied clarifications:", format_diff_summary(res$diff))))
    })

    output$status_ui <- shiny::renderUI({
      st <- status_rv()
      if (is.null(st)) return(NULL)
      htmltools::tags$div(
        class = sprintf("alert alert-%s py-2 px-3 small mb-3", st$type),
        st$text
      )
    })

    output$clarifications_ui <- shiny::renderUI({
      cls <- clarifications_rv()
      if (length(cls) == 0L) return(NULL)

      q_widgets <- lapply(cls, function(cl) {
        opt_vals <- unlist(cl$options, use.names = FALSE)
        shiny::selectInput(
          inputId = ns(paste0("clarif_", cl$id)),
          label = cl$question,
          choices = as.character(opt_vals),
          selected = as.character(cl$current_default)
        )
      })

      bslib::card(
        class = "border-warning mb-3",
        bslib::card_header(
          class = "bg-warning-subtle py-2",
          htmltools::tags$strong(sprintf("Clarifications Needed (%d)", length(cls)))
        ),
        bslib::card_body(
          class = "py-2",
          htmltools::tagList(q_widgets),
          shiny::actionButton(
            inputId = ns("apply_clarifications"),
            label = "Confirm Choices",
            class = "btn-warning btn-sm w-100"
          )
        )
      )
    })

    output$rationale_ui <- shiny::renderUI({
      sp <- spec_rv()
      if (is.null(sp) || length(sp$defaults_rationale) == 0L) return(NULL)

      items <- lapply(names(sp$defaults_rationale), function(k) {
        htmltools::tags$li(
          class = "mb-1",
          htmltools::tags$code(k),
          ": ",
          sp$defaults_rationale[[k]]
        )
      })

      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          title = sprintf("Inferred Defaults & Rationale (%d)", length(items)),
          htmltools::tags$ul(class = "small ps-3 mb-0", items)
        )
      )
    })

    list(
      spec = spec_rv,
      diff = last_diff_rv,
      clarifications = clarifications_rv,
      status = status_rv,
      history = history_rv
    )
  })
}
