#' Dynamic Specification Controls Shiny Module (UI)
#'
#' Dynamically synthesizes and renders **only** the `bslib` controls needed for
#' the active `spec_ir`, staying bidirectionally synchronized with both visual
#' widget edits and conversational IR updates while displaying undo/redo
#' history buttons, live visual diff badges, and inline statistical guardrail
#' alerts.
#'
#' @param id Module namespace ID.
#' @return A `shiny.tagList` containing the dynamic controls panel.
#' @export
#' @examples
#' ui <- mod_spec_controls_ui("controls")
#' inherits(ui, "shiny.tag.list")
mod_spec_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("controls_header")),
    shiny::uiOutput(ns("diff_banner")),
    shiny::uiOutput(ns("guardrail_alert")),
    shiny::uiOutput(ns("dynamic_controls")),
    shiny::uiOutput(ns("grid_manager_ui"))
  )
}

#' Dynamic Specification Controls Shiny Module (Server)
#'
#' Maintains loop-free bidirectional synchronization between `spec_rv` and the
#' dynamically synthesized UI widgets, backed by a [spec_history()] undo/redo
#' stack that tracks both visual widget edits and conversational updates.
#' Structural IR changes re-synthesize the active control layout; value-only IR
#' changes update active widgets in place; UI edits are validated through
#' [patch_spec_from_control()] and rejected with inline diagnostic alerts if
#' they violate statistical sanity guardrails.
#'
#' @param id Module namespace ID.
#' @param spec_rv A `shiny::reactiveVal` holding the current validated `spec_ir`.
#' @param last_diff_rv Optional `shiny::reactiveVal` holding the latest `spec_diff`.
#' @param history Optional [spec_history()] object; created automatically from
#'   the initial `spec_rv()` when `NULL`.
#'
#' @return A list of reactives and accessors: `descriptor`, `guardrail_error`,
#'   `diff`, `history`, `can_undo`, and `can_redo`.
#' @export
#' @examples
#' spec_rv <- shiny::reactiveVal(new_spec_ir())
#' shiny::testServer(
#'   mod_spec_controls_server,
#'   args = list(spec_rv = spec_rv),
#'   {
#'     session$setInputs(n_obs = 300)
#'     spec_rv()$hierarchy$n_obs
#'   }
#' )
mod_spec_controls_server <- function(id, spec_rv, last_diff_rv = NULL, history = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(last_diff_rv)) {
      last_diff_rv <- shiny::reactiveVal(NULL)
    }

    init_sp <- shiny::isolate(spec_rv())
    if (is.null(history)) {
      history <- spec_history(initial_spec = init_sp)
    } else {
      assert_spec_history(history)
      if (!is.null(init_sp) && is.null(history_current(history))) {
        history_push(history, init_sp, label = "Initial specification")
      }
    }

    guardrail_error_rv <- shiny::reactiveVal(NULL)
    structure_key_rv <- shiny::reactiveVal("")
    render_tick_rv <- shiny::reactiveVal(0L)
    history_tick_rv <- shiny::reactiveVal(0L)
    last_seen_spec_rv <- shiny::reactiveVal(init_sp)
    navigating_history_env <- new.env(parent = emptyenv())
    navigating_history_env$active <- FALSE
    registered_ids <- new.env(parent = emptyenv())

    descriptor_r <- shiny::reactive({
      sp <- spec_rv()
      shiny::req(sp)
      spec_to_controls(sp)
    })

    can_undo_r <- shiny::reactive({
      history_tick_rv()
      history_can_undo(history)
    })

    can_redo_r <- shiny::reactive({
      history_tick_rv()
      history_can_redo(history)
    })

    # Register an observer once per unique control_id so UI -> IR works without leaks
    register_control_observer <- function(cid) {
      if (exists(cid, envir = registered_ids, inherits = FALSE)) {
        return(invisible(NULL))
      }
      assign(cid, TRUE, envir = registered_ids)

      shiny::observeEvent(input[[cid]], {
        cur <- shiny::isolate(spec_rv())
        if (is.null(cur)) return()
        val <- input[[cid]]
        if (is.null(val)) return()

        cur_desc <- spec_to_controls(cur)
        if (!(cid %in% cur_desc$control_ids)) return()

        res <- patch_spec_from_control(cur, cid, val)
        if (!res$changed) {
          if (res$validation$valid) {
            guardrail_error_rv(NULL)
          }
          return()
        }

        if (res$validation$valid) {
          guardrail_error_rv(NULL)
          last_diff_rv(res$diff)
          spec_rv(res$spec)
        } else {
          guardrail_error_rv(res$validation)
        }
      }, ignoreInit = FALSE, ignoreNULL = TRUE)
    }

    # Wire initial observers immediately if spec_rv is already populated
    if (!is.null(init_sp)) {
      init_desc <- spec_to_controls(init_sp)
      for (cid in init_desc$control_ids) {
        register_control_observer(cid)
      }
    }

    # IR -> UI synchronization observer (also records undo/redo snapshots)
    shiny::observeEvent(spec_rv(), {
      sp <- spec_rv()
      if (is.null(sp)) return()
      prev_sp <- shiny::isolate(last_seen_spec_rv())
      if (!is.null(prev_sp) && diff_spec_ir(prev_sp, sp)$has_changes) {
        guardrail_error_rv(NULL)
      }
      last_seen_spec_rv(sp)

      if (!isTRUE(navigating_history_env$active)) {
        history_push(history, sp)
        history_tick_rv(shiny::isolate(history_tick_rv()) + 1L)
      }

      desc <- spec_to_controls(sp)

      for (cid in desc$control_ids) {
        register_control_observer(cid)
      }

      prev_key <- shiny::isolate(structure_key_rv())
      if (!identical(prev_key, desc$structure_key)) {
        structure_key_rv(desc$structure_key)
        render_tick_rv(shiny::isolate(render_tick_rv()) + 1L)
      } else {
        for (sec in desc$sections) {
          for (ct in sec$controls) {
            push_widget_value(session, ct)
          }
        }
      }
    }, ignoreNULL = TRUE)

    # Undo button handler
    shiny::observeEvent(input$undo_btn, {
      res <- history_undo(history)
      if (is.null(res)) return()
      navigating_history_env$active <- TRUE
      on.exit(navigating_history_env$active <- FALSE, add = TRUE)
      guardrail_error_rv(NULL)
      last_diff_rv(res$diff)
      spec_rv(res$spec)
      history_tick_rv(shiny::isolate(history_tick_rv()) + 1L)
    })

    # Redo button handler
    shiny::observeEvent(input$redo_btn, {
      res <- history_redo(history)
      if (is.null(res)) return()
      navigating_history_env$active <- TRUE
      on.exit(navigating_history_env$active <- FALSE, add = TRUE)
      guardrail_error_rv(NULL)
      last_diff_rv(res$diff)
      spec_rv(res$spec)
      history_tick_rv(shiny::isolate(history_tick_rv()) + 1L)
    })

    # Add a parameter grid axis via button
    shiny::observeEvent(input$add_grid_btn, {
      cur <- shiny::isolate(spec_rv())
      axis <- input$add_grid_axis
      if (is.null(cur) || !is_chr_scalar(axis) || !nzchar(axis)) return()

      default_vec <- default_grid_values_for_axis(cur, axis)
      s <- cur
      s$simulation$parameter_grid[[axis]] <- default_vec
      cand <- as_spec_ir(s, validate = FALSE)
      val_rep <- validate_spec_ir(cand, strict = FALSE)
      if (val_rep$valid) {
        guardrail_error_rv(NULL)
        last_diff_rv(diff_spec_ir(cur, cand))
        spec_rv(cand)
      } else {
        guardrail_error_rv(val_rep)
      }
    })

    output$controls_header <- shiny::renderUI({
      desc <- descriptor_r()
      u_ok <- can_undo_r()
      r_ok <- can_redo_r()
      htmltools::tags$div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-2",
        htmltools::tags$h5(class = "mb-0", "Dynamic Design Controls"),
        htmltools::tags$div(
          class = "d-flex align-items-center gap-2",
          shiny::actionButton(
            inputId = ns("undo_btn"),
            label = "Undo",
            class = if (u_ok) "btn-outline-secondary btn-sm" else "btn-outline-secondary btn-sm disabled"
          ),
          shiny::actionButton(
            inputId = ns("redo_btn"),
            label = "Redo",
            class = if (r_ok) "btn-outline-secondary btn-sm" else "btn-outline-secondary btn-sm disabled"
          ),
          htmltools::tags$span(
            class = "badge text-bg-dark",
            sprintf("%d active control%s", desc$control_count, if (desc$control_count == 1L) "" else "s")
          )
        )
      )
    })

    output$diff_banner <- shiny::renderUI({
      render_diff_badges_ui(last_diff_rv())
    })

    output$guardrail_alert <- shiny::renderUI({
      err_rep <- guardrail_error_rv()
      if (is.null(err_rep) || isTRUE(err_rep$valid)) return(NULL)
      items <- lapply(seq_len(nrow(err_rep$errors)), function(i) {
        htmltools::tags$li(
          htmltools::tags$strong(sprintf("[%s] ", err_rep$errors$path[[i]])),
          err_rep$errors$message[[i]]
        )
      })
      htmltools::tags$div(
        class = "alert alert-danger py-2 px-3 mb-3 small",
        htmltools::tags$div(class = "fw-bold mb-1", "Statistical Guardrail Blocked Invalid Edit:"),
        htmltools::tags$ul(class = "mb-0 ps-3", items)
      )
    })

    output$dynamic_controls <- shiny::renderUI({
      render_tick_rv()
      sp <- shiny::isolate(spec_rv())
      shiny::req(sp)
      desc <- spec_to_controls(sp)
      render_spec_controls_ui(desc, ns = ns, diff = shiny::isolate(last_diff_rv()))
    })

    output$grid_manager_ui <- shiny::renderUI({
      desc <- descriptor_r()
      sp <- spec_rv()
      active_axes <- names(sp$simulation$parameter_grid %||% list())
      addable <- setdiff(desc$available_grid_axes, active_axes)
      if (length(addable) == 0L) return(NULL)

      bslib::card(
        class = "mb-3",
        bslib::card_body(
          class = "py-2 d-flex align-items-end gap-2",
          htmltools::tags$div(
            class = "flex-grow-1",
            shiny::selectInput(
              inputId = ns("add_grid_axis"),
              label = "Add Parameter Grid Sweep",
              choices = addable
            )
          ),
          shiny::actionButton(
            inputId = ns("add_grid_btn"),
            label = "Add Sweep",
            class = "btn-outline-primary mb-3"
          )
        )
      )
    })

    list(
      descriptor = descriptor_r,
      guardrail_error = guardrail_error_rv,
      diff = last_diff_rv,
      history = history,
      can_undo = can_undo_r,
      can_redo = can_redo_r
    )
  })
}

push_widget_value <- function(session, ct) {
  if (ct$type == "numeric") {
    shiny::updateNumericInput(session, ct$id, value = ct$value)
  } else if (ct$type == "slider") {
    shiny::updateSliderInput(session, ct$id, value = ct$value)
  } else if (ct$type == "select") {
    shiny::updateSelectInput(session, ct$id, choices = ct$choices, selected = ct$value)
  } else if (ct$type == "checkbox") {
    shiny::updateCheckboxInput(session, ct$id, value = isTRUE(ct$value))
  } else if (ct$type == "text") {
    shiny::updateTextInput(session, ct$id, value = as.character(ct$value %||% ""))
  }
}

default_grid_values_for_axis <- function(spec, axis) {
  if (axis == "n_obs") {
    base <- as.integer(spec$hierarchy$n_obs)
    unique(pmax(10L, as.integer(round(c(0.5, 1, 1.5) * base))))
  } else if (axis == "n_clusters") {
    base <- as.integer(spec$hierarchy$n_clusters)
    unique(pmax(10L, as.integer(round(c(0.5, 1, 1.5) * base))))
  } else if (axis == "cluster_size") {
    base <- as.integer(spec$hierarchy$cluster_size)
    unique(pmax(5L, as.integer(round(c(0.5, 1, 1.5) * base))))
  } else if (axis == "icc") {
    c(0.1, 0.2, 0.3)
  } else if (axis == "focal_effect") {
    c(0.2, 0.35, 0.5)
  } else if (axis == "residual_sd") {
    c(0.75, 1.0, 1.5)
  } else if (axis == "missing_rate") {
    c(0.05, 0.15, 0.30)
  } else if (axis == "ar1_rho") {
    c(0, 0.3, 0.6)
  } else {
    c(1, 2)
  }
}
