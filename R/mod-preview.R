#' Live Specification & Deterministic Codegen Preview Shiny Module (UI)
#'
#' Displays a live multi-tab `bslib` inspector for a validated `spec_ir`:
#' mathematical model summary, serialized YAML/JSON IR, deterministic standalone
#' R script (with bounded budget-capped preview execution), and Quarto `.qmd`
#' report, each with download handlers.
#'
#' @param id Module namespace ID.
#' @return A `bslib::navset_card_tab()` UI component.
#' @export
#' @examples
#' ui <- mod_spec_preview_ui("preview")
#' inherits(ui, "bslib_fragment") || inherits(ui, "shiny.tag")
mod_spec_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    id = ns("preview_tabs"),
    title = "Verified Spec & Deterministic Output",
    bslib::nav_panel(
      title = "Model Summary",
      shiny::uiOutput(ns("summary_ui"))
    ),
    bslib::nav_panel(
      title = "Spec IR (YAML/JSON)",
      htmltools::tags$div(
        class = "d-flex justify-content-between align-items-center mb-2",
        shiny::radioButtons(
          inputId = ns("ir_format"),
          label = NULL,
          choices = c("YAML" = "yaml", "JSON" = "json"),
          selected = "yaml",
          inline = TRUE
        ),
        htmltools::tags$div(
          class = "d-flex gap-2",
          shiny::downloadButton(ns("dl_yaml"), "Download .yaml", class = "btn-sm btn-outline-secondary"),
          shiny::downloadButton(ns("dl_json"), "Download .json", class = "btn-sm btn-outline-secondary")
        )
      ),
      shiny::verbatimTextOutput(ns("ir_code"))
    ),
    bslib::nav_panel(
      title = "R Script",
      htmltools::tags$div(
        class = "d-flex justify-content-between align-items-center mb-2",
        shiny::actionButton(
          inputId = ns("run_preview"),
          label = "Run Quick Preview (capped)",
          class = "btn-sm btn-outline-primary"
        ),
        shiny::downloadButton(ns("dl_r"), "Download .R", class = "btn-sm btn-primary")
      ),
      shiny::uiOutput(ns("preview_run_ui")),
      shiny::verbatimTextOutput(ns("r_code"))
    ),
    bslib::nav_panel(
      title = "Quarto Report",
      htmltools::tags$div(
        class = "d-flex justify-content-end mb-2",
        shiny::downloadButton(ns("dl_qmd"), "Download .qmd", class = "btn-sm btn-primary")
      ),
      shiny::verbatimTextOutput(ns("qmd_code"))
    )
  )
}

#' Live Specification & Deterministic Codegen Preview Shiny Module (Server)
#'
#' Reactively renders mathematical summaries, YAML/JSON IR, AST-verified R code,
#' and Quarto reports, and enforces hard replication, sample-size, and wall-time
#' budgets on the interactive quick preview runner so large simulation specs
#' (e.g., `n_reps = 50000`) never freeze the Shiny session.
#'
#' @param id Module namespace ID.
#' @param spec_rv A `shiny::reactiveVal` holding the current validated `spec_ir`.
#' @param last_diff_rv Optional `shiny::reactiveVal` holding the latest `spec_diff`.
#' @param max_preview_reps Positive integer hard cap on preview replications
#'   (defaults to `10L`).
#' @param max_preview_n Positive integer soft cap on total observations simulated
#'   per preview replication (defaults to `2000L`).
#' @param max_preview_seconds Positive numeric wall-clock budget in seconds for
#'   the preview loop (defaults to `5`).
#'
#' @return A list of reactive expressions: `math`, `yaml`, `json`, `r_script`,
#'   `quarto_report`, `preview_results`, and `preview_warnings`.
#' @export
#' @examples
#' spec_rv <- shiny::reactiveVal(new_spec_ir(simulation = list(n_reps = 5L, seed = 1L)))
#' shiny::testServer(
#'   mod_spec_preview_server,
#'   args = list(spec_rv = spec_rv),
#'   {
#'     grepl("run_simulation", session$returned$r_script(), fixed = TRUE)
#'   }
#' )
mod_spec_preview_server <- function(id,
                                    spec_rv,
                                    last_diff_rv = NULL,
                                    max_preview_reps = 10L,
                                    max_preview_n = 2000L,
                                    max_preview_seconds = 5) {
  shiny::moduleServer(id, function(input, output, session) {
    preview_rv <- shiny::reactiveVal(NULL)
    preview_warn_rv <- shiny::reactiveVal(character())

    math_r <- shiny::reactive({
      sp <- spec_rv()
      shiny::req(sp)
      spec_to_math(sp)
    })

    yaml_r <- shiny::reactive({
      sp <- spec_rv()
      shiny::req(sp)
      as_spec_yaml(sp)
    })

    json_r <- shiny::reactive({
      sp <- spec_rv()
      shiny::req(sp)
      as_spec_json(sp, pretty = TRUE)
    })

    r_script_r <- shiny::reactive({
      sp <- spec_rv()
      shiny::req(sp)
      compile_r_script(sp)
    })

    quarto_r <- shiny::reactive({
      sp <- spec_rv()
      shiny::req(sp)
      compile_quarto_report(sp)
    })

    shiny::observeEvent(spec_rv(), {
      preview_rv(NULL)
      preview_warn_rv(character())
    })

    # Safe budget-capped pure-R preview simulation (zero eval/parse)
    shiny::observeEvent(input$run_preview, {
      sp <- shiny::isolate(spec_rv())
      if (is.null(sp)) return()
      res <- tryCatch(
        run_spec_preview_sim(
          sp,
          max_reps = max_preview_reps,
          max_n = max_preview_n,
          max_seconds = max_preview_seconds
        ),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        preview_rv(res)
        preview_warn_rv(character())
      } else {
        preview_rv(res$summary)
        preview_warn_rv(res$warnings)
      }
    })

    output$summary_ui <- shiny::renderUI({
      sp <- spec_rv()
      shiny::req(sp)
      m <- math_r()
      desc <- spec_to_controls(sp)
      focal <- sp$effects$focal_predictor %||% sp$predictors[[1L]]$name
      focal_b <- spec_focal_beta(sp)

      htmltools::tagList(
        htmltools::tags$div(
          class = "d-flex flex-wrap gap-2 mb-3",
          htmltools::tags$span(class = "badge text-bg-success", "Schema & AST Guardrails: PASS"),
          htmltools::tags$span(class = "badge text-bg-primary", paste("Design:", sp$design_type)),
          htmltools::tags$span(class = "badge text-bg-secondary", paste("Expected N:", spec_total_n(sp))),
          htmltools::tags$span(class = "badge text-bg-info", paste("Estimator:", sp$estimator$engine)),
          htmltools::tags$span(class = "badge text-bg-dark", sprintf("Focal %s = %s", focal, format(focal_b))),
          htmltools::tags$span(class = "badge text-bg-light border text-dark", sprintf("%d Dynamic Controls", desc$control_count))
        ),
        htmltools::tags$h6("Model Formula"),
        htmltools::tags$pre(class = "p-2 bg-light border rounded small", m$formula),
        htmltools::tags$h6("Mathematical Specification (LaTeX)"),
        htmltools::tags$pre(class = "p-2 bg-light border rounded small", m$latex),
        htmltools::tags$h6("Design Digest"),
        htmltools::tags$pre(class = "p-2 bg-light border rounded small mb-0", m$text)
      )
    })

    output$ir_code <- shiny::renderText({
      fmt <- input$ir_format %||% "yaml"
      if (identical(fmt, "json")) json_r() else yaml_r()
    })

    output$r_code <- shiny::renderText({
      r_script_r()
    })

    output$qmd_code <- shiny::renderText({
      quarto_r()
    })

    output$preview_run_ui <- shiny::renderUI({
      res <- preview_rv()
      warns <- preview_warn_rv()
      if (is.null(res)) return(NULL)
      if (inherits(res, "error")) {
        return(htmltools::tags$div(class = "alert alert-danger small py-2", conditionMessage(res)))
      }
      warn_node <- if (length(warns) > 0L) {
        htmltools::tags$div(
          class = "alert alert-warning py-2 px-3 small mb-2",
          htmltools::tags$strong("Preview Budget Guardrails Applied:"),
          htmltools::tags$ul(class = "mb-0 ps-3", lapply(warns, htmltools::tags$li))
        )
      } else {
        NULL
      }
      tbl_lines <- utils::capture.output(print(res, row.names = FALSE))
      htmltools::tagList(
        warn_node,
        htmltools::tags$div(
          class = "alert alert-success py-2 px-3 small mb-2",
          htmltools::tags$strong(sprintf("Quick Preview Simulation (%d completed rep%s, first grid cell):", res$n_reps, if (res$n_reps == 1L) "" else "s")),
          htmltools::tags$pre(class = "mb-0 mt-1", paste(tbl_lines, collapse = "\n"))
        )
      )
    })

    output$dl_yaml <- shiny::downloadHandler(
      filename = function() "specshiny_spec.yaml",
      content = function(file) writeLines(yaml_r(), file, useBytes = TRUE)
    )
    output$dl_json <- shiny::downloadHandler(
      filename = function() "specshiny_spec.json",
      content = function(file) writeLines(json_r(), file, useBytes = TRUE)
    )
    output$dl_r <- shiny::downloadHandler(
      filename = function() "specshiny_simulation.R",
      content = function(file) writeLines(r_script_r(), file, useBytes = TRUE)
    )
    output$dl_qmd <- shiny::downloadHandler(
      filename = function() "specshiny_report.qmd",
      content = function(file) writeLines(quarto_r(), file, useBytes = TRUE)
    )

    list(
      math = math_r,
      yaml = yaml_r,
      json = json_r,
      r_script = r_script_r,
      quarto_report = quarto_r,
      preview_results = preview_rv,
      preview_warnings = preview_warn_rv
    )
  })
}

# Pure-R bounded preview simulator (zero eval/parse; restores global .Random.seed)
run_spec_preview_sim <- function(spec,
                                 max_reps = 10L,
                                 max_n = 2000L,
                                 max_seconds = 5) {
  assert_valid_spec_ir(spec)
  engine <- spec$estimator$engine
  if (engine %in% c("lmer", "glmer") && !requireNamespace("lme4", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg lme4} is required to run multilevel preview simulations.")
  }

  # Preserve global RNG state cleanly (CRAN hygiene)
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (has_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    } else {
      RNGkind(kind = old_kind[[1L]], normal.kind = old_kind[[2L]], sample.kind = old_kind[[3L]])
    }
  }, add = TRUE)

  warns <- character()
  orig_reps <- as.integer(spec$simulation$n_reps)
  n_reps <- min(orig_reps, as.integer(max_reps))
  if (orig_reps > n_reps) {
    warns <- c(
      warns,
      sprintf("Replications capped from %d to %d to keep Shiny responsive; download simulation.R for full execution.", orig_reps, n_reps)
    )
  }
  if (length(spec$simulation$parameter_grid %||% list()) > 0L) {
    warns <- c(warns, "Preview executes the baseline design cell only; download simulation.R for the full parameter grid.")
  }

  levels_val <- as.integer(spec$hierarchy$levels)
  is_long <- identical(spec$design_type, "longitudinal_growth")
  eff_n_obs <- if (levels_val == 1L) as.integer(spec$hierarchy$n_obs) else NA_integer_
  eff_n_cl <- if (levels_val == 2L) as.integer(spec$hierarchy$n_clusters) else NA_integer_
  eff_cs <- if (levels_val == 2L) as.integer(spec$hierarchy$cluster_size) else NA_integer_
  eff_cs_sd <- if (levels_val == 2L && !is.null(spec$hierarchy$cluster_size_sd)) {
    as.numeric(spec$hierarchy$cluster_size_sd)
  } else {
    NA_real_
  }

  total_n_orig <- as.numeric(spec_total_n(spec))
  if (!is.finite(total_n_orig) || total_n_orig > as.numeric(max_n)) {
    scaled_down <- FALSE
    if (levels_val == 1L) {
      orig_n_obs <- eff_n_obs
      eff_n_obs <- min(orig_n_obs, as.integer(max_n))
      scaled_down <- eff_n_obs < orig_n_obs
    } else if (is_long) {
      orig_n_cl <- eff_n_cl
      q_min <- max(3L, as.integer(isTRUE(spec$effects$random$intercept)) + length(spec$effects$random$slopes %||% list()) + 2L)
      floor_cl <- min(orig_n_cl, q_min)
      target_cl <- as.integer(floor(as.numeric(max_n) / as.numeric(eff_cs)))
      eff_n_cl <- min(orig_n_cl, max(floor_cl, target_cl))
      scaled_down <- eff_n_cl < orig_n_cl
    } else {
      orig_n_cl <- eff_n_cl
      orig_cs <- eff_cs
      scale_f <- if (is.finite(total_n_orig) && total_n_orig > 0) sqrt(as.numeric(max_n) / total_n_orig) else 0.1
      q_min <- max(3L, as.integer(isTRUE(spec$effects$random$intercept)) + length(spec$effects$random$slopes %||% list()) + 2L)
      eff_n_cl <- min(orig_n_cl, max(min(orig_n_cl, q_min), as.integer(floor(as.numeric(orig_n_cl) * scale_f))))
      eff_cs <- min(orig_cs, max(min(orig_cs, q_min), as.integer(floor(as.numeric(orig_cs) * scale_f))))
      if (is.finite(eff_cs_sd)) {
        eff_cs_sd <- max(0.5, eff_cs_sd * scale_f)
      }
      scaled_down <- (as.numeric(eff_n_cl) * as.numeric(eff_cs)) < total_n_orig
    }
    if (isTRUE(scaled_down)) {
      warns <- c(
        warns,
        sprintf("Preview sample size scaled down from N = %.0f to fit the %d-observation interactive budget.", total_n_orig, as.integer(max_n))
      )
    } else {
      warns <- c(
        warns,
        sprintf("Preview retained minimum identifiable sample size (N = %.0f exceeds the %d-observation interactive budget).", total_n_orig, as.integer(max_n))
      )
    }
  }

  y_name <- spec$outcome$name
  fam <- spec$outcome$family
  lnk <- spec$outcome$link
  pred_names <- vapply(spec$predictors, `[[`, character(1), "name")
  focal_raw <- spec$effects$focal_predictor %||% pred_names[[1L]]
  focal <- if (grepl(":", focal_raw, fixed = TRUE)) {
    paste(canonical_interaction_terms(strsplit(focal_raw, ":", fixed = TRUE)[[1L]], pred_names), collapse = ":")
  } else {
    focal_raw
  }
  true_beta <- spec_focal_beta(spec)
  form <- stats::as.formula(spec_to_math(spec)$formula)

  set.seed(as.integer(spec$simulation$seed), kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")

  run_one <- function() {
    if (levels_val == 1L) {
      n <- eff_n_obs
      dat <- data.frame(row_id = seq_len(n))
      cluster_idx <- NULL
    } else {
      n_cl <- eff_n_cl
      cs <- eff_cs
      cs_dist <- spec$hierarchy$cluster_size_dist %||% "fixed"
      min_nj <- max(2L, as.integer(isTRUE(spec$effects$random$intercept)) + length(spec$effects$random$slopes %||% list()) + 1L)
      if (identical(cs_dist, "normal")) {
        csizes <- pmax(min_nj, as.integer(round(stats::rnorm(n_cl, mean = cs, sd = eff_cs_sd))))
        n <- sum(csizes)
        cluster_idx <- rep(seq_len(n_cl), times = csizes)
        wave_idx <- NULL
      } else if (identical(cs_dist, "poisson")) {
        csizes <- pmax(min_nj, stats::rpois(n_cl, lambda = cs))
        n <- sum(csizes)
        cluster_idx <- rep(seq_len(n_cl), times = csizes)
        wave_idx <- NULL
      } else {
        n <- n_cl * cs
        cluster_idx <- rep(seq_len(n_cl), each = cs)
        wave_idx <- if (is_long) rep(seq_len(cs), times = n_cl) else NULL
      }
      dat <- stats::setNames(data.frame(factor(cluster_idx)), spec$hierarchy$cluster_name)
    }

    if (!is.null(spec$predictor_corr) && is.matrix(spec$predictor_corr)) {
      p_lvls <- vapply(spec$predictors, function(pr) as.integer(pr$level), integer(1))
      draw_lvl <- function(lvl_idx, draw_n, expand_cl) {
        k <- length(lvl_idx)
        if (k == 0L) return(invisible(NULL))
        Z <- if (k == 1L) {
          matrix(stats::rnorm(draw_n), nrow = draw_n, ncol = 1L)
        } else {
          R_sub <- spec$predictor_corr[lvl_idx, lvl_idx, drop = FALSE]
          matrix(stats::rnorm(draw_n * k), nrow = draw_n, ncol = k) %*% chol(R_sub)
        }
        for (pos in seq_len(k)) {
          p <- spec$predictors[[lvl_idx[[pos]]]]
          zv <- Z[, pos]
          vals <- switch(
            p$distribution,
            time = as.numeric(spec$hierarchy$time_spacing)[wave_idx],
            normal = p$mean + p$sd * zv,
            binary = as.integer(stats::pnorm(zv) < p$prob),
            uniform = p$min + (p$max - p$min) * stats::pnorm(zv)
          )
          col_v <- if (expand_cl) vals[cluster_idx] else vals
          if (identical(p$centering, "grand_mean")) {
            col_v <- col_v - mean(col_v)
          } else if (identical(p$centering, "group_mean") && levels_val == 2L) {
            col_v <- col_v - stats::ave(col_v, dat[[spec$hierarchy$cluster_name]], FUN = mean)
          }
          dat[[p$name]] <<- col_v
        }
      }
      draw_lvl(which(p_lvls == 1L), n, FALSE)
      if (levels_val == 2L) draw_lvl(which(p_lvls == 2L), eff_n_cl, TRUE)
    } else {
      for (p in spec$predictors) {
        is_l2 <- levels_val == 2L && identical(as.integer(p$level), 2L)
        dn <- if (is_l2) eff_n_cl else n
        vals <- switch(
          p$distribution,
          time = as.numeric(spec$hierarchy$time_spacing)[wave_idx],
          normal = stats::rnorm(dn, mean = p$mean, sd = p$sd),
          binary = stats::rbinom(dn, size = 1L, prob = p$prob),
          uniform = stats::runif(dn, min = p$min, max = p$max)
        )
        col_v <- if (is_l2) vals[cluster_idx] else vals
        if (identical(p$centering, "grand_mean")) {
          col_v <- col_v - mean(col_v)
        } else if (identical(p$centering, "group_mean") && levels_val == 2L) {
          col_v <- col_v - stats::ave(col_v, dat[[spec$hierarchy$cluster_name]], FUN = mean)
        }
        dat[[p$name]] <- col_v
      }
    }

    eta <- rep(as.numeric(spec$effects$intercept), n)
    for (p in spec$predictors) {
      eta <- eta + as.numeric(spec$effects$fixed[[p$name]]) * dat[[p$name]]
    }
    for (it in spec$effects$interactions %||% list()) {
      ct <- canonical_interaction_terms(it$terms, pred_names)
      eta <- eta + as.numeric(it$beta) * dat[[ct[[1L]]]] * dat[[ct[[2L]]]]
    }

    if (levels_val == 2L) {
      has_ri <- isTRUE(spec$effects$random$intercept)
      slopes <- spec$effects$random$slopes %||% list()
      q_r <- as.integer(has_ri) + length(slopes)
      if (q_r > 0L) {
        n_cl <- eff_n_cl
        tau0 <- spec_implied_intercept_sd(spec)
        re_sds <- c(if (has_ri) tau0 else numeric(), vapply(slopes, function(sl) as.numeric(sl$sd), numeric(1)))
        U <- matrix(0, nrow = n_cl, ncol = q_r)
        pos <- which(re_sds > 0)
        if (length(pos) == 1L) {
          U[, pos] <- stats::rnorm(n_cl, 0, re_sds[pos])
        } else if (length(pos) > 1L) {
          cm <- spec$effects$random$corr_matrix %||% diag(q_r)
          D_sub <- diag(re_sds[pos], nrow = length(pos))
          Sigma <- D_sub %*% cm[pos, pos, drop = FALSE] %*% D_sub
          U[, pos] <- matrix(stats::rnorm(n_cl * length(pos)), nrow = n_cl) %*% chol(Sigma)
        }
        col_i <- 1L
        if (has_ri) {
          eta <- eta + U[cluster_idx, 1L]
          col_i <- 2L
        }
        for (sl in slopes) {
          eta <- eta + U[cluster_idx, col_i] * dat[[sl$predictor]]
          col_i <- col_i + 1L
        }
      }
    }

    dat[[y_name]] <- if (fam == "gaussian") {
      edist <- spec$outcome$error_dist %||% "normal"
      rsd <- as.numeric(spec$outcome$residual_sd)
      eps_innov <- if (identical(edist, "t")) {
        df_v <- as.numeric(spec$outcome$error_df)
        rsd * stats::rt(n, df = df_v) * sqrt((df_v - 2) / df_v)
      } else if (identical(edist, "skewed")) {
        sk_v <- as.numeric(spec$outcome$error_skew)
        k_sh <- 4 / (sk_v^2)
        rsd * sign(sk_v) * (stats::rgamma(n, shape = k_sh, rate = 1) - k_sh) / sqrt(k_sh)
      } else {
        stats::rnorm(n, 0, rsd)
      }
      eps <- eps_innov
      rho <- as.numeric(spec$outcome$ar1_rho %||% 0)
      if (is_long && is.finite(rho) && abs(rho) > 0 && eff_cs >= 2L) {
        t_sp <- as.numeric(spec$hierarchy$time_spacing)
        for (s_i in seq_len(eff_n_cl)) {
          idx_s <- ((s_i - 1L) * eff_cs + 1L):(s_i * eff_cs)
          for (w_t in 2L:eff_cs) {
            dt_w <- if (rho >= 0) t_sp[w_t] - t_sp[w_t - 1L] else 1
            phi_w <- if (rho >= 0) rho^dt_w else rho
            eps[idx_s[w_t]] <- phi_w * eps[idx_s[w_t - 1L]] + sqrt(1 - phi_w^2) * eps_innov[idx_s[w_t]]
          }
        }
      }
      eta + eps
    } else if (fam == "binomial" && lnk == "logit") {
      stats::rbinom(n, 1L, stats::plogis(eta))
    } else if (fam == "binomial" && lnk == "probit") {
      stats::rbinom(n, 1L, stats::pnorm(eta))
    } else {
      stats::rpois(n, exp(pmin(eta, 15)))
    }

    if (identical(spec$missingness$mechanism, "MCAR") && spec$missingness$rate > 0) {
      dat[[y_name]][stats::rbinom(n, 1L, spec$missingness$rate) == 1L] <- NA
    } else if (identical(spec$missingness$mechanism, "MAR") && spec$missingness$rate > 0) {
      ax <- as.numeric(dat[[spec$missingness$auxiliary]])
      asd <- stats::sd(ax)
      az <- if (is.finite(asd) && asd > 0) (ax - mean(ax)) / asd else rep(0, n)
      g <- as.numeric(spec$missingness$strength)
      tr <- as.numeric(spec$missingness$rate)
      a0 <- tryCatch(
        stats::uniroot(function(a) mean(stats::plogis(a + g * az)) - tr, c(-25, 25))$root,
        error = function(e) stats::qlogis(tr)
      )
      dat[[y_name]][stats::rbinom(n, 1L, stats::plogis(a0 + g * az)) == 1L] <- NA
    } else if (identical(spec$missingness$mechanism, "monotone_dropout") && spec$missingness$rate > 0 && is_long) {
      tr <- as.numeric(spec$missingness$rate)
      h_base <- 1 - (1 - tr)^(1 / (eff_cs - 1L))
      if (!is.null(spec$missingness$auxiliary)) {
        base_idx <- seq.int(1L, n, by = eff_cs)
        ax_s <- as.numeric(dat[[spec$missingness$auxiliary]][base_idx])
        asd_s <- stats::sd(ax_s)
        az_s <- if (is.finite(asd_s) && asd_s > 0) (ax_s - mean(ax_s)) / asd_s else rep(0, eff_n_cl)
        g <- as.numeric(spec$missingness$strength %||% 1.0)
        a0 <- tryCatch(
          stats::uniroot(function(a) mean(1 - (1 - stats::plogis(a + g * az_s))^(eff_cs - 1L)) - tr, c(-25, 25))$root,
          error = function(e) stats::qlogis(h_base)
        )
        h_vec <- stats::plogis(a0 + g * az_s)
      } else {
        h_vec <- rep(h_base, eff_n_cl)
      }
      for (s_i in seq_len(eff_n_cl)) {
        idx_s <- ((s_i - 1L) * eff_cs + 1L):(s_i * eff_cs)
        dropped <- FALSE
        for (w_t in 2L:eff_cs) {
          if (!dropped && stats::rbinom(1L, 1L, h_vec[s_i]) == 1L) dropped <- TRUE
          if (dropped) dat[[y_name]][idx_s[w_t]] <- NA
        }
      }
    }

    dat_cc <- stats::na.omit(dat)
    tryCatch({
      conv_ok <- TRUE
      if (engine == "lm") {
        fit <- stats::lm(form, data = dat_cc)
        sm <- stats::coef(summary(fit))
        est <- unname(sm[focal, 1L]); se <- unname(sm[focal, 2L]); pv <- unname(sm[focal, 4L])
        crit <- stats::qt(1 - (1 - spec$estimator$conf_level) / 2, df = fit$df.residual)
      } else if (engine == "glm") {
        fam_fn <- switch(fam, gaussian = stats::gaussian, binomial = stats::binomial, poisson = stats::poisson)
        fit <- stats::glm(form, data = dat_cc, family = fam_fn(link = lnk))
        sm <- stats::coef(summary(fit))
        est <- unname(sm[focal, 1L]); se <- unname(sm[focal, 2L]); pv <- unname(sm[focal, 4L])
        crit <- stats::qnorm(1 - (1 - spec$estimator$conf_level) / 2)
        conv_ok <- isTRUE(fit$converged)
      } else if (engine == "lmer") {
        fit <- suppressMessages(lme4::lmer(form, data = dat_cc, REML = isTRUE(spec$estimator$reml)))
        sm <- stats::coef(summary(fit))
        est <- unname(sm[focal, 1L]); se <- unname(sm[focal, 2L]); pv <- 2 * stats::pnorm(-abs(unname(sm[focal, 3L])))
        crit <- stats::qnorm(1 - (1 - spec$estimator$conf_level) / 2)
        opt_code <- fit@optinfo$conv$opt
        opt_msgs <- fit@optinfo$conv$lme4$messages
        opt_ok <- (is.null(opt_code) || isTRUE(opt_code == 0)) && (is.null(opt_msgs) || length(opt_msgs) == 0L)
        conv_ok <- !lme4::isSingular(fit) && opt_ok
      } else {
        fam_fn <- switch(fam, gaussian = stats::gaussian, binomial = stats::binomial, poisson = stats::poisson)
        fit <- suppressMessages(lme4::glmer(form, data = dat_cc, family = fam_fn(link = lnk)))
        sm <- stats::coef(summary(fit))
        est <- unname(sm[focal, 1L]); se <- unname(sm[focal, 2L]); pv <- unname(sm[focal, 4L])
        crit <- stats::qnorm(1 - (1 - spec$estimator$conf_level) / 2)
        opt_code <- fit@optinfo$conv$opt
        opt_msgs <- fit@optinfo$conv$lme4$messages
        opt_ok <- (is.null(opt_code) || isTRUE(opt_code == 0)) && (is.null(opt_msgs) || length(opt_msgs) == 0L)
        conv_ok <- !lme4::isSingular(fit) && opt_ok
      }
      list(
        converged = conv_ok && is.finite(est) && is.finite(se) && se > 0,
        est = est, se = se,
        reject = is.finite(pv) && pv < spec$simulation$alpha,
        covered = is.finite(est) && is.finite(se) && (true_beta >= est - crit * se) && (true_beta <= est + crit * se)
      )
    }, error = function(e) {
      list(converged = FALSE, est = NA_real_, se = NA_real_, reject = NA, covered = NA)
    })
  }

  t0 <- Sys.time()
  reps <- list()
  for (i in seq_len(n_reps)) {
    reps[[i]] <- run_one()
    elapsed_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (i < n_reps && elapsed_sec >= as.numeric(max_seconds)) {
      warns <- c(
        warns,
        sprintf("Preview stopped early at %d/%d replications after exceeding the %.1fs interactive time budget.", i, n_reps, as.numeric(max_seconds))
      )
      break
    }
  }

  n_done <- length(reps)
  conv <- vapply(reps, `[[`, logical(1), "converged")
  ok <- which(conv)
  ests <- vapply(reps, `[[`, numeric(1), "est")[ok]
  ses <- vapply(reps, `[[`, numeric(1), "se")[ok]
  rejs <- vapply(reps, `[[`, logical(1), "reject")[ok]
  covs <- vapply(reps, `[[`, logical(1), "covered")[ok]

  summ_df <- data.frame(
    n_reps = n_done,
    n_converged = length(ok),
    true_effect = true_beta,
    mean_est = if (length(ok) > 0L) round(mean(ests), 4) else NA_real_,
    bias = if (length(ok) > 0L) round(mean(ests) - true_beta, 4) else NA_real_,
    mean_se = if (length(ok) > 0L) round(mean(ses), 4) else NA_real_,
    power = if (length(ok) > 0L) round(mean(rejs), 4) else NA_real_,
    coverage = if (length(ok) > 0L) round(mean(covs), 4) else NA_real_,
    stringsAsFactors = FALSE
  )

  list(summary = summ_df, warnings = warns)
}
