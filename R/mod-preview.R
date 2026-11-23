#' Live Specification & Deterministic Codegen Preview Shiny Module (UI)
#'
#' Displays a live multi-tab `bslib` inspector for a validated `spec_ir`:
#' mathematical model summary, serialized YAML/JSON IR, deterministic standalone
#' R script (with optional fast preview execution), and Quarto `.qmd` report,
#' each with download handlers.
#'
#' @param id Module namespace ID.
#' @return A `bslib::navset_card_tab()` UI component.
#' @export
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
          label = "Run Quick Preview (10 reps)",
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
#' @param id Module namespace ID.
#' @param spec_rv A `shiny::reactiveVal` holding the current validated `spec_ir`.
#' @param last_diff_rv Optional `shiny::reactiveVal` holding the latest `spec_diff`.
#'
#' @return A list of reactive expressions: `math`, `yaml`, `json`, `r_script`,
#'   `quarto_report`, and `preview_results`.
#' @export
mod_spec_preview_server <- function(id, spec_rv, last_diff_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    preview_rv <- shiny::reactiveVal(NULL)

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

    # Clear stale preview table whenever spec changes
    shiny::observeEvent(spec_rv(), {
      preview_rv(NULL)
    })

    # Safe pure-R preview simulation (zero eval/parse)
    shiny::observeEvent(input$run_preview, {
      sp <- shiny::isolate(spec_rv())
      if (is.null(sp)) return()
      res <- tryCatch(
        run_spec_preview_sim(sp, max_reps = 10L),
        error = function(e) e
      )
      preview_rv(res)
    })

    output$summary_ui <- shiny::renderUI({
      sp <- spec_rv()
      shiny::req(sp)
      m <- math_r()
      desc <- spec_to_controls(sp)
      focal <- sp$effects$focal_predictor %||% sp$predictors[[1L]]$name
      focal_b <- sp$effects$fixed[[focal]]

      htmltools::tagList(
        htmltools::tags$div(
          class = "d-flex flex-wrap gap-2 mb-3",
          htmltools::tags$span(class = "badge text-bg-success", "Schema & Guardrails: PASS"),
          htmltools::tags$span(class = "badge text-bg-primary", paste("Design:", sp$design_type)),
          htmltools::tags$span(class = "badge text-bg-secondary", paste("Total N:", spec_total_n(sp))),
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
      if (is.null(res)) return(NULL)
      if (inherits(res, "error")) {
        return(htmltools::tags$div(class = "alert alert-danger small py-2", conditionMessage(res)))
      }
      tbl_lines <- utils::capture.output(print(res, row.names = FALSE))
      htmltools::tags$div(
        class = "alert alert-success py-2 px-3 small mb-2",
        htmltools::tags$strong("Quick Preview Simulation (first grid cell, up to 10 reps):"),
        htmltools::tags$pre(class = "mb-0 mt-1", paste(tbl_lines, collapse = "\n"))
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
      preview_results = preview_rv
    )
  })
}

# Pure-R bounded preview simulator (zero eval/parse) for responsive Shiny previews
run_spec_preview_sim <- function(spec, max_reps = 10L) {
  assert_valid_spec_ir(spec)
  engine <- spec$estimator$engine
  if (engine %in% c("lmer", "glmer") && !requireNamespace("lme4", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg lme4} is required to run multilevel preview simulations.")
  }

  n_reps <- min(as.integer(spec$simulation$n_reps), as.integer(max_reps))
  levels_val <- as.integer(spec$hierarchy$levels)
  y_name <- spec$outcome$name
  fam <- spec$outcome$family
  lnk <- spec$outcome$link
  focal <- spec$effects$focal_predictor %||% spec$predictors[[1L]]$name
  true_beta <- as.numeric(spec$effects$fixed[[focal]])
  form <- stats::as.formula(spec_to_math(spec)$formula)

  set.seed(as.integer(spec$simulation$seed), kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")

  run_one <- function() {
    if (levels_val == 1L) {
      n <- as.integer(spec$hierarchy$n_obs)
      dat <- data.frame(row_id = seq_len(n))
      cluster_idx <- NULL
    } else {
      n_cl <- as.integer(spec$hierarchy$n_clusters)
      cs <- as.integer(spec$hierarchy$cluster_size)
      n <- n_cl * cs
      cluster_idx <- rep(seq_len(n_cl), each = cs)
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
      if (levels_val == 2L) draw_lvl(which(p_lvls == 2L), as.integer(spec$hierarchy$n_clusters), TRUE)
    } else {
      for (p in spec$predictors) {
        is_l2 <- levels_val == 2L && identical(as.integer(p$level), 2L)
        dn <- if (is_l2) as.integer(spec$hierarchy$n_clusters) else n
        vals <- switch(
          p$distribution,
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

    if (levels_val == 2L) {
      has_ri <- isTRUE(spec$effects$random$intercept)
      slopes <- spec$effects$random$slopes %||% list()
      q_r <- as.integer(has_ri) + length(slopes)
      if (q_r > 0L) {
        n_cl <- as.integer(spec$hierarchy$n_clusters)
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
      stats::rnorm(n, eta, as.numeric(spec$outcome$residual_sd))
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
        stats::uniroot(function(a) mean(stats::plogis(a + g * az)) - tr, c(-20, 20))$root,
        error = function(e) stats::qlogis(tr)
      )
      dat[[y_name]][stats::rbinom(n, 1L, stats::plogis(a0 + g * az)) == 1L] <- NA
    }

    dat_cc <- stats::na.omit(dat)
    tryCatch({
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
      } else if (engine == "lmer") {
        fit <- suppressMessages(lme4::lmer(form, data = dat_cc, REML = isTRUE(spec$estimator$reml)))
        sm <- stats::coef(summary(fit))
        est <- unname(sm[focal, 1L]); se <- unname(sm[focal, 2L]); pv <- 2 * stats::pnorm(-abs(unname(sm[focal, 3L])))
        crit <- stats::qnorm(1 - (1 - spec$estimator$conf_level) / 2)
      } else {
        fam_fn <- switch(fam, gaussian = stats::gaussian, binomial = stats::binomial, poisson = stats::poisson)
        fit <- suppressMessages(lme4::glmer(form, data = dat_cc, family = fam_fn(link = lnk)))
        sm <- stats::coef(summary(fit))
        est <- unname(sm[focal, 1L]); se <- unname(sm[focal, 2L]); pv <- unname(sm[focal, 4L])
        crit <- stats::qnorm(1 - (1 - spec$estimator$conf_level) / 2)
      }
      list(
        converged = is.finite(est) && is.finite(se) && se > 0,
        est = est, se = se,
        reject = is.finite(pv) && pv < spec$simulation$alpha,
        covered = is.finite(est) && is.finite(se) && (true_beta >= est - crit * se) && (true_beta <= est + crit * se)
      )
    }, error = function(e) {
      list(converged = FALSE, est = NA_real_, se = NA_real_, reject = NA, covered = NA)
    })
  }

  reps <- lapply(seq_len(n_reps), function(i) run_one())
  conv <- vapply(reps, `[[`, logical(1), "converged")
  ok <- which(conv)
  ests <- vapply(reps, `[[`, numeric(1), "est")[ok]
  ses <- vapply(reps, `[[`, numeric(1), "se")[ok]
  rejs <- vapply(reps, `[[`, logical(1), "reject")[ok]
  covs <- vapply(reps, `[[`, logical(1), "covered")[ok]

  data.frame(
    n_reps = n_reps,
    n_converged = length(ok),
    true_effect = true_beta,
    mean_est = if (length(ok) > 0L) round(mean(ests), 4) else NA_real_,
    bias = if (length(ok) > 0L) round(mean(ests) - true_beta, 4) else NA_real_,
    mean_se = if (length(ok) > 0L) round(mean(ses), 4) else NA_real_,
    power = if (length(ok) > 0L) round(mean(rejs), 4) else NA_real_,
    coverage = if (length(ok) > 0L) round(mean(covs), 4) else NA_real_,
    stringsAsFactors = FALSE
  )
}
