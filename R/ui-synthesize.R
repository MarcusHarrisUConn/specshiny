#' Synthesize a minimal dynamic UI descriptor from a validated `spec_ir`
#'
#' Pure function mapping a validated `spec_ir` into an explicit UI descriptor
#' containing **only** the sections and input controls relevant to that exact
#' specification. For example, a single-level Gaussian complete-data spec emits
#' no cluster count, ICC, random-slope, missingness-rate, or REML controls.
#'
#' @param spec A validated `spec_ir` object.
#' @return An S3 object of class `spec_ui_descriptor` containing:
#'   - `sections`: named list of section descriptors, each holding a list of control descriptors.
#'   - `control_ids`: character vector of all synthesized control IDs.
#'   - `control_count`: integer number of active controls.
#'   - `math_summary`: output of [spec_to_math()].
#'   - `structure_key`: deterministic structural signature for diffing UI topology.
#'   - `available_grid_axes`: character vector of valid sweepable grid axes for this design.
#' @export
#' @examples
#' s1 <- new_spec_ir(design_type = "monte_carlo_lm", hierarchy = list(levels = 1L, n_obs = 150L))
#' d1 <- spec_to_controls(s1)
#' d1$control_ids
#'
#' s2 <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   hierarchy = list(levels = 2L, n_clusters = 50L, cluster_size = 20L, icc = 0.2)
#' )
#' d2 <- spec_to_controls(s2)
#' d2$control_ids
spec_to_controls <- function(spec) {
  assert_valid_spec_ir(spec)

  levels_val <- as.integer(spec$hierarchy$levels)
  sections <- list()

  make_ctrl <- function(id, path, label, type, value, min = NULL, max = NULL,
                        step = NULL, choices = NULL, help = NULL) {
    list(
      id = id,
      path = path,
      label = label,
      type = type,
      value = value,
      min = min,
      max = max,
      step = step,
      choices = choices,
      help = help
    )
  }

  # 1. Hierarchy section
  if (levels_val == 1L) {
    hier_ctrls <- list(
      make_ctrl(
        id = "n_obs",
        path = "hierarchy.n_obs",
        label = sprintf("Sample size (N %ss)", spec$hierarchy$unit_name),
        type = "numeric",
        value = as.integer(spec$hierarchy$n_obs),
        min = 5L, max = 50000L, step = 10L
      )
    )
    hier_sub <- "Single-level independent observations"
  } else {
    hier_ctrls <- list(
      make_ctrl(
        id = "n_clusters",
        path = "hierarchy.n_clusters",
        label = sprintf("Number of %ss (J)", spec$hierarchy$cluster_name),
        type = "numeric",
        value = as.integer(spec$hierarchy$n_clusters),
        min = 3L, max = 1000L, step = 5L
      ),
      make_ctrl(
        id = "cluster_size",
        path = "hierarchy.cluster_size",
        label = sprintf("%ss per %s (n_j)", tools::toTitleCase(spec$hierarchy$unit_name), spec$hierarchy$cluster_name),
        type = "numeric",
        value = as.integer(spec$hierarchy$cluster_size),
        min = 2L, max = 500L, step = 1L
      ),
      make_ctrl(
        id = "icc",
        path = "hierarchy.icc",
        label = "Intraclass Correlation (ICC)",
        type = "slider",
        value = as.numeric(spec$hierarchy$icc),
        min = 0, max = 0.90, step = 0.01
      )
    )
    hier_sub <- sprintf("2-level nested structure (total N = %d)", spec_total_n(spec))
  }
  sections$hierarchy <- list(
    id = "hierarchy",
    title = "Data Hierarchy",
    subtitle = hier_sub,
    controls = hier_ctrls
  )

  # 2. Outcome section
  valid_links <- switch(
    spec$outcome$family,
    gaussian = "identity",
    binomial = c("logit", "probit"),
    poisson = "log"
  )
  out_ctrls <- list(
    make_ctrl(
      id = "outcome_family",
      path = "outcome.family",
      label = sprintf("Outcome family (%s)", spec$outcome$name),
      type = "select",
      value = spec$outcome$family,
      choices = c("gaussian", "binomial", "poisson")
    )
  )
  if (length(valid_links) > 1L) {
    out_ctrls[[length(out_ctrls) + 1L]] <- make_ctrl(
      id = "outcome_link",
      path = "outcome.link",
      label = "Link function",
      type = "select",
      value = spec$outcome$link,
      choices = valid_links
    )
  }
  if (identical(spec$outcome$family, "gaussian")) {
    out_ctrls[[length(out_ctrls) + 1L]] <- make_ctrl(
      id = "residual_sd",
      path = "outcome.residual_sd",
      label = "Level-1 Residual SD (sigma)",
      type = "numeric",
      value = as.numeric(spec$outcome$residual_sd),
      min = 0.05, max = 100, step = 0.1
    )
  }
  sections$outcome <- list(
    id = "outcome",
    title = "Outcome Model",
    subtitle = sprintf("%s (%s, %s link)", spec$outcome$name, spec$outcome$family, spec$outcome$link),
    controls = out_ctrls
  )

  # 3. Predictors & Fixed Effects section (only active predictors & their distribution params)
  pred_ctrls <- list()
  pred_names <- vapply(spec$predictors, `[[`, character(1), "name")
  for (p in spec$predictors) {
    pn <- p$name
    b_val <- as.numeric(spec$effects$fixed[[pn]] %||% 0)
    pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
      id = paste0("pred_beta_", pn),
      path = paste0("effects.fixed.", pn),
      label = sprintf("Fixed effect beta (%s, L%d %s)", pn, as.integer(p$level), p$distribution),
      type = "numeric",
      value = b_val,
      min = -10, max = 10, step = 0.05
    )

    if (identical(p$distribution, "normal")) {
      pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
        id = paste0("pred_sd_", pn),
        path = paste0("predictors.", pn, ".sd"),
        label = sprintf("SD of %s", pn),
        type = "numeric",
        value = as.numeric(p$sd),
        min = 0.05, max = 100, step = 0.1
      )
    } else if (identical(p$distribution, "binary")) {
      pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
        id = paste0("pred_prob_", pn),
        path = paste0("predictors.", pn, ".prob"),
        label = sprintf("Prevalence P(%s = 1)", pn),
        type = "slider",
        value = as.numeric(p$prob),
        min = 0.05, max = 0.95, step = 0.05
      )
    } else if (identical(p$distribution, "uniform")) {
      pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
        id = paste0("pred_min_", pn),
        path = paste0("predictors.", pn, ".min"),
        label = sprintf("Min of %s", pn),
        type = "numeric",
        value = as.numeric(p$min),
        min = -1000, max = 1000, step = 0.5
      )
      pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
        id = paste0("pred_max_", pn),
        path = paste0("predictors.", pn, ".max"),
        label = sprintf("Max of %s", pn),
        type = "numeric",
        value = as.numeric(p$max),
        min = -1000, max = 1000, step = 0.5
      )
    }

    cent_choices <- if (levels_val == 2L && identical(as.integer(p$level), 1L)) {
      c("none", "grand_mean", "group_mean")
    } else {
      c("none", "grand_mean")
    }
    pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
      id = paste0("pred_centering_", pn),
      path = paste0("predictors.", pn, ".centering"),
      label = sprintf("Centering (%s)", pn),
      type = "select",
      value = p$centering,
      choices = cent_choices
    )
  }
  if (!is.null(spec$predictor_corr) && is.matrix(spec$predictor_corr) && length(pred_names) >= 2L) {
    pcm <- spec$predictor_corr
    p_lvls <- vapply(spec$predictors, function(pr) as.integer(pr$level), integer(1))
    for (i in seq_len(length(pred_names) - 1L)) {
      for (j in seq.int(i + 1L, length(pred_names))) {
        if (p_lvls[[i]] == p_lvls[[j]]) {
          pred_ctrls[[length(pred_ctrls) + 1L]] <- make_ctrl(
            id = sprintf("pcorr_%d_%d", i, j),
            path = "predictor_corr",
            label = sprintf("Predictor Corr (%s, %s)", pred_names[[i]], pred_names[[j]]),
            type = "slider",
            value = as.numeric(pcm[i, j]),
            min = -0.95, max = 0.95, step = 0.05
          )
        }
      }
    }
  }
  sections$predictors <- list(
    id = "predictors",
    title = "Predictors & Fixed Effects",
    subtitle = sprintf("%d active predictor%s", length(pred_names), if (length(pred_names) == 1L) "" else "s"),
    controls = pred_ctrls
  )

  # 4. Effects (Intercept, Focal Target, and Random Effects ONLY if 2-level)
  eff_ctrls <- list(
    make_ctrl(
      id = "effects_intercept",
      path = "effects.intercept",
      label = "Model Intercept (beta_0)",
      type = "numeric",
      value = as.numeric(spec$effects$intercept),
      min = -50, max = 50, step = 0.1
    )
  )
  if (length(pred_names) > 1L) {
    eff_ctrls[[length(eff_ctrls) + 1L]] <- make_ctrl(
      id = "focal_predictor",
      path = "effects.focal_predictor",
      label = "Focal Predictor (Power/Bias Target)",
      type = "select",
      value = spec$effects$focal_predictor %||% pred_names[[1L]],
      choices = pred_names
    )
  }

  if (levels_val == 2L) {
    slopes <- spec$effects$random$slopes %||% list()
    for (sl in slopes) {
      eff_ctrls[[length(eff_ctrls) + 1L]] <- make_ctrl(
        id = paste0("slope_sd_", sl$predictor),
        path = paste0("effects.random.slopes.", sl$predictor, ".sd"),
        label = sprintf("Random slope SD (%s)", sl$predictor),
        type = "numeric",
        value = as.numeric(sl$sd),
        min = 0.01, max = 5, step = 0.05
      )
    }

    re_names <- c(
      if (isTRUE(spec$effects$random$intercept)) "intercept" else character(),
      vapply(slopes, `[[`, character(1), "predictor")
    )
    q_rand <- length(re_names)
    if (q_rand >= 2L) {
      cm <- spec$effects$random$corr_matrix %||% diag(q_rand)
      for (i in seq_len(q_rand - 1L)) {
        for (j in seq.int(i + 1L, q_rand)) {
          eff_ctrls[[length(eff_ctrls) + 1L]] <- make_ctrl(
            id = sprintf("corr_%d_%d", i, j),
            path = "effects.random.corr_matrix",
            label = sprintf("RE Correlation (%s, %s)", re_names[[i]], re_names[[j]]),
            type = "slider",
            value = as.numeric(cm[i, j]),
            min = -0.95, max = 0.95, step = 0.05
          )
        }
      }
    }
  }
  sections$effects <- list(
    id = "effects",
    title = if (levels_val == 2L) "Fixed & Random Effects" else "Model Effects",
    subtitle = if (levels_val == 2L) {
      sprintf("Random intercept%s", if (length(spec$effects$random$slopes) > 0L) sprintf(" + %d random slope(s)", length(spec$effects$random$slopes)) else " only")
    } else {
      "Single-level fixed effects"
    },
    controls = eff_ctrls
  )

  # 5. Missingness section (conditional on mechanism)
  miss_mech <- spec$missingness$mechanism
  miss_ctrls <- list(
    make_ctrl(
      id = "missing_mechanism",
      path = "missingness.mechanism",
      label = "Missingness Mechanism",
      type = "select",
      value = miss_mech,
      choices = c("none", "MCAR", "MAR")
    )
  )
  if (!identical(miss_mech, "none")) {
    miss_ctrls[[length(miss_ctrls) + 1L]] <- make_ctrl(
      id = "missing_rate",
      path = "missingness.rate",
      label = "Target Missingness Proportion",
      type = "slider",
      value = as.numeric(spec$missingness$rate),
      min = 0.01, max = 0.85, step = 0.01
    )
  }
  if (identical(miss_mech, "MAR")) {
    miss_ctrls[[length(miss_ctrls) + 1L]] <- make_ctrl(
      id = "missing_auxiliary",
      path = "missingness.auxiliary",
      label = "MAR Auxiliary Predictor",
      type = "select",
      value = spec$missingness$auxiliary,
      choices = pred_names
    )
    miss_ctrls[[length(miss_ctrls) + 1L]] <- make_ctrl(
      id = "missing_strength",
      path = "missingness.strength",
      label = "MAR Log-Odds Slope",
      type = "slider",
      value = as.numeric(spec$missingness$strength),
      min = -3, max = 3, step = 0.1
    )
  }
  sections$missingness <- list(
    id = "missingness",
    title = "Missingness",
    subtitle = if (identical(miss_mech, "none")) "Complete data" else sprintf("%s (%.0f%%)", miss_mech, 100 * spec$missingness$rate),
    controls = miss_ctrls
  )

  # 6. Simulation & Estimator section
  sim_ctrls <- list(
    make_ctrl(
      id = "n_reps",
      path = "simulation.n_reps",
      label = "Monte Carlo Replications",
      type = "numeric",
      value = as.integer(spec$simulation$n_reps),
      min = 1L, max = 100000L, step = 50L
    ),
    make_ctrl(
      id = "alpha",
      path = "simulation.alpha",
      label = "Significance Level (alpha)",
      type = "numeric",
      value = as.numeric(spec$simulation$alpha),
      min = 0.001, max = 0.25, step = 0.005
    ),
    make_ctrl(
      id = "seed",
      path = "simulation.seed",
      label = "Random Seed",
      type = "numeric",
      value = as.integer(spec$simulation$seed),
      min = 1L, max = 9999999L, step = 1L
    )
  )
  if (identical(spec$estimator$engine, "lmer")) {
    sim_ctrls[[length(sim_ctrls) + 1L]] <- make_ctrl(
      id = "estimator_reml",
      path = "estimator.reml",
      label = "Use REML estimation (lmer)",
      type = "checkbox",
      value = isTRUE(spec$estimator$reml)
    )
  }
  pg <- spec$simulation$parameter_grid %||% list()
  for (gn in names(pg)) {
    sim_ctrls[[length(sim_ctrls) + 1L]] <- make_ctrl(
      id = paste0("grid_", gn),
      path = paste0("simulation.parameter_grid.", gn),
      label = sprintf("Grid Sweep: %s (comma-separated)", gn),
      type = "text",
      value = paste(pg[[gn]], collapse = ", "),
      help = "Enter comma-separated numeric values"
    )
  }

  avail_grid <- available_grid_axes_for_spec(spec)
  sections$simulation <- list(
    id = "simulation",
    title = "Simulation & Estimator",
    subtitle = sprintf("Engine: %s | %d grid axis(es)", spec$estimator$engine, length(pg)),
    controls = sim_ctrls
  )

  all_ids <- unlist(lapply(sections, function(s) vapply(s$controls, `[[`, character(1), "id")), use.names = FALSE)

  structure(
    list(
      sections = sections,
      control_ids = all_ids,
      control_count = length(all_ids),
      math_summary = spec_to_math(spec),
      structure_key = paste(all_ids, collapse = "|"),
      available_grid_axes = avail_grid
    ),
    class = "spec_ui_descriptor"
  )
}

#' @export
print.spec_ui_descriptor <- function(x, ...) {
  cli::cli_h3("spec_ui_descriptor ({x$control_count} active controls across {length(x$sections)} sections)")
  for (sec in x$sections) {
    ids <- vapply(sec$controls, `[[`, character(1), "id")
    cli::cli_bullets(c("*" = sprintf("%s (%d): %s", sec$title, length(ids), paste(ids, collapse = ", "))))
  }
  invisible(x)
}

available_grid_axes_for_spec <- function(spec) {
  levels_val <- as.integer(spec$hierarchy$levels)
  axes <- character()
  if (levels_val == 1L) {
    axes <- c(axes, "n_obs")
  } else {
    axes <- c(axes, "n_clusters", "cluster_size", "icc")
  }
  if (!is.null(spec$effects$focal_predictor)) {
    axes <- c(axes, "focal_effect")
  }
  if (identical(spec$outcome$family, "gaussian")) {
    axes <- c(axes, "residual_sd")
  }
  if (!identical(spec$missingness$mechanism, "none")) {
    axes <- c(axes, "missing_rate")
  }
  axes
}

#' Apply a single dynamic UI control edit to a `spec_ir` with validation
#'
#' Pure helper powering UI-to-IR synchronization in [mod_spec_controls_server()].
#' Never calls `eval()` or `parse()`; all values are coerced deterministically
#' and checked against [validate_spec_ir()].
#'
#' @param spec Current valid `spec_ir` object.
#' @param control_id Character scalar ID matching a control from [spec_to_controls()].
#' @param value New input value from Shiny.
#'
#' @return A list containing `spec` (candidate `spec_ir`), `changed` (logical),
#'   `diff` (`spec_diff`), and `validation` (`spec_validation`).
#' @export
#' @examples
#' spec <- new_spec_ir(design_type = "multilevel_sim",
#'   hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2))
#' res <- patch_spec_from_control(spec, "n_clusters", 75)
#' res$validation$valid
#' res$diff
patch_spec_from_control <- function(spec, control_id, value) {
  old_spec <- spec
  s <- spec

  if (is.null(value)) {
    return(list(
      spec = old_spec,
      changed = FALSE,
      diff = diff_spec_ir(old_spec, old_spec),
      validation = validate_spec_ir(old_spec, strict = FALSE)
    ))
  }

  if (control_id == "n_obs") {
    s$hierarchy$n_obs <- maybe_int(suppressWarnings(as.numeric(value)))
  } else if (control_id == "n_clusters") {
    s$hierarchy$n_clusters <- maybe_int(suppressWarnings(as.numeric(value)))
  } else if (control_id == "cluster_size") {
    s$hierarchy$cluster_size <- maybe_int(suppressWarnings(as.numeric(value)))
  } else if (control_id == "icc") {
    s$hierarchy$icc <- suppressWarnings(as.numeric(value))
    s$effects$random$intercept_sd <- NULL
  } else if (control_id == "outcome_family") {
    fam <- as.character(value)
    s$outcome$family <- fam
    s$outcome$link <- switch(fam, gaussian = "identity", binomial = "logit", poisson = "log", s$outcome$link)
    if (fam == "gaussian") {
      if (is.null(s$outcome$residual_sd)) s$outcome$residual_sd <- 1.0
    } else {
      s$outcome$residual_sd <- NULL
      s$simulation$parameter_grid$residual_sd <- NULL
    }
    q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
    s$estimator$engine <- infer_default_engine(as.integer(s$hierarchy$levels), fam, q_r)
  } else if (control_id == "outcome_link") {
    s$outcome$link <- as.character(value)
  } else if (control_id == "residual_sd") {
    s$outcome$residual_sd <- suppressWarnings(as.numeric(value))
  } else if (control_id == "effects_intercept") {
    s$effects$intercept <- suppressWarnings(as.numeric(value))
  } else if (control_id == "focal_predictor") {
    s$effects$focal_predictor <- as.character(value)
  } else if (control_id == "missing_mechanism") {
    mech <- as.character(value)
    s$missingness$mechanism <- mech
    if (mech == "none") {
      s$missingness$rate <- 0
      s$missingness$auxiliary <- NULL
      s$missingness$strength <- NULL
      s$simulation$parameter_grid$missing_rate <- NULL
    } else {
      if (!is_num_scalar(s$missingness$rate) || s$missingness$rate <= 0) {
        s$missingness$rate <- 0.15
      }
      if (mech == "MAR") {
        if (is.null(s$missingness$auxiliary)) s$missingness$auxiliary <- s$predictors[[1L]]$name
        if (is.null(s$missingness$strength) || s$missingness$strength == 0) s$missingness$strength <- 1.0
      } else {
        s$missingness$auxiliary <- NULL
        s$missingness$strength <- NULL
      }
    }
  } else if (control_id == "missing_rate") {
    s$missingness$rate <- suppressWarnings(as.numeric(value))
  } else if (control_id == "missing_auxiliary") {
    s$missingness$auxiliary <- as.character(value)
  } else if (control_id == "missing_strength") {
    s$missingness$strength <- suppressWarnings(as.numeric(value))
  } else if (control_id == "n_reps") {
    s$simulation$n_reps <- maybe_int(suppressWarnings(as.numeric(value)))
  } else if (control_id == "alpha") {
    a <- suppressWarnings(as.numeric(value))
    s$simulation$alpha <- a
    if (is_num_scalar(a) && a > 0 && a < 0.5) {
      s$estimator$conf_level <- 1 - a
    }
  } else if (control_id == "seed") {
    s$simulation$seed <- maybe_int(suppressWarnings(as.numeric(value)))
  } else if (control_id == "estimator_reml") {
    s$estimator$reml <- isTRUE(value)
  } else if (grepl("^pred_beta_", control_id)) {
    pn <- sub("^pred_beta_", "", control_id)
    s$effects$fixed[[pn]] <- suppressWarnings(as.numeric(value))
  } else if (grepl("^pred_(sd|prob|min|max|centering)_", control_id)) {
    m <- reg_capture("^pred_(sd|prob|min|max|centering)_(.+)$", control_id)
    field <- m[[1L]]
    pn <- m[[2L]]
    for (i in seq_along(s$predictors)) {
      if (identical(s$predictors[[i]]$name, pn)) {
        s$predictors[[i]][[field]] <- if (field == "centering") as.character(value) else suppressWarnings(as.numeric(value))
      }
    }
  } else if (grepl("^slope_sd_", control_id)) {
    pn <- sub("^slope_sd_", "", control_id)
    for (i in seq_along(s$effects$random$slopes)) {
      if (identical(s$effects$random$slopes[[i]]$predictor, pn)) {
        s$effects$random$slopes[[i]]$sd <- suppressWarnings(as.numeric(value))
      }
    }
  } else if (grepl("^corr_\\d+_\\d+$", control_id)) {
    m <- reg_capture("^corr_(\\d+)_(\\d+)$", control_id)
    r_i <- as.integer(m[[1L]])
    c_j <- as.integer(m[[2L]])
    q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
    cm <- s$effects$random$corr_matrix %||% diag(q_r)
    rho <- suppressWarnings(as.numeric(value))
    if (is.matrix(cm) && r_i <= nrow(cm) && c_j <= ncol(cm)) {
      cm[r_i, c_j] <- rho
      cm[c_j, r_i] <- rho
      s$effects$random$corr_matrix <- cm
    }
  } else if (grepl("^pcorr_\\d+_\\d+$", control_id)) {
    m <- reg_capture("^pcorr_(\\d+)_(\\d+)$", control_id)
    r_i <- as.integer(m[[1L]])
    c_j <- as.integer(m[[2L]])
    p_len <- length(s$predictors)
    pcm <- s$predictor_corr %||% diag(p_len)
    rho <- suppressWarnings(as.numeric(value))
    if (is.matrix(pcm) && r_i <= nrow(pcm) && c_j <= ncol(pcm) && length(rho) == 1L) {
      pcm[r_i, c_j] <- rho
      pcm[c_j, r_i] <- rho
      s$predictor_corr <- pcm
    }
  } else if (grepl("^grid_", control_id)) {
    gn <- sub("^grid_", "", control_id)
    raw_str <- trimws(as.character(value))
    if (!nzchar(raw_str)) {
      s$simulation$parameter_grid[[gn]] <- NULL
    } else {
      parts <- trimws(strsplit(raw_str, ",", fixed = TRUE)[[1L]])
      nums <- suppressWarnings(as.numeric(parts))
      s$simulation$parameter_grid[[gn]] <- nums
    }
  }

  candidate <- as_spec_ir(s, validate = FALSE)
  diff_obj <- diff_spec_ir(old_spec, candidate)
  val_rep <- validate_spec_ir(candidate, strict = FALSE)

  list(
    spec = candidate,
    changed = diff_obj$has_changes,
    diff = diff_obj,
    validation = val_rep
  )
}

#' Render a `spec_ui_descriptor` into `bslib` Shiny UI cards
#'
#' @param descriptor A `spec_ui_descriptor` returned by [spec_to_controls()].
#' @param ns Namespace function (typically `session$ns` inside a module).
#' @param diff Optional `spec_diff` object used to highlight changed sections
#'   and controls with visual diff badges.
#'
#' @return An `htmltools::tagList` of `bslib::card()` elements.
#' @export
render_spec_controls_ui <- function(descriptor, ns = identity, diff = NULL) {
  if (!inherits(descriptor, "spec_ui_descriptor")) {
    cli::cli_abort("{.arg descriptor} must be a {.cls spec_ui_descriptor}.")
  }

  changed_paths <- if (inherits(diff, "spec_diff") && diff$has_changes) diff$changed_paths else character()
  changed_sections <- if (inherits(diff, "spec_diff") && diff$has_changes) diff$changed_sections else character()

  cards <- lapply(descriptor$sections, function(sec) {
    sec_changed <- sec$id %in% changed_sections ||
      any(vapply(sec$controls, function(ct) ct$path %in% changed_paths, logical(1)))

    sec_badge <- if (sec_changed) {
      htmltools::tags$span(class = "badge rounded-pill text-bg-success ms-2", "Updated")
    } else {
      NULL
    }

    ctrl_tags <- lapply(sec$controls, function(ct) {
      is_ct_changed <- ct$path %in% changed_paths
      lbl_node <- if (is_ct_changed) {
        htmltools::tagList(
          ct$label,
          htmltools::tags$span(class = "badge text-bg-info ms-1", style = "font-size: 0.7em;", "changed")
        )
      } else {
        ct$label
      }

      input_widget <- switch(
        ct$type,
        numeric = shiny::numericInput(
          inputId = ns(ct$id), label = lbl_node,
          value = ct$value, min = ct$min, max = ct$max, step = ct$step
        ),
        slider = shiny::sliderInput(
          inputId = ns(ct$id), label = lbl_node,
          min = ct$min, max = ct$max, value = ct$value, step = ct$step
        ),
        select = shiny::selectInput(
          inputId = ns(ct$id), label = lbl_node,
          choices = ct$choices, selected = ct$value
        ),
        checkbox = shiny::checkboxInput(
          inputId = ns(ct$id), label = lbl_node, value = isTRUE(ct$value)
        ),
        text = shiny::textInput(
          inputId = ns(ct$id), label = lbl_node, value = as.character(ct$value %||% "")
        )
      )

      wrapper_cls <- if (is_ct_changed) "p-2 rounded border border-info bg-light-subtle mb-2" else "mb-2"
      htmltools::tags$div(class = wrapper_cls, input_widget)
    })

    bslib::card(
      class = if (sec_changed) "border-success mb-3" else "mb-3",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center py-2",
        htmltools::tags$span(
          htmltools::tags$strong(sec$title),
          sec_badge
        ),
        htmltools::tags$small(class = "text-muted", sec$subtitle)
      ),
      bslib::card_body(
        class = "py-2",
        do.call(
          bslib::layout_columns,
          c(list(col_widths = if (length(ctrl_tags) <= 1L) 12 else 6), ctrl_tags)
        )
      )
    )
  })

  htmltools::tagList(cards)
}

#' Render visual diff badges for a `spec_diff` object
#'
#' @param diff A `spec_diff` object (or `NULL`).
#' @return An `htmltools` tag representing the live change diff banner.
#' @export
render_diff_badges_ui <- function(diff) {
  if (!inherits(diff, "spec_diff") || !diff$has_changes) {
    return(
      htmltools::tags$div(
        class = "alert alert-light border py-2 px-3 mb-3 small d-flex align-items-center gap-2",
        htmltools::tags$span(class = "badge text-bg-secondary", "Synced"),
        htmltools::tags$span(class = "text-muted", "Specification IR and dynamic controls are synchronized.")
      )
    )
  }

  badges <- lapply(seq_len(nrow(diff$changes)), function(i) {
    row <- diff$changes[i, , drop = FALSE]
    bg <- switch(
      row$change_type,
      added = "text-bg-success",
      removed = "text-bg-danger",
      modified = "text-bg-primary"
    )
    htmltools::tags$span(
      class = paste("badge rounded-pill me-1 mb-1", bg),
      row$summary
    )
  })

  htmltools::tags$div(
    class = "alert alert-info py-2 px-3 mb-3",
    htmltools::tags$div(
      class = "fw-semibold small mb-1",
      sprintf("Latest Sync Diff (%d change%s):", diff$n_changes, if (diff$n_changes == 1L) "" else "s")
    ),
    htmltools::tags$div(badges)
  )
}
