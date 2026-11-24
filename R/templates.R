# Package-private mutable registry for user-registered custom templates
.spec_template_registry <- new.env(parent = emptyenv())

builtin_spec_templates <- function() {
  list(
    cluster_rct = list(
      name = "cluster_rct",
      source = "builtin",
      category = "experimental",
      description = "2-level cluster-randomized trial (60 schools x 25 students, ICC 0.20, level-2 treatment + grand-mean centered pretest).",
      builder = function() {
        new_spec_ir(
          title = "Cluster-Randomized Controlled Trial (Schools)",
          description = "2-level CRT with school-level randomization and student-level pretest covariate.",
          design_type = "power_analysis",
          hierarchy = list(
            levels = 2L,
            n_clusters = 60L,
            cluster_size = 25L,
            cluster_size_dist = "fixed",
            cluster_name = "school",
            unit_name = "student",
            icc = 0.20
          ),
          predictors = list(
            spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5, role = "treatment"),
            spec_predictor("pretest", level = 1L, distribution = "normal", mean = 0, sd = 1, centering = "grand_mean", role = "covariate")
          ),
          outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1.0, error_dist = "normal"),
          effects = list(
            intercept = 0,
            fixed = list(treatment = 0.35, pretest = 0.50),
            focal_predictor = "treatment",
            random = list(intercept = TRUE, slopes = list())
          ),
          missingness = list(mechanism = "none", rate = 0),
          simulation = list(n_reps = 500L, seed = 2026L, alpha = 0.05)
        )
      }
    ),
    cross_level_mlm = list(
      name = "cross_level_mlm",
      source = "builtin",
      category = "multilevel",
      description = "2-level cross-level interaction (x1:z1) with random slope for x1, unbalanced normal cluster sizes, and Student-t errors (df = 5).",
      builder = function() {
        new_spec_ir(
          title = "Cross-Level Interaction MLM (Unbalanced + Heavy Tails)",
          description = "2-level random-slope model testing an x1:z1 cross-level interaction under unbalanced clusters and Student-t errors.",
          design_type = "two_level_mlm",
          hierarchy = list(
            levels = 2L,
            n_clusters = 50L,
            cluster_size = 20L,
            cluster_size_dist = "normal",
            cluster_size_sd = 5,
            cluster_name = "school",
            unit_name = "student",
            icc = 0.20
          ),
          predictors = list(
            spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, centering = "group_mean", role = "covariate"),
            spec_predictor("z1", level = 2L, distribution = "normal", mean = 0, sd = 1, centering = "grand_mean", role = "covariate")
          ),
          outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1.0, error_dist = "t", error_df = 5),
          effects = list(
            intercept = 0,
            fixed = list(x1 = 0.20, z1 = 0.25),
            interactions = list(spec_interaction("x1", "z1", beta = 0.30)),
            focal_predictor = "x1:z1",
            random = list(
              intercept = TRUE,
              slopes = list(list(predictor = "x1", sd = 0.25)),
              corr_matrix = matrix(c(1, 0.2, 0.2, 1), nrow = 2L)
            )
          ),
          missingness = list(mechanism = "none", rate = 0),
          simulation = list(n_reps = 300L, seed = 2026L, alpha = 0.05)
        )
      }
    ),
    longitudinal_growth_ar1 = list(
      name = "longitudinal_growth_ar1",
      source = "builtin",
      category = "longitudinal",
      description = "5-wave longitudinal growth curve (120 subjects, ICC 0.40, AR(1) rho = 0.35, random time slope, time:treatment interaction, 25% monotone dropout).",
      builder = function() {
        new_spec_ir(
          title = "Longitudinal Growth Curve with AR(1) & Monotone Dropout",
          description = "Repeated-measures linear growth model comparing treatment vs. control trajectories under AR(1) errors and monotone attrition.",
          design_type = "longitudinal_growth",
          hierarchy = list(
            levels = 2L,
            n_clusters = 120L,
            time_points = 5L,
            cluster_size = 5L,
            time_spacing = c(0, 1, 2, 3, 4),
            cluster_size_dist = "fixed",
            cluster_name = "subject",
            unit_name = "wave",
            icc = 0.40
          ),
          predictors = list(
            spec_predictor("time", level = 1L, distribution = "time", centering = "none", role = "covariate"),
            spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5, centering = "none", role = "treatment")
          ),
          outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1.0, error_dist = "normal", ar1_rho = 0.35),
          effects = list(
            intercept = 0,
            fixed = list(time = 0.30, treatment = 0.15),
            interactions = list(spec_interaction("time", "treatment", beta = 0.25)),
            focal_predictor = "time:treatment",
            random = list(
              intercept = TRUE,
              slopes = list(list(predictor = "time", sd = 0.25)),
              corr_matrix = matrix(c(1, 0.1, 0.1, 1), nrow = 2L)
            )
          ),
          missingness = list(mechanism = "monotone_dropout", rate = 0.25),
          simulation = list(n_reps = 300L, seed = 2026L, alpha = 0.05)
        )
      }
    ),
    logistic_power = list(
      name = "logistic_power",
      source = "builtin",
      category = "glm",
      description = "2-level binary logistic mixed model (50 clinics x 20 patients, ICC 0.15, logit link, treatment OR = exp(0.60), 10% MCAR missingness).",
      builder = function() {
        new_spec_ir(
          title = "2-Level Logistic Mixed-Model Power Study",
          description = "Clustered binary outcome study evaluating a treatment effect on log-odds with MCAR missingness.",
          design_type = "power_analysis",
          hierarchy = list(
            levels = 2L,
            n_clusters = 50L,
            cluster_size = 20L,
            cluster_size_dist = "fixed",
            cluster_name = "clinic",
            unit_name = "patient",
            icc = 0.15
          ),
          predictors = list(
            spec_predictor("treatment", level = 1L, distribution = "binary", prob = 0.5, role = "treatment"),
            spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "covariate")
          ),
          outcome = list(name = "y", family = "binomial", link = "logit"),
          effects = list(
            intercept = -0.2,
            fixed = list(treatment = 0.60, x1 = 0.35),
            focal_predictor = "treatment",
            random = list(intercept = TRUE, slopes = list())
          ),
          missingness = list(mechanism = "MCAR", rate = 0.10),
          simulation = list(n_reps = 250L, seed = 2026L, alpha = 0.05)
        )
      }
    ),
    factorial_ols = list(
      name = "factorial_ols",
      source = "builtin",
      category = "experimental",
      description = "Single-level 2x2 factorial linear regression (N = 240, focal treatment:moderator interaction beta = 0.45).",
      builder = function() {
        new_spec_ir(
          title = "2x2 Factorial Linear Model (OLS)",
          description = "Single-level randomized 2x2 factorial design powering the treatment-by-moderator interaction.",
          design_type = "monte_carlo_lm",
          hierarchy = list(levels = 1L, n_obs = 240L, unit_name = "participant"),
          predictors = list(
            spec_predictor("treatment", level = 1L, distribution = "binary", prob = 0.5, role = "treatment"),
            spec_predictor("moderator", level = 1L, distribution = "binary", prob = 0.5, role = "covariate")
          ),
          outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1.0, error_dist = "normal"),
          effects = list(
            intercept = 0,
            fixed = list(treatment = 0.25, moderator = 0.10),
            interactions = list(spec_interaction("treatment", "moderator", beta = 0.45)),
            focal_predictor = "treatment:moderator",
            random = list(intercept = FALSE, slopes = list())
          ),
          missingness = list(mechanism = "none", rate = 0),
          simulation = list(n_reps = 500L, seed = 2026L, alpha = 0.05)
        )
      }
    )
  )
}

#' List available built-in and custom `spec_ir` study templates
#'
#' @param category Optional character filter (e.g., `"experimental"`,
#'   `"multilevel"`, `"longitudinal"`, `"glm"`, `"custom"`).
#'
#' @return A `data.frame` summarizing registered templates with columns `name`,
#'   `source`, `category`, `design_type`, `levels`, `estimator`, and
#'   `description`.
#' @export
#' @examples
#' list_spec_templates()
#' list_spec_templates(category = "longitudinal")
list_spec_templates <- function(category = NULL) {
  if (!is.null(category) && !is_chr_scalar(category)) {
    cli::cli_abort("{.arg category} must be NULL or a non-empty character scalar.")
  }

  builtins <- builtin_spec_templates()
  entries <- lapply(builtins, function(b) {
    sp <- b$builder()
    data.frame(
      name = b$name,
      source = "builtin",
      category = b$category,
      design_type = as.character(sp$design_type),
      levels = as.integer(sp$hierarchy$levels),
      estimator = as.character(sp$estimator$engine),
      description = b$description,
      stringsAsFactors = FALSE
    )
  })

  custom_names <- sort(ls(envir = .spec_template_registry, all.names = FALSE))
  for (cn in custom_names) {
    rec <- get(cn, envir = .spec_template_registry, inherits = FALSE)
    sp <- rec$spec
    entries[[length(entries) + 1L]] <- data.frame(
      name = rec$name,
      source = "custom",
      category = rec$category,
      design_type = as.character(sp$design_type),
      levels = as.integer(sp$hierarchy$levels),
      estimator = as.character(sp$estimator$engine),
      description = rec$description,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, entries)
  rownames(out) <- NULL

  if (!is.null(category)) {
    out <- out[tolower(out$category) == tolower(category), , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

#' Register a validated custom `spec_ir` template
#'
#' Registers a reusable domain-specific study template in the session template
#' registry. Every candidate template is strictly gated by both
#' [assert_valid_spec_ir()] and [verify_compiled_r_ast()] on its compiled R
#' script before registration succeeds, ensuring no invalid or unsafe template
#' can enter the registry. Built-in templates cannot be overwritten.
#'
#' @param name Character scalar template name (must be a valid ASCII R
#'   identifier matching `^[A-Za-z][A-Za-z0-9_]*$`).
#' @param spec A valid `spec_ir` object.
#' @param description Optional plain-language template description.
#' @param category Character scalar category label (default `"custom"`).
#' @param overwrite Logical; whether to replace an existing custom template of
#'   the same name (default `FALSE`).
#'
#' @return Invisibly returns a metadata list (`name`, `category`, `description`,
#'   `source`, `spec`).
#' @export
#' @examples
#' my_spec <- new_spec_ir(
#'   title = "Small Pilot OLS",
#'   design_type = "monte_carlo_lm",
#'   hierarchy = list(levels = 1L, n_obs = 80L)
#' )
#' register_spec_template("pilot_ols_ex", my_spec, description = "80-unit OLS pilot")
#' s <- use_spec_template("pilot_ols_ex", n_obs = 120L)
#' s$hierarchy$n_obs
#' unregister_spec_template("pilot_ols_ex")
register_spec_template <- function(name,
                                   spec,
                                   description = NULL,
                                   category = "custom",
                                   overwrite = FALSE) {
  if (!is_valid_r_ident(name)) {
    cli::cli_abort("Template {.arg name} must be a valid ASCII identifier (e.g., {.val my_cluster_rct}).")
  }
  if (!is_chr_scalar(category) || !nzchar(trimws(category))) {
    cli::cli_abort("{.arg category} must be a non-empty character scalar.")
  }
  if (!is_lgl_scalar(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be TRUE or FALSE.")
  }

  builtins <- builtin_spec_templates()
  if (name %in% names(builtins)) {
    cli::cli_abort("Cannot overwrite built-in template {.val {name}}.")
  }
  if (exists(name, envir = .spec_template_registry, inherits = FALSE) && !isTRUE(overwrite)) {
    cli::cli_abort("Custom template {.val {name}} already exists. Set {.code overwrite = TRUE} to replace it.")
  }

  if (!is_spec_ir(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls spec_ir} object.")
  }
  assert_valid_spec_ir(spec)

  # Security gate: compiled R script must pass strict AST verification
  compiled_r <- compile_r_script(spec)
  verify_compiled_r_ast(compiled_r, strict = TRUE)

  desc_txt <- if (is.null(description)) {
    spec$metadata$description %||% spec$metadata$title %||% sprintf("Custom %s template", spec$design_type)
  } else {
    if (!is_chr_scalar(description)) {
      cli::cli_abort("{.arg description} must be NULL or a character scalar.")
    }
    description
  }

  record <- list(
    name = name,
    source = "custom",
    category = trimws(category),
    description = desc_txt,
    spec = spec
  )
  assign(name, record, envir = .spec_template_registry)
  invisible(record)
}

#' Unregister a custom `spec_ir` template
#'
#' Removes a user-registered template from the session registry. Built-in
#' templates are immutable and cannot be unregistered.
#'
#' @param name Character scalar name of the custom template to remove.
#'
#' @return Invisibly returns `TRUE` upon successful removal.
#' @export
#' @examples
#' my_spec <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
#' register_spec_template("temp_tpl_ex", my_spec)
#' unregister_spec_template("temp_tpl_ex")
unregister_spec_template <- function(name) {
  if (!is_chr_scalar(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a non-empty character scalar.")
  }
  builtins <- builtin_spec_templates()
  if (name %in% names(builtins)) {
    cli::cli_abort("Cannot unregister built-in template {.val {name}}.")
  }
  if (!exists(name, envir = .spec_template_registry, inherits = FALSE)) {
    cli::cli_abort("Custom template {.val {name}} is not registered.")
  }
  rm(list = name, envir = .spec_template_registry)
  invisible(TRUE)
}

#' Instantiate a `spec_ir` from a registered template with optional overrides
#'
#' Loads a built-in or custom template by name, optionally applies parameter
#' overrides (by UI control ID, shorthand parameter name, or dotted IR path)
#' and/or a natural-language refinement instruction via [refine_spec()], and
#' returns the validated `spec_ir`.
#'
#' @param name Character scalar name of a template listed by
#'   [list_spec_templates()].
#' @param overrides Optional named list of parameter overrides (e.g.,
#'   `list(n_clusters = 80L, icc = 0.25)` or
#'   `list("outcome.ar1_rho" = 0.5, "simulation.n_reps" = 100L)`).
#' @param refine Optional character scalar natural-language refinement
#'   instruction applied via [refine_spec()] after `overrides`.
#' @param ... Additional named overrides merged with `overrides`.
#'
#' @return A validated `spec_ir` object.
#' @export
#' @examples
#' s <- use_spec_template("longitudinal_growth_ar1", n_clusters = 80L, ar1_rho = 0.5)
#' s$hierarchy$n_clusters
#' s$outcome$ar1_rho
use_spec_template <- function(name, overrides = list(), refine = NULL, ...) {
  if (!is_chr_scalar(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a non-empty character scalar.")
  }
  if (!is.list(overrides)) {
    cli::cli_abort("{.arg overrides} must be a named list.")
  }

  builtins <- builtin_spec_templates()
  if (exists(name, envir = .spec_template_registry, inherits = FALSE)) {
    spec <- get(name, envir = .spec_template_registry, inherits = FALSE)$spec
  } else if (name %in% names(builtins)) {
    spec <- builtins[[name]]$builder()
  } else {
    avail <- c(names(builtins), sort(ls(envir = .spec_template_registry, all.names = FALSE)))
    cli::cli_abort(
      "Unknown template {.val {name}}. Available templates: {.val {avail}}."
    )
  }

  extra <- list(...)
  if (length(extra) > 0L) {
    if (is.null(names(extra)) || any(!nzchar(names(extra)))) {
      cli::cli_abort("All {.code ...} overrides passed to {.fn use_spec_template} must be named.")
    }
    for (ek in names(extra)) {
      overrides[[ek]] <- extra[[ek]]
    }
  }

  if (length(overrides) > 0L) {
    if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
      cli::cli_abort("All entries in {.arg overrides} must be named.")
    }
    for (k in names(overrides)) {
      val <- overrides[[k]]
      spec <- apply_template_override(spec, k, val)
    }
  }

  if (!is.null(refine)) {
    if (!is_chr_scalar(refine) || !nzchar(trimws(refine))) {
      cli::cli_abort("{.arg refine} must be NULL or a non-empty character scalar.")
    }
    spec <- refine_spec(spec, refine, validate = TRUE)$spec
  }

  spec <- as_spec_ir(spec, validate = TRUE)
  spec
}

apply_template_override <- function(spec, key, value) {
  desc <- spec_to_controls(spec)
  if (key %in% desc$control_ids) {
    res <- patch_spec_from_control(spec, key, value)
    if (!res$validation$valid) {
      abort_spec_validation(res$validation)
    }
    return(res$spec)
  }

  s <- spec
  if (identical(key, "title") || identical(key, "metadata.title")) {
    s$metadata$title <- as.character(value)
    return(as_spec_ir(s, validate = FALSE))
  }
  if (identical(key, "description") || identical(key, "metadata.description")) {
    s$metadata$description <- as.character(value)
    return(as_spec_ir(s, validate = FALSE))
  }
  if (identical(key, "parameter_grid") || identical(key, "simulation.parameter_grid")) {
    s$simulation$parameter_grid <- normalize_parameter_grid(value)
    return(as_spec_ir(s, validate = FALSE))
  }
  if (identical(key, "time_spacing") || identical(key, "hierarchy.time_spacing")) {
    ts_v <- as.numeric(value)
    s$hierarchy$time_spacing <- ts_v
    s$hierarchy$time_points <- as.integer(length(ts_v))
    s$hierarchy$cluster_size <- as.integer(length(ts_v))
    return(as_spec_ir(s, validate = FALSE))
  }

  # Map dotted IR path to corresponding control ID if available
  for (sec in desc$sections) {
    for (ct in sec$controls) {
      if (identical(ct$path, key)) {
        res <- patch_spec_from_control(spec, ct$id, value)
        if (!res$validation$valid) {
          abort_spec_validation(res$validation)
        }
        return(res$spec)
      }
    }
  }

  # Fallback: generic dotted path setter for valid nested fields
  if (grepl(".", key, fixed = TRUE)) {
    parts <- strsplit(key, ".", fixed = TRUE)[[1L]]
    if (length(parts) == 2L && parts[[1L]] %in% names(s) && is.list(s[[parts[[1L]]]])) {
      s[[parts[[1L]]]][[parts[[2L]]]] <- value
      if (identical(key, "hierarchy.time_points") && is_whole_scalar(value)) {
        tp <- as.integer(value)
        s$hierarchy$cluster_size <- tp
        s$hierarchy$time_spacing <- as.numeric(seq.int(0L, tp - 1L))
      }
      return(as_spec_ir(s, validate = FALSE))
    }
    if (length(parts) == 3L && identical(parts[[1L]], "effects") && identical(parts[[2L]], "fixed")) {
      s$effects$fixed[[parts[[3L]]]] <- as.numeric(value)
      return(as_spec_ir(s, validate = FALSE))
    }
  }

  cli::cli_abort("Unrecognized template override {.val {key}} for template design {.val {spec$design_type}}.")
}
