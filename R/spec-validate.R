SPEC_ALLOWED_GRID_PARAMS <- c(
  "n_obs", "n_clusters", "cluster_size", "icc",
  "focal_effect", "residual_sd", "missing_rate", "ar1_rho"
)

SPEC_MAX_GRID_CELLS <- 250L

# Closed nested key allowlists (rejects unexpected LLM-injected properties)
SPEC_ALLOWED_KEYS <- list(
  top = c(
    "schema_version", "metadata", "design_type", "hierarchy", "predictors",
    "predictor_corr", "outcome", "effects", "missingness", "estimator",
    "simulation", "defaults_rationale"
  ),
  metadata = c("title", "description", "source_prompt", "tags"),
  hierarchy = c("levels", "n_obs", "n_clusters", "cluster_size", "cluster_size_dist", "cluster_size_sd", "time_points", "time_spacing", "cluster_name", "unit_name", "icc"),
  predictor = c("name", "level", "distribution", "mean", "sd", "prob", "min", "max", "centering", "role"),
  outcome = c("name", "family", "link", "residual_sd", "error_dist", "error_df", "error_skew", "ar1_rho"),
  effects = c("intercept", "fixed", "interactions", "focal_predictor", "random"),
  interaction = c("terms", "beta"),
  random = c("intercept", "intercept_sd", "slopes", "corr_matrix"),
  slope = c("predictor", "sd"),
  missingness = c("mechanism", "rate", "target", "auxiliary", "strength"),
  estimator = c("engine", "reml", "conf_level", "missing_handling"),
  simulation = c("n_reps", "seed", "alpha", "parameter_grid")
)

#' Validate a `spec_ir` object against schema and statistical sanity guardrails
#'
#' Performs pure-R structural schema validation (`category = "schema"`) and
#' statistical sanity and identifiability checks (`category = "statistical"`),
#' including strict ASCII identifier allowlists, closed nested object keys
#' (rejecting unknown extra fields from LLM JSON), positive variances and sample
#' sizes, ICC bounds, random-slope identifiability, positive-definite predictor
#' and random-effects correlation matrices via eigenvalue checks, missingness
#' bounds, estimator-family compatibility, and parameter grid bounds.
#'
#' @param spec A `spec_ir` object (or list) to validate.
#' @param strict Logical; if `TRUE`, aborts with a formatted `cli` error when
#'   validation fails. Defaults to `FALSE`, returning a `spec_validation` report.
#'
#' @return An S3 object of class `spec_validation` with elements:
#'   - `valid`: logical scalar (`TRUE` when `nrow(errors) == 0L`).
#'   - `errors`: data.frame with columns `path`, `code`, `category`, `message`.
#'   - `warnings`: data.frame with columns `path`, `code`, `category`, `message`.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' validate_spec_ir(spec)
validate_spec_ir <- function(spec, strict = FALSE) {
  err_list <- list()
  warn_list <- list()

  add_err <- function(path, code, category, message) {
    err_list[[length(err_list) + 1L]] <<- list(
      path = path,
      code = code,
      category = category,
      message = message
    )
  }

  add_warn <- function(path, code, category, message) {
    warn_list[[length(warn_list) + 1L]] <<- list(
      path = path,
      code = code,
      category = category,
      message = message
    )
  }

  check_extra_keys <- function(obj, allowed, path_prefix) {
    if (!is.list(obj)) return(invisible(NULL))
    nms <- names(obj) %||% character()
    extras <- setdiff(nms, allowed)
    for (ek in extras) {
      add_err(
        paste0(path_prefix, ".", ek),
        "unknown_nested_field",
        "schema",
        sprintf("Unknown field '%s' inside '%s' is not permitted by IR schema v%s.", ek, path_prefix, SPEC_SCHEMA_VERSION)
      )
    }
  }

  check_corr_matrix <- function(cm, expected_dim, path_name) {
    if (!is.matrix(cm) || !is.numeric(cm)) {
      add_err(path_name, "invalid_corr_matrix_type", "schema",
              sprintf("%s must be a numeric matrix.", path_name))
      return(FALSE)
    }
    if (nrow(cm) != expected_dim || ncol(cm) != expected_dim) {
      add_err(path_name, "corr_matrix_dim_mismatch", "statistical",
              sprintf("%s must be %dx%d (got %dx%d).", path_name, expected_dim, expected_dim, nrow(cm), ncol(cm)))
      return(FALSE)
    }
    if (any(!is.finite(cm))) {
      add_err(path_name, "corr_matrix_non_finite", "statistical",
              sprintf("%s contains non-finite values.", path_name))
      return(FALSE)
    }
    ok <- TRUE
    if (max(abs(cm - t(cm))) > 1e-8) {
      add_err(path_name, "corr_matrix_not_symmetric", "statistical",
              sprintf("%s must be symmetric.", path_name))
      ok <- FALSE
    }
    if (max(abs(diag(cm) - 1)) > 1e-8) {
      add_err(path_name, "corr_matrix_unit_diagonal", "statistical",
              sprintf("%s must have all diagonal entries equal to 1.", path_name))
      ok <- FALSE
    }
    if (any(cm < -1 - 1e-8 | cm > 1 + 1e-8)) {
      add_err(path_name, "corr_matrix_out_of_bounds", "statistical",
              sprintf("%s entries must lie in [-1, 1].", path_name))
      ok <- FALSE
    }
    eig_vals <- eigen(0.5 * (cm + t(cm)), symmetric = TRUE, only.values = TRUE)$values
    if (min(eig_vals) <= 1e-8) {
      add_err(path_name, "corr_matrix_not_positive_definite", "statistical",
              sprintf("%s is not positive definite (minimum eigenvalue = %.6g).", path_name, min(eig_vals)))
      ok <- FALSE
    }
    ok
  }

  build_report <- function() {
    to_df <- function(items) {
      if (length(items) == 0L) {
        return(data.frame(
          path = character(),
          code = character(),
          category = character(),
          message = character(),
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        path = vapply(items, `[[`, character(1), "path"),
        code = vapply(items, `[[`, character(1), "code"),
        category = vapply(items, `[[`, character(1), "category"),
        message = vapply(items, `[[`, character(1), "message"),
        stringsAsFactors = FALSE
      )
    }

    errs <- to_df(err_list)
    warns <- to_df(warn_list)
    res <- structure(
      list(
        valid = nrow(errs) == 0L,
        errors = errs,
        warnings = warns
      ),
      class = "spec_validation"
    )
    if (isTRUE(strict) && !res$valid) {
      abort_spec_validation(res)
    }
    res
  }

  # 1. Top-level structure checks
  if (!is.list(spec)) {
    add_err("$", "not_a_list", "schema", "Specification must be a named list or spec_ir object.")
    return(build_report())
  }

  required_top <- c(
    "schema_version", "metadata", "design_type", "hierarchy", "predictors",
    "outcome", "effects", "missingness", "estimator", "simulation", "defaults_rationale"
  )
  missing_top <- setdiff(required_top, names(spec))
  for (k in missing_top) {
    add_err(k, "missing_required_field", "schema", sprintf("Missing required top-level field '%s'.", k))
  }
  extra_top <- setdiff(names(spec), SPEC_ALLOWED_KEYS$top)
  for (k in extra_top) {
    add_err(k, "unknown_top_level_field", "schema",
            sprintf("Unknown top-level field '%s' is not part of IR schema v%s.", k, SPEC_SCHEMA_VERSION))
  }

  if (length(missing_top) > 0L) {
    return(build_report())
  }

  # 2. Schema version & metadata
  if (!is_chr_scalar(spec$schema_version) || !(spec$schema_version %in% SPEC_SUPPORTED_SCHEMA_VERSIONS)) {
    ver_hint <- if (is_chr_scalar(spec$schema_version) && grepl("^\\d+\\.\\d+\\.\\d+$", spec$schema_version)) {
      if (utils::compareVersion(spec$schema_version, SPEC_SCHEMA_VERSION) > 0L) {
        sprintf("schema_version '%s' is newer than supported '%s'; upgrade specshiny.", spec$schema_version, SPEC_SCHEMA_VERSION)
      } else {
        sprintf("schema_version '%s' is unsupported; supported versions: %s.", spec$schema_version, paste(SPEC_SUPPORTED_SCHEMA_VERSIONS, collapse = ", "))
      }
    } else {
      sprintf("schema_version must be one of: %s.", paste(SPEC_SUPPORTED_SCHEMA_VERSIONS, collapse = ", "))
    }
    add_err("schema_version", "unsupported_schema_version", "schema", ver_hint)
  } else if (!identical(spec$schema_version, SPEC_SCHEMA_VERSION)) {
    add_warn(
      "schema_version", "schema_outdated", "schema",
      sprintf("Spec uses legacy schema_version '%s'; call migrate_spec_ir() to upgrade to '%s'.",
              spec$schema_version, SPEC_SCHEMA_VERSION)
    )
  }

  if (!is.list(spec$metadata)) {
    add_err("metadata", "invalid_type", "schema", "metadata must be a named list.")
  } else {
    check_extra_keys(spec$metadata, SPEC_ALLOWED_KEYS$metadata, "metadata")
    if (!is_chr_scalar(spec$metadata$title) || !nzchar(trimws(spec$metadata$title)) || nchar(spec$metadata$title) > 300L) {
      add_err("metadata.title", "invalid_title", "schema", "metadata$title must be a non-empty character scalar (<= 300 chars).")
    }
    if (!is_chr_scalar(spec$metadata$description) || nchar(spec$metadata$description) > 2000L) {
      add_err("metadata.description", "invalid_description", "schema", "metadata$description must be a character scalar (<= 2000 chars).")
    }
    if (!is.null(spec$metadata$source_prompt) && (!is_chr_scalar(spec$metadata$source_prompt) || nchar(spec$metadata$source_prompt) > 2000L)) {
      add_err("metadata.source_prompt", "invalid_source_prompt", "schema", "metadata$source_prompt must be NULL or a character scalar (<= 2000 chars).")
    }
    if (!is.null(spec$metadata$tags) && !is.character(spec$metadata$tags)) {
      add_err("metadata.tags", "invalid_tags", "schema", "metadata$tags must be a character vector.")
    }
  }

  # 3. Design type
  design_ok <- is_chr_scalar(spec$design_type) && spec$design_type %in% spec_design_types()
  if (!design_ok) {
    add_err(
      "design_type", "invalid_design_type", "schema",
      sprintf("design_type must be one of: %s.", paste(spec_design_types(), collapse = ", "))
    )
  }

  # 4. Hierarchy
  levels_val <- NA_integer_
  n_clusters_val <- NA_integer_
  cluster_size_val <- NA_integer_
  icc_val <- NA_real_
  cluster_name_val <- NULL
  unit_name_val <- NULL

  if (!is.list(spec$hierarchy)) {
    add_err("hierarchy", "invalid_type", "schema", "hierarchy must be a named list.")
  } else {
    h <- spec$hierarchy
    check_extra_keys(h, SPEC_ALLOWED_KEYS$hierarchy, "hierarchy")
    if (!is_whole_scalar(h$levels) || !(as.integer(h$levels) %in% c(1L, 2L))) {
      add_err("hierarchy.levels", "invalid_levels", "schema", "hierarchy$levels must be integer 1 or 2.")
    } else {
      levels_val <- as.integer(h$levels)
    }

    if (!is_valid_r_ident(h$unit_name)) {
      add_err("hierarchy.unit_name", "invalid_identifier", "schema",
              "hierarchy$unit_name must be a valid pure-ASCII non-reserved R identifier (<= 64 chars).")
    } else {
      unit_name_val <- h$unit_name
    }

    if (design_ok && !is.na(levels_val)) {
      if (spec$design_type == "monte_carlo_lm" && levels_val != 1L) {
        add_err("hierarchy.levels", "design_level_mismatch", "statistical",
                "design_type 'monte_carlo_lm' requires hierarchy$levels == 1.")
      }
      if (spec$design_type %in% c("multilevel_sim", "two_level_mlm", "longitudinal_growth") && levels_val != 2L) {
        add_err("hierarchy.levels", "design_level_mismatch", "statistical",
                sprintf("design_type '%s' requires hierarchy$levels == 2.", spec$design_type))
      }
    }

    if (identical(levels_val, 1L)) {
      if (!is_whole_scalar(h$n_obs) || as.integer(h$n_obs) < 5L) {
        add_err("hierarchy.n_obs", "invalid_sample_size", "statistical",
                "1-level design requires integer hierarchy$n_obs >= 5.")
      }
      if (!is.null(h$icc) && (!is_num_scalar(h$icc) || h$icc != 0)) {
        add_err("hierarchy.icc", "icc_in_single_level", "statistical",
                "1-level design cannot specify a non-zero hierarchy$icc.")
      }
      if (!is.null(h$cluster_size_dist) && !identical(h$cluster_size_dist, "fixed")) {
        add_err("hierarchy.cluster_size_dist", "unbalanced_in_single_level", "statistical",
                "1-level design cannot specify unbalanced cluster_size_dist.")
      }
      if (!is.null(h$cluster_size_sd)) {
        add_err("hierarchy.cluster_size_sd", "unbalanced_in_single_level", "statistical",
                "1-level design cannot specify hierarchy$cluster_size_sd.")
      }
    } else if (identical(levels_val, 2L)) {
      if (!is_whole_scalar(h$n_clusters) || as.integer(h$n_clusters) < 3L) {
        add_err("hierarchy.n_clusters", "invalid_n_clusters", "statistical",
                "2-level design requires integer hierarchy$n_clusters >= 3.")
      } else {
        n_clusters_val <- as.integer(h$n_clusters)
      }
      if (!is_whole_scalar(h$cluster_size) || as.integer(h$cluster_size) < 2L) {
        add_err("hierarchy.cluster_size", "invalid_cluster_size", "statistical",
                "2-level design requires integer hierarchy$cluster_size >= 2.")
      } else {
        cluster_size_val <- as.integer(h$cluster_size)
      }
      cs_dist <- h$cluster_size_dist %||% "fixed"
      if (!is_chr_scalar(cs_dist) || !(cs_dist %in% c("fixed", "normal", "poisson"))) {
        add_err("hierarchy.cluster_size_dist", "invalid_cluster_size_dist", "schema",
                "hierarchy$cluster_size_dist must be 'fixed', 'normal', or 'poisson'.")
      } else if (cs_dist == "normal") {
        if (!is_num_scalar(h$cluster_size_sd) || h$cluster_size_sd <= 0) {
          add_err("hierarchy.cluster_size_sd", "invalid_cluster_size_sd", "statistical",
                  "Unbalanced 'normal' cluster sizes require finite hierarchy$cluster_size_sd > 0.")
        } else if (!is.na(cluster_size_val) && h$cluster_size_sd >= 5 * cluster_size_val) {
          add_err("hierarchy.cluster_size_sd", "cluster_size_sd_excessive", "statistical",
                  "hierarchy$cluster_size_sd must be < 5 * cluster_size for realistic cluster sizes.")
        }
      } else if (!is.null(h$cluster_size_sd)) {
        add_err("hierarchy.cluster_size_sd", "unused_cluster_size_sd", "statistical",
                sprintf("hierarchy$cluster_size_sd must be NULL when cluster_size_dist is '%s'.", cs_dist))
      }
      if (!is_valid_r_ident(h$cluster_name)) {
        add_err("hierarchy.cluster_name", "invalid_identifier", "schema",
                "2-level design requires hierarchy$cluster_name to be a valid pure-ASCII non-reserved R identifier.")
      } else {
        cluster_name_val <- h$cluster_name
      }
      if (!is_num_scalar(h$icc) || h$icc < 0 || h$icc >= 1) {
        add_err("hierarchy.icc", "invalid_icc", "statistical",
                "2-level design requires finite hierarchy$icc in [0, 1).")
      } else {
        icc_val <- as.numeric(h$icc)
      }

      # Longitudinal time_points & time_spacing guardrails
      if (identical(spec$design_type, "longitudinal_growth")) {
        if (!is_whole_scalar(h$time_points) || as.integer(h$time_points) < 3L) {
          add_err("hierarchy.time_points", "invalid_time_points", "statistical",
                  "Longitudinal growth design requires integer hierarchy$time_points >= 3.")
        } else {
          tp_val <- as.integer(h$time_points)
          if (!is.na(cluster_size_val) && cluster_size_val != tp_val) {
            add_err("hierarchy.cluster_size", "time_points_cluster_size_mismatch", "statistical",
                    sprintf("Longitudinal growth requires hierarchy$cluster_size (%d) == hierarchy$time_points (%d).", cluster_size_val, tp_val))
          }
          if (!identical(cs_dist, "fixed")) {
            add_err("hierarchy.cluster_size_dist", "longitudinal_requires_fixed_waves", "statistical",
                    "Longitudinal growth designs use fixed wave schedules; use missingness$mechanism = 'monotone_dropout' for attrition.")
          }
          ts <- h$time_spacing
          if (!is.numeric(ts) || length(ts) != tp_val || any(!is.finite(ts)) || any(diff(ts) <= 0)) {
            add_err("hierarchy.time_spacing", "invalid_time_spacing", "statistical",
                    sprintf("hierarchy$time_spacing must be a strictly increasing finite numeric vector of length %d.", tp_val))
          }
        }
      } else {
        if (!is.null(h$time_points)) {
          add_err("hierarchy.time_points", "time_points_non_longitudinal", "statistical",
                  "hierarchy$time_points is only valid for design_type == 'longitudinal_growth'.")
        }
        if (!is.null(h$time_spacing)) {
          add_err("hierarchy.time_spacing", "time_spacing_non_longitudinal", "statistical",
                  "hierarchy$time_spacing is only valid for design_type == 'longitudinal_growth'.")
        }
      }
    }
  }

  # 5. Predictors
  pred_names <- character()
  pred_levels <- integer()

  if (!is.list(spec$predictors) || length(spec$predictors) < 1L) {
    add_err("predictors", "empty_predictors", "schema", "predictors must be a non-empty list of predictor specifications.")
  } else {
    for (i in seq_along(spec$predictors)) {
      p <- spec$predictors[[i]]
      p_path <- sprintf("predictors[%d]", i)
      if (!is.list(p)) {
        add_err(p_path, "invalid_predictor", "schema", "Each predictor must be a named list.")
        next
      }
      check_extra_keys(p, SPEC_ALLOWED_KEYS$predictor, p_path)
      if (!is_valid_r_ident(p$name)) {
        add_err(paste0(p_path, ".name"), "invalid_identifier", "schema",
                sprintf("Predictor name '%s' is not a valid pure-ASCII non-reserved R identifier.", as.character(p$name %||% "")))
      } else {
        pred_names <- c(pred_names, p$name)
      }

      if (!is_whole_scalar(p$level) || !(as.integer(p$level) %in% c(1L, 2L))) {
        add_err(paste0(p_path, ".level"), "invalid_predictor_level", "schema",
                "Predictor level must be integer 1 or 2.")
        pred_levels <- c(pred_levels, NA_integer_)
      } else {
        pl <- as.integer(p$level)
        pred_levels <- c(pred_levels, pl)
        if (!is.na(levels_val) && pl > levels_val) {
          add_err(paste0(p_path, ".level"), "predictor_level_exceeds_hierarchy", "statistical",
                  sprintf("Predictor '%s' has level %d, exceeding hierarchy$levels (%d).", p$name %||% i, pl, levels_val))
        }
      }

      dist_ok <- is_chr_scalar(p$distribution) && p$distribution %in% c("normal", "binary", "uniform", "time")
      if (!dist_ok) {
        add_err(paste0(p_path, ".distribution"), "invalid_distribution", "schema",
                "Predictor distribution must be 'normal', 'binary', 'uniform', or 'time'.")
      } else if (p$distribution == "time") {
        if (!identical(spec$design_type, "longitudinal_growth") || !identical(as.integer(p$level %||% 0L), 1L)) {
          add_err(paste0(p_path, ".distribution"), "time_predictor_non_longitudinal", "statistical",
                  "Predictor distribution 'time' is only valid for level-1 predictors in 'longitudinal_growth' designs.")
        }
      } else if (p$distribution == "normal") {
        if (!is_num_scalar(p$mean)) {
          add_err(paste0(p_path, ".mean"), "invalid_normal_mean", "schema",
                  sprintf("Normal predictor '%s' requires finite numeric mean.", p$name %||% i))
        }
        if (!is_num_scalar(p$sd) || p$sd <= 0) {
          add_err(paste0(p_path, ".sd"), "non_positive_sd", "statistical",
                  sprintf("Normal predictor '%s' requires finite numeric sd > 0.", p$name %||% i))
        }
      } else if (p$distribution == "binary") {
        if (!is_num_scalar(p$prob) || p$prob <= 0 || p$prob >= 1) {
          add_err(paste0(p_path, ".prob"), "invalid_binary_prob", "statistical",
                  sprintf("Binary predictor '%s' requires finite prob strictly in (0, 1).", p$name %||% i))
        }
      } else if (p$distribution == "uniform") {
        if (!is_num_scalar(p$min) || !is_num_scalar(p$max)) {
          add_err(paste0(p_path, ".min"), "invalid_uniform_bounds", "schema",
                  sprintf("Uniform predictor '%s' requires finite numeric min and max.", p$name %||% i))
        } else if (p$min >= p$max) {
          add_err(paste0(p_path, ".max"), "uniform_min_ge_max", "statistical",
                  sprintf("Uniform predictor '%s' requires min < max.", p$name %||% i))
        }
      }

      if (!is_chr_scalar(p$centering) || !(p$centering %in% c("none", "grand_mean", "group_mean"))) {
        add_err(paste0(p_path, ".centering"), "invalid_centering", "schema",
                "Predictor centering must be 'none', 'grand_mean', or 'group_mean'.")
      } else if (p$centering == "group_mean") {
        if (!identical(levels_val, 2L) || !identical(as.integer(p$level %||% 0L), 1L)) {
          add_err(paste0(p_path, ".centering"), "invalid_group_mean_centering", "statistical",
                  sprintf("Predictor '%s' cannot use 'group_mean' centering unless hierarchy$levels == 2 and predictor level == 1.", p$name %||% i))
        }
      }

      if (!is.null(p$role) && (!is_chr_scalar(p$role) || !(p$role %in% c("covariate", "focal", "treatment")))) {
        add_err(paste0(p_path, ".role"), "invalid_role", "schema",
                "Predictor role must be 'covariate', 'focal', or 'treatment'.")
      }
    }

    if (anyDuplicated(pred_names)) {
      dups <- unique(pred_names[duplicated(pred_names)])
      add_err("predictors", "duplicate_predictor_names", "schema",
              sprintf("Duplicate predictor name(s): %s.", paste(dups, collapse = ", ")))
    }
  }
  pred_level_map <- stats::setNames(pred_levels[seq_along(pred_names)], pred_names)

  # 5b. Optional Predictor Correlation Matrix (strict PD + same-level guardrail)
  if (!is.null(spec$predictor_corr)) {
    p_len <- length(spec$predictors)
    pcm <- spec$predictor_corr
    if (check_corr_matrix(pcm, p_len, "predictor_corr") && length(pred_levels) == p_len && !any(is.na(pred_levels))) {
      for (i in seq_len(p_len)) {
        for (j in seq_len(p_len)) {
          if (i < j && pred_levels[[i]] != pred_levels[[j]] && abs(pcm[i, j]) > 1e-8) {
            add_err(
              "predictor_corr", "cross_level_predictor_corr", "statistical",
              sprintf("predictor_corr[%d,%d] cannot specify non-zero correlation between level-%d and level-%d predictors.",
                      i, j, pred_levels[[i]], pred_levels[[j]])
            )
          }
        }
      }
    }
  }

  # 6. Outcome
  out_family <- NA_character_
  out_name <- NULL
  if (!is.list(spec$outcome)) {
    add_err("outcome", "invalid_type", "schema", "outcome must be a named list.")
  } else {
    o <- spec$outcome
    check_extra_keys(o, SPEC_ALLOWED_KEYS$outcome, "outcome")
    if (!is_valid_r_ident(o$name)) {
      add_err("outcome.name", "invalid_identifier", "schema",
              "outcome$name must be a valid pure-ASCII non-reserved R identifier.")
    } else {
      out_name <- o$name
      if (out_name %in% pred_names) {
        add_err("outcome.name", "name_collision", "schema",
                sprintf("outcome$name ('%s') collides with a predictor name.", out_name))
      }
      if (!is.null(cluster_name_val) && identical(out_name, cluster_name_val)) {
        add_err("outcome.name", "name_collision", "schema",
                sprintf("outcome$name ('%s') collides with hierarchy$cluster_name.", out_name))
      }
    }

    if (!is.null(cluster_name_val) && cluster_name_val %in% pred_names) {
      add_err("hierarchy.cluster_name", "name_collision", "schema",
              sprintf("hierarchy$cluster_name ('%s') collides with a predictor name.", cluster_name_val))
    }

    if (!is_chr_scalar(o$family) || !(o$family %in% c("gaussian", "binomial", "poisson"))) {
      add_err("outcome.family", "invalid_family", "schema",
              "outcome$family must be 'gaussian', 'binomial', or 'poisson'.")
    } else {
      out_family <- o$family
      valid_links <- switch(
        out_family,
        gaussian = "identity",
        binomial = c("logit", "probit"),
        poisson = "log"
      )
      if (!is_chr_scalar(o$link) || !(o$link %in% valid_links)) {
        add_err("outcome.link", "invalid_link_for_family", "statistical",
                sprintf("For family '%s', outcome$link must be one of: %s.", out_family, paste(valid_links, collapse = ", ")))
      }
      if (out_family == "gaussian") {
        if (!is_num_scalar(o$residual_sd) || o$residual_sd <= 0) {
          add_err("outcome.residual_sd", "non_positive_residual_sd", "statistical",
                  "Gaussian outcome requires finite outcome$residual_sd > 0.")
        }
        edist <- o$error_dist %||% "normal"
        if (!is_chr_scalar(edist) || !(edist %in% c("normal", "t", "skewed"))) {
          add_err("outcome.error_dist", "invalid_error_dist", "schema",
                  "Gaussian outcome$error_dist must be 'normal', 't', or 'skewed'.")
        } else if (edist == "t") {
          if (!is_num_scalar(o$error_df) || o$error_df <= 2 || o$error_df > 1000) {
            add_err("outcome.error_df", "invalid_error_df", "statistical",
                    "Student-t errors require finite outcome$error_df in (2, 1000] so variance is finite.")
          }
        } else if (edist == "skewed") {
          if (!is_num_scalar(o$error_skew) || o$error_skew == 0 || abs(o$error_skew) > 4) {
            add_err("outcome.error_skew", "invalid_error_skew", "statistical",
                    "Skewed errors require non-zero finite outcome$error_skew in [-4, 4].")
          }
        }
        if (!is.null(o$ar1_rho)) {
          if (!identical(spec$design_type, "longitudinal_growth")) {
            add_err("outcome.ar1_rho", "ar1_non_longitudinal", "statistical",
                    "outcome$ar1_rho is only valid for design_type == 'longitudinal_growth'.")
          } else if (!is_num_scalar(o$ar1_rho) || abs(o$ar1_rho) >= 0.95) {
            add_err("outcome.ar1_rho", "invalid_ar1_rho", "statistical",
                    "Stationary AR(1) autocorrelation requires finite outcome$ar1_rho strictly in (-0.95, 0.95).")
          }
        }
      } else {
        if (!is.null(o$error_dist) && !identical(o$error_dist, "normal")) {
          add_err("outcome.error_dist", "error_dist_non_gaussian", "statistical",
                  sprintf("outcome$error_dist applies only to Gaussian outcomes, not '%s'.", out_family))
        }
        if (!is.null(o$ar1_rho)) {
          add_err("outcome.ar1_rho", "ar1_non_gaussian", "statistical",
                  "outcome$ar1_rho applies only to Gaussian longitudinal models.")
        }
      }
    }
  }

  # 7. Effects (fixed + random + PD correlation matrix + identifiability)
  q_rand <- 0L
  focal_pred <- NULL
  if (!is.list(spec$effects)) {
    add_err("effects", "invalid_type", "schema", "effects must be a named list.")
  } else {
    ef <- spec$effects
    check_extra_keys(ef, SPEC_ALLOWED_KEYS$effects, "effects")
    if (!is_num_scalar(ef$intercept)) {
      add_err("effects.intercept", "invalid_intercept", "schema", "effects$intercept must be a finite numeric scalar.")
    }

    if (!is.list(ef$fixed)) {
      add_err("effects.fixed", "invalid_fixed_effects", "schema", "effects$fixed must be a named list of finite numerics.")
    } else {
      f_names <- names(ef$fixed) %||% character()
      if (length(ef$fixed) > 0L && any(!nzchar(f_names))) {
        add_err("effects.fixed", "unnamed_fixed_effect", "schema", "All entries in effects$fixed must be named.")
      }
      for (fn in f_names) {
        if (!(fn %in% pred_names)) {
          add_err(paste0("effects.fixed.", fn), "unknown_fixed_predictor", "schema",
                  sprintf("Fixed effect '%s' does not match any defined predictor.", fn))
        }
        if (!is_num_scalar(ef$fixed[[fn]])) {
          add_err(paste0("effects.fixed.", fn), "non_numeric_fixed_effect", "schema",
                  sprintf("Fixed effect for '%s' must be a finite numeric scalar.", fn))
        }
      }
    }

    # Interactions validation
    inter_names <- character()
    inters <- ef$interactions %||% list()
    if (!is.list(inters)) {
      add_err("effects.interactions", "invalid_interactions", "schema",
              "effects$interactions must be a list of interaction specifications.")
    } else if (length(inters) > 0L) {
      seen_pairs <- character()
      for (k in seq_along(inters)) {
        it <- inters[[k]]
        i_path <- sprintf("effects.interactions[%d]", k)
        if (!is.list(it)) {
          add_err(i_path, "invalid_interaction_entry", "schema",
                  "Each interaction must be a named list with 'terms' and 'beta'.")
          next
        }
        check_extra_keys(it, SPEC_ALLOWED_KEYS$interaction, i_path)
        tvec <- it$terms
        if (!is.character(tvec) || length(tvec) != 2L || any(is.na(tvec))) {
          add_err(paste0(i_path, ".terms"), "invalid_interaction_terms", "schema",
                  "Each interaction 'terms' must be a character vector of 2 predictor names.")
        } else if (identical(tvec[[1L]], tvec[[2L]])) {
          add_err(paste0(i_path, ".terms"), "self_interaction_forbidden", "statistical",
                  sprintf("Interaction term '%s:%s' must combine two distinct predictors.", tvec[[1L]], tvec[[2L]]))
        } else if (!all(tvec %in% pred_names)) {
          missing_t <- setdiff(tvec, pred_names)
          add_err(paste0(i_path, ".terms"), "unknown_interaction_predictor", "schema",
                  sprintf("Interaction references unknown predictor(s): %s.", paste(missing_t, collapse = ", ")))
        } else {
          canon_pair <- paste(sort(tvec), collapse = ":")
          if (canon_pair %in% seen_pairs) {
            add_err(paste0(i_path, ".terms"), "duplicate_interaction", "schema",
                    sprintf("Duplicate interaction term '%s'.", paste(tvec, collapse = ":")))
          } else {
            seen_pairs <- c(seen_pairs, canon_pair)
            inter_names <- c(inter_names, paste(tvec, collapse = ":"), paste(rev(tvec), collapse = ":"))
          }
        }
        if (!is_num_scalar(it$beta)) {
          add_err(paste0(i_path, ".beta"), "non_numeric_interaction_beta", "schema",
                  "Interaction 'beta' must be a finite numeric scalar.")
        }
      }
    }

    if (!is.null(ef$focal_predictor)) {
      if (!is_chr_scalar(ef$focal_predictor) || !(ef$focal_predictor %in% c(pred_names, inter_names))) {
        add_err("effects.focal_predictor", "invalid_focal_predictor", "schema",
                "effects$focal_predictor must be NULL or match an existing predictor or interaction term.")
      } else {
        focal_pred <- ef$focal_predictor
      }
    }

    if (!is.list(ef$random)) {
      add_err("effects.random", "invalid_random_effects", "schema", "effects$random must be a named list.")
    } else {
      r <- ef$random
      check_extra_keys(r, SPEC_ALLOWED_KEYS$random, "effects.random")
      if (!is_lgl_scalar(r$intercept)) {
        add_err("effects.random.intercept", "invalid_random_intercept", "schema",
                "effects$random$intercept must be TRUE or FALSE.")
      }
      slopes <- r$slopes %||% list()
      if (!is.list(slopes)) {
        add_err("effects.random.slopes", "invalid_random_slopes", "schema",
                "effects$random$slopes must be a list.")
      }

      if (identical(levels_val, 1L)) {
        if (isTRUE(r$intercept)) {
          add_err("effects.random.intercept", "random_effects_in_single_level", "statistical",
                  "1-level design cannot include a random intercept.")
        }
        if (is.list(slopes) && length(slopes) > 0L) {
          add_err("effects.random.slopes", "random_slopes_in_single_level", "statistical",
                  "1-level design cannot include random slopes.")
        }
        if (!is.null(r$corr_matrix)) {
          add_err("effects.random.corr_matrix", "corr_matrix_in_single_level", "statistical",
                  "1-level design cannot specify a random-effects correlation matrix.")
        }
      } else if (identical(levels_val, 2L)) {
        if (!is.na(icc_val) && icc_val > 0 && identical(r$intercept, FALSE)) {
          add_err("effects.random.intercept", "icc_requires_random_intercept", "statistical",
                  "2-level design with ICC > 0 requires effects$random$intercept = TRUE.")
        }
        if (!is.null(r$intercept_sd)) {
          if (!is_num_scalar(r$intercept_sd) || r$intercept_sd < 0) {
            add_err("effects.random.intercept_sd", "invalid_intercept_sd", "statistical",
                    "effects$random$intercept_sd must be a non-negative finite numeric scalar.")
          } else if (!is.na(icc_val) && !is.na(out_family)) {
            l1_var <- outcome_level1_variance(spec$outcome)
            implied_icc <- (r$intercept_sd^2) / (r$intercept_sd^2 + l1_var)
            if (abs(implied_icc - icc_val) > 1e-2) {
              add_err("effects.random.intercept_sd", "intercept_sd_icc_mismatch", "statistical",
                      sprintf("effects$random$intercept_sd (implied ICC = %.3f) conflicts with hierarchy$icc (%.3f).",
                              implied_icc, icc_val))
            }
          }
        }

        slope_preds <- character()
        if (is.list(slopes) && length(slopes) > 0L) {
          for (j in seq_along(slopes)) {
            sl <- slopes[[j]]
            s_path <- sprintf("effects.random.slopes[%d]", j)
            if (!is.list(sl)) {
              add_err(s_path, "invalid_slope_entry", "schema", "Each random slope must be a list with 'predictor' and 'sd'.")
              next
            }
            check_extra_keys(sl, SPEC_ALLOWED_KEYS$slope, s_path)
            if (!is_chr_scalar(sl$predictor) || !(sl$predictor %in% pred_names)) {
              add_err(paste0(s_path, ".predictor"), "unknown_slope_predictor", "schema",
                      sprintf("Random slope predictor '%s' does not exist in predictors.", as.character(sl$predictor %||% "")))
            } else {
              slope_preds <- c(slope_preds, sl$predictor)
              if (!identical(as.integer(pred_level_map[[sl$predictor]]), 1L)) {
                add_err(paste0(s_path, ".predictor"), "random_slope_level2_unidentifiable", "statistical",
                        sprintf("Random slope on '%s' is unidentifiable: level-2 cluster-constant predictors cannot have random slopes across clusters.", sl$predictor))
              }
            }
            if (!is_num_scalar(sl$sd) || sl$sd <= 0) {
              add_err(paste0(s_path, ".sd"), "non_positive_slope_sd", "statistical",
                      sprintf("Random slope '%s' requires finite numeric sd > 0.", as.character(sl$predictor %||% j)))
            }
          }
          if (anyDuplicated(slope_preds)) {
            add_err("effects.random.slopes", "duplicate_random_slope", "schema",
                    "Duplicate predictor specified in effects$random$slopes.")
          }
        }

        q_rand <- as.integer(isTRUE(r$intercept)) + (if (is.list(slopes)) length(slopes) else 0L)

        if (q_rand > 0L) {
          if (!is.na(cluster_size_val) && cluster_size_val <= q_rand) {
            add_err("hierarchy.cluster_size", "insufficient_cluster_size_for_random_effects", "statistical",
                    sprintf("Model has %d random effect(s) per cluster, requiring cluster_size > %d (got %d) for identifiability.",
                            q_rand, q_rand, cluster_size_val))
          }
          if (!is.na(n_clusters_val) && n_clusters_val <= q_rand) {
            add_err("hierarchy.n_clusters", "insufficient_clusters_for_random_effects", "statistical",
                    sprintf("Model has %d random effect(s), requiring n_clusters > %d (got %d).",
                            q_rand, q_rand, n_clusters_val))
          }
          if (!is.na(n_clusters_val) && n_clusters_val < 10L) {
            add_warn("hierarchy.n_clusters", "few_clusters_warning", "statistical",
                     sprintf("Only %d clusters specified; variance component estimates may be unstable with fewer than 10 clusters.", n_clusters_val))
          }
        }

        if (!is.null(r$corr_matrix)) {
          check_corr_matrix(r$corr_matrix, q_rand, "effects.random.corr_matrix")
        }
      }
    }
  }

  # 8. Missingness
  miss_mech <- NA_character_
  if (!is.list(spec$missingness)) {
    add_err("missingness", "invalid_type", "schema", "missingness must be a named list.")
  } else {
    m <- spec$missingness
    check_extra_keys(m, SPEC_ALLOWED_KEYS$missingness, "missingness")
    if (!is_chr_scalar(m$mechanism) || !(m$mechanism %in% c("none", "MCAR", "MAR", "monotone_dropout"))) {
      add_err("missingness.mechanism", "invalid_missing_mechanism", "schema",
              "missingness$mechanism must be 'none', 'MCAR', 'MAR', or 'monotone_dropout'.")
    } else {
      miss_mech <- m$mechanism
      if (identical(miss_mech, "monotone_dropout") && !identical(spec$design_type, "longitudinal_growth")) {
        add_err("missingness.mechanism", "monotone_dropout_non_longitudinal", "statistical",
                "missingness$mechanism == 'monotone_dropout' requires design_type == 'longitudinal_growth'.")
      }
    }

    if (!is_num_scalar(m$rate) || m$rate < 0 || m$rate >= 0.95) {
      add_err("missingness.rate", "invalid_missing_rate", "statistical",
              "missingness$rate must be a finite numeric scalar in [0, 0.95).")
    } else if (!is.na(miss_mech)) {
      if (miss_mech == "none" && m$rate != 0) {
        add_err("missingness.rate", "nonzero_rate_with_no_mechanism", "statistical",
                "missingness$rate must be 0 when missingness$mechanism == 'none'.")
      }
      if (miss_mech %in% c("MCAR", "MAR", "monotone_dropout") && m$rate <= 0) {
        add_err("missingness.rate", "zero_rate_with_active_mechanism", "statistical",
                sprintf("missingness$rate must be > 0 when missingness$mechanism == '%s'.", miss_mech))
      }
    }

    if (!is_chr_scalar(m$target) || !identical(m$target, "outcome")) {
      add_err("missingness.target", "invalid_missing_target", "schema",
              "missingness$target must be 'outcome' in IR schema.")
    }

    if (identical(miss_mech, "MAR")) {
      if (!is_chr_scalar(m$auxiliary) || !(m$auxiliary %in% pred_names)) {
        add_err("missingness.auxiliary", "invalid_mar_auxiliary", "statistical",
                "MAR missingness requires missingness$auxiliary to name an existing predictor.")
      }
      if (!is_num_scalar(m$strength) || m$strength == 0) {
        add_err("missingness.strength", "invalid_mar_strength", "statistical",
                "MAR missingness requires finite non-zero numeric missingness$strength.")
      }
    } else if (identical(miss_mech, "monotone_dropout") && !is.null(m$auxiliary)) {
      if (!is_chr_scalar(m$auxiliary) || !(m$auxiliary %in% pred_names)) {
        add_err("missingness.auxiliary", "invalid_mar_auxiliary", "statistical",
                "monotone_dropout auxiliary must be NULL or match an existing predictor.")
      }
      if (!is.null(m$strength) && (!is_num_scalar(m$strength) || m$strength == 0)) {
        add_err("missingness.strength", "invalid_mar_strength", "statistical",
                "monotone_dropout strength (when provided) must be finite non-zero numeric.")
      }
    }
  }

  # 9. Estimator
  if (!is.list(spec$estimator)) {
    add_err("estimator", "invalid_type", "schema", "estimator must be a named list.")
  } else {
    est <- spec$estimator
    check_extra_keys(est, SPEC_ALLOWED_KEYS$estimator, "estimator")
    if (!is_chr_scalar(est$engine) || !(est$engine %in% c("lm", "glm", "lmer", "glmer"))) {
      add_err("estimator.engine", "invalid_estimator_engine", "schema",
              "estimator$engine must be one of 'lm', 'glm', 'lmer', or 'glmer'.")
    } else if (!is.na(levels_val) && !is.na(out_family)) {
      if (levels_val == 1L && out_family == "gaussian" && !(est$engine %in% c("lm", "glm"))) {
        add_err("estimator.engine", "estimator_incompatible", "statistical",
                sprintf("1-level Gaussian model requires 'lm' or 'glm', not '%s'.", est$engine))
      }
      if (levels_val == 1L && out_family != "gaussian" && est$engine != "glm") {
        add_err("estimator.engine", "estimator_incompatible", "statistical",
                sprintf("1-level %s model requires 'glm', not '%s'.", out_family, est$engine))
      }
      if (levels_val == 2L && q_rand > 0L && out_family == "gaussian" && est$engine != "lmer") {
        add_err("estimator.engine", "estimator_incompatible", "statistical",
                sprintf("2-level Gaussian model with random effects requires 'lmer', not '%s'.", est$engine))
      }
      if (levels_val == 2L && q_rand > 0L && out_family != "gaussian" && est$engine != "glmer") {
        add_err("estimator.engine", "estimator_incompatible", "statistical",
                sprintf("2-level %s model with random effects requires 'glmer', not '%s'.", out_family, est$engine))
      }
    }

    if (!is_lgl_scalar(est$reml)) {
      add_err("estimator.reml", "invalid_reml", "schema", "estimator$reml must be TRUE or FALSE.")
    }
    if (!is_num_scalar(est$conf_level) || est$conf_level <= 0.5 || est$conf_level >= 1) {
      add_err("estimator.conf_level", "invalid_conf_level", "statistical",
              "estimator$conf_level must lie strictly in (0.5, 1).")
    }
    if (!is_chr_scalar(est$missing_handling) || !identical(est$missing_handling, "complete_case")) {
      add_err("estimator.missing_handling", "invalid_missing_handling", "schema",
              "estimator$missing_handling must be 'complete_case'.")
    }
  }

  # 10. Simulation & parameter grid
  if (!is.list(spec$simulation)) {
    add_err("simulation", "invalid_type", "schema", "simulation must be a named list.")
  } else {
    sim <- spec$simulation
    check_extra_keys(sim, SPEC_ALLOWED_KEYS$simulation, "simulation")
    if (!is_whole_scalar(sim$n_reps) || as.integer(sim$n_reps) < 1L || as.integer(sim$n_reps) > 100000L) {
      add_err("simulation.n_reps", "invalid_n_reps", "statistical",
              "simulation$n_reps must be an integer in [1, 100000].")
    }
    if (!is_whole_scalar(sim$seed)) {
      add_err("simulation.seed", "invalid_seed", "schema", "simulation$seed must be a finite integer scalar.")
    }
    if (!is_num_scalar(sim$alpha) || sim$alpha <= 0 || sim$alpha >= 0.5) {
      add_err("simulation.alpha", "invalid_alpha", "statistical",
              "simulation$alpha must be a finite numeric scalar strictly in (0, 0.5).")
    }

    pg <- sim$parameter_grid
    if (!is.null(pg)) {
      if (!is.list(pg)) {
        add_err("simulation.parameter_grid", "invalid_parameter_grid", "schema",
                "simulation$parameter_grid must be a named list.")
      } else if (length(pg) > 0L) {
        gnames <- names(pg) %||% character()
        if (any(!nzchar(gnames))) {
          add_err("simulation.parameter_grid", "unnamed_grid_axis", "schema",
                  "All parameter_grid entries must be named.")
        }
        cell_prod <- 1
        for (gn in gnames) {
          gpath <- paste0("simulation.parameter_grid.", gn)
          vals <- pg[[gn]]
          if (!(gn %in% SPEC_ALLOWED_GRID_PARAMS)) {
            add_err(gpath, "unsupported_grid_param", "schema",
                    sprintf("Grid parameter '%s' is not supported. Allowed: %s.",
                            gn, paste(SPEC_ALLOWED_GRID_PARAMS, collapse = ", ")))
            next
          }
          if (!is.numeric(vals) || length(vals) < 1L || length(vals) > 20L || any(!is.finite(vals))) {
            add_err(gpath, "invalid_grid_values", "schema",
                    sprintf("Grid parameter '%s' must be a finite numeric vector of length 1..20.", gn))
            next
          }
          cell_prod <- cell_prod * length(vals)

          if (gn == "n_obs") {
            if (!identical(levels_val, 1L)) {
              add_err(gpath, "grid_param_inapplicable", "statistical", "Grid parameter 'n_obs' applies only to 1-level designs.")
            } else if (any(vals != floor(vals) | vals < 5)) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical", "All 'n_obs' grid values must be integers >= 5.")
            }
          } else if (gn == "n_clusters") {
            if (!identical(levels_val, 2L)) {
              add_err(gpath, "grid_param_inapplicable", "statistical", "Grid parameter 'n_clusters' applies only to 2-level designs.")
            } else if (any(vals != floor(vals) | vals < max(3L, q_rand + 1L))) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical",
                      sprintf("All 'n_clusters' grid values must be integers >= %d.", max(3L, q_rand + 1L)))
            }
          } else if (gn == "cluster_size") {
            if (!identical(levels_val, 2L)) {
              add_err(gpath, "grid_param_inapplicable", "statistical", "Grid parameter 'cluster_size' applies only to 2-level designs.")
            } else if (identical(spec$design_type, "longitudinal_growth")) {
              add_err(gpath, "grid_param_inapplicable", "statistical",
                      "Grid parameter 'cluster_size' is not supported for longitudinal_growth designs with fixed time_spacing.")
            } else if (any(vals != floor(vals) | vals < max(2L, q_rand + 1L))) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical",
                      sprintf("All 'cluster_size' grid values must be integers >= %d for identifiability.", max(2L, q_rand + 1L)))
            }
          } else if (gn == "icc") {
            if (!identical(levels_val, 2L)) {
              add_err(gpath, "grid_param_inapplicable", "statistical", "Grid parameter 'icc' applies only to 2-level designs.")
            } else if (any(vals < 0 | vals >= 1)) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical", "All 'icc' grid values must lie in [0, 1).")
            }
          } else if (gn == "residual_sd") {
            if (!identical(out_family, "gaussian")) {
              add_err(gpath, "grid_param_inapplicable", "statistical", "Grid parameter 'residual_sd' applies only to Gaussian outcomes.")
            } else if (any(vals <= 0)) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical", "All 'residual_sd' grid values must be > 0.")
            }
          } else if (gn == "missing_rate") {
            if (identical(miss_mech, "none")) {
              add_err(gpath, "grid_param_inapplicable", "statistical",
                      "Grid parameter 'missing_rate' requires missingness$mechanism to be 'MCAR', 'MAR', or 'monotone_dropout'.")
            } else if (any(vals < 0 | vals >= 0.95)) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical", "All 'missing_rate' grid values must lie in [0, 0.95).")
            }
          } else if (gn == "focal_effect") {
            if (is.null(focal_pred)) {
              add_err(gpath, "grid_param_inapplicable", "statistical",
                      "Grid parameter 'focal_effect' requires effects$focal_predictor to be set.")
            }
          } else if (gn == "ar1_rho") {
            if (!identical(spec$design_type, "longitudinal_growth") || !identical(out_family, "gaussian")) {
              add_err(gpath, "grid_param_inapplicable", "statistical",
                      "Grid parameter 'ar1_rho' applies only to Gaussian longitudinal_growth designs.")
            } else if (any(abs(vals) >= 0.95)) {
              add_err(gpath, "grid_value_out_of_bounds", "statistical",
                      "All 'ar1_rho' grid values must lie strictly in (-0.95, 0.95).")
            }
          }
        }
        if (cell_prod > SPEC_MAX_GRID_CELLS) {
          add_err("simulation.parameter_grid", "grid_combinatorial_explosion", "statistical",
                  sprintf("Parameter grid has %d combinations, exceeding the safety maximum of %d cells.",
                          as.integer(cell_prod), SPEC_MAX_GRID_CELLS))
        }
      }
    }
  }

  # 11. Defaults rationale (strict ASCII path keys & bounded string values)
  if (!is.list(spec$defaults_rationale)) {
    add_err("defaults_rationale", "invalid_rationale", "schema", "defaults_rationale must be a named list of character scalars.")
  } else if (length(spec$defaults_rationale) > 0L) {
    rnames <- names(spec$defaults_rationale) %||% character()
    if (any(!nzchar(rnames))) {
      add_err("defaults_rationale", "unnamed_rationale", "schema", "All entries in defaults_rationale must be named.")
    }
    for (rn in rnames) {
      if (nzchar(rn) && !is_valid_rationale_key(rn)) {
        add_err(paste0("defaults_rationale.", rn), "invalid_rationale_key", "schema",
                sprintf("defaults_rationale key '%s' must be a pure-ASCII dotted IR path.", rn))
      }
      rv <- spec$defaults_rationale[[rn]]
      if (!is_chr_scalar(rv) || nchar(rv) > 1000L) {
        add_err(paste0("defaults_rationale.", rn), "non_string_rationale", "schema",
                "All entries in defaults_rationale must be character scalars (<= 1000 chars).")
      }
    }
  }

  build_report()
}

#' Check whether a `spec_ir` object is valid
#'
#' @param spec A `spec_ir` object.
#' @return Logical scalar (`TRUE` if valid, `FALSE` otherwise).
#' @export
#' @examples
#' is_valid_spec_ir(new_spec_ir())
is_valid_spec_ir <- function(spec) {
  isTRUE(validate_spec_ir(spec, strict = FALSE)$valid)
}

#' Assert that a `spec_ir` object passes all schema and statistical checks
#'
#' @param spec A `spec_ir` object.
#' @return Invisibly returns `spec` if valid; otherwise aborts with formatted diagnostics.
#' @export
#' @examples
#' spec <- assert_valid_spec_ir(new_spec_ir())
#' spec$design_type
assert_valid_spec_ir <- function(spec) {
  validate_spec_ir(spec, strict = TRUE)
  invisible(spec)
}

abort_spec_validation <- function(report) {
  bullets <- stats::setNames(
    sprintf("[%s | %s] %s", report$errors$category, report$errors$path, report$errors$message),
    rep("x", nrow(report$errors))
  )
  cli::cli_abort(
    c(
      sprintf("Invalid spec_ir (%d validation error%s):", nrow(report$errors), if (nrow(report$errors) == 1L) "" else "s"),
      bullets
    ),
    class = "specshiny_validation_error"
  )
}

#' @export
print.spec_validation <- function(x, ...) {
  if (x$valid) {
    cli::cli_alert_success("spec_ir validation passed (0 errors, {nrow(x$warnings)} warning{?s}).")
  } else {
    cli::cli_alert_danger("spec_ir validation failed ({nrow(x$errors)} error{?s}, {nrow(x$warnings)} warning{?s}):")
    for (i in seq_len(nrow(x$errors))) {
      cli::cli_bullets(c("x" = sprintf("[%s | %s] %s", x$errors$category[[i]], x$errors$path[[i]], x$errors$message[[i]])))
    }
  }
  if (nrow(x$warnings) > 0L) {
    for (i in seq_len(nrow(x$warnings))) {
      cli::cli_bullets(c("!" = sprintf("[%s | %s] %s", x$warnings$category[[i]], x$warnings$path[[i]], x$warnings$message[[i]])))
    }
  }
  invisible(x)
}

# Scalar predicates
is_chr_scalar <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

is_num_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

is_whole_scalar <- function(x) {
  is_num_scalar(x) && (x == floor(x))
}

is_lgl_scalar <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}
