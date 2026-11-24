# Current IR schema version and supported versions
SPEC_SCHEMA_VERSION <- "1.2.0"
SPEC_SUPPORTED_SCHEMA_VERSIONS <- c("1.0.0", "1.1.0", "1.2.0")

#' Supported specification design types
#'
#' Returns the canonical design types supported by the `specshiny` IR grammar.
#'
#' @return A character vector of supported design type names.
#' @export
#' @examples
#' spec_design_types()
spec_design_types <- function() {
  c("monte_carlo_lm", "power_analysis", "multilevel_sim", "two_level_mlm", "longitudinal_growth")
}

#' Locate the bundled JSON Schema for the `spec_ir` grammar
#'
#' @return Path to the bundled JSON Schema file (`spec_ir_v1.json`).
#' @export
#' @examples
#' spec_schema_path()
spec_schema_path <- function() {
  system.file("schema", "spec_ir_v1.json", package = "specshiny")
}

#' Construct a predictor specification
#'
#' Helper constructor for defining a single predictor entry inside a [new_spec_ir()]
#' specification.
#'
#' @param name Character scalar giving a valid R identifier for the predictor.
#' @param level Integer scalar (`1L` or `2L`) indicating the hierarchy level.
#' @param distribution One of `"normal"`, `"binary"`, `"uniform"`, or `"time"`
#'   (for wave-indexed longitudinal time predictors).
#' @param mean Numeric mean for `"normal"` predictors (defaults to `0`).
#' @param sd Positive numeric SD for `"normal"` predictors (defaults to `1`).
#' @param prob Numeric probability in `(0, 1)` for `"binary"` predictors (defaults to `0.5`).
#' @param min Numeric lower bound for `"uniform"` predictors (defaults to `0`).
#' @param max Numeric upper bound for `"uniform"` predictors (defaults to `1`).
#' @param centering One of `"none"`, `"grand_mean"`, or `"group_mean"`.
#' @param role One of `"covariate"`, `"focal"`, or `"treatment"`.
#'
#' @return A named list representing a predictor specification.
#' @export
#' @examples
#' spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1)
#' spec_predictor("trt", level = 2L, distribution = "binary", prob = 0.5, role = "treatment")
spec_predictor <- function(name,
                           level = 1L,
                           distribution = c("normal", "binary", "uniform", "time"),
                           mean = NULL,
                           sd = NULL,
                           prob = NULL,
                           min = NULL,
                           max = NULL,
                           centering = c("none", "grand_mean", "group_mean"),
                           role = c("covariate", "focal", "treatment")) {
  distribution <- match.arg(distribution)
  centering <- match.arg(centering)
  role <- match.arg(role)

  out <- list(
    name = as.character(name),
    level = maybe_int(level),
    distribution = distribution,
    mean = NULL,
    sd = NULL,
    prob = NULL,
    min = NULL,
    max = NULL,
    centering = centering,
    role = role
  )

  if (distribution == "normal") {
    out$mean <- if (is.null(mean)) 0 else as.numeric(mean)
    out$sd <- if (is.null(sd)) 1 else as.numeric(sd)
  } else if (distribution == "binary") {
    out$prob <- if (is.null(prob)) 0.5 else as.numeric(prob)
  } else if (distribution == "uniform") {
    out$min <- if (is.null(min)) 0 else as.numeric(min)
    out$max <- if (is.null(max)) 1 else as.numeric(max)
  }

  out
}

#' Construct a two-way interaction term specification
#'
#' Helper constructor for defining an interaction (including cross-level
#' interactions such as `x1:z1`) inside `effects$interactions` of [new_spec_ir()].
#'
#' @param term1 Character scalar naming the first predictor.
#' @param term2 Character scalar naming the second predictor.
#' @param beta Numeric scalar true interaction coefficient.
#'
#' @return A named list with elements `terms` (length-2 character vector) and `beta`.
#' @export
#' @examples
#' spec_interaction("x1", "z1", beta = 0.25)
spec_interaction <- function(term1, term2, beta = 0.2) {
  list(
    terms = c(as.character(term1), as.character(term2)),
    beta = as.numeric(beta)
  )
}

#' Construct a typed `spec_ir` statistical and simulation specification
#'
#' Creates a versioned S3 `spec_ir` object describing a statistical or Monte Carlo
#' simulation study. Missing subsections are populated with statistically sensible
#' defaults, and plain-language explanations are recorded in `defaults_rationale`.
#'
#' @param design_type Character scalar, one of [spec_design_types()].
#' @param title Character scalar title for the specification.
#' @param description Character scalar description.
#' @param source_prompt Optional character scalar capturing the originating NL prompt.
#' @param tags Optional character vector of descriptive tags.
#' @param hierarchy Optional named list specifying data hierarchy (`levels`, `n_obs`,
#'   `n_clusters`, `cluster_size`, `cluster_name`, `unit_name`, `icc`).
#' @param predictors Optional list of predictor specifications (see [spec_predictor()]).
#' @param predictor_corr Optional numeric positive-definite correlation matrix of
#'   dimension `length(predictors) x length(predictors)` specifying correlations
#'   among same-level predictors (defaults to `NULL` for independent predictors).
#' @param outcome Optional named list specifying `name`, `family`, `link`, and `residual_sd`.
#' @param effects Optional named list specifying `intercept`, `fixed`, `focal_predictor`,
#'   and `random` (`intercept`, `intercept_sd`, `slopes`, `corr_matrix`).
#' @param missingness Optional named list specifying `mechanism` (`"none"`, `"MCAR"`, `"MAR"`),
#'   `rate`, `target`, `auxiliary`, and `strength`.
#' @param estimator Optional named list specifying `engine`, `reml`, `conf_level`,
#'   and `missing_handling`.
#' @param simulation Optional named list specifying `n_reps`, `seed`, `alpha`, and
#'   `parameter_grid`.
#' @param defaults_rationale Optional named list mapping dotted IR paths to plain-language
#'   explanations of inferred defaults.
#' @param schema_version Character schema version (defaults to `"1.0.0"`).
#' @param validate Logical; if `TRUE` (default), validates the resulting IR with
#'   [assert_valid_spec_ir()].
#'
#' @return An S3 object of class `c("spec_ir", "list")`.
#' @export
#' @examples
#' spec <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   title = "School Achievement Study",
#'   hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2),
#'   simulation = list(n_reps = 100L, seed = 42L)
#' )
#' print(spec)
new_spec_ir <- function(design_type = "monte_carlo_lm",
                        title = "Untitled Statistical Study",
                        description = "",
                        source_prompt = NULL,
                        tags = character(),
                        hierarchy = list(),
                        predictors = NULL,
                        predictor_corr = NULL,
                        outcome = list(),
                        effects = list(),
                        missingness = list(),
                        estimator = list(),
                        simulation = list(),
                        defaults_rationale = list(),
                        schema_version = SPEC_SCHEMA_VERSION,
                        validate = TRUE) {
  rationale <- as.list(defaults_rationale)

  add_rationale <- function(path, text) {
    if (is.null(rationale[[path]])) {
      rationale[[path]] <<- text
    }
  }

  # Infer default levels from design_type if omitted
  is_longitudinal <- identical(design_type, "longitudinal_growth")
  default_levels <- if (identical(design_type, "multilevel_sim") ||
                        identical(design_type, "two_level_mlm") ||
                        is_longitudinal) {
    2L
  } else {
    1L
  }

  levels_val <- if (is.null(hierarchy$levels)) {
    add_rationale(
      "hierarchy.levels",
      sprintf("Defaulted to %d-level hierarchy based on design_type '%s'.", default_levels, design_type)
    )
    default_levels
  } else {
    maybe_int(hierarchy$levels)
  }

  # Build hierarchy defaults cleanly
  if (identical(levels_val, 2L)) {
    n_clusters <- if (is.null(hierarchy$n_clusters)) {
      def_j <- if (is_longitudinal) 100L else 40L
      add_rationale("hierarchy.n_clusters", sprintf("Defaulted to %d level-2 units for stable random-effect variance estimation.", def_j))
      def_j
    } else {
      maybe_int(hierarchy$n_clusters)
    }

    time_points <- NULL
    time_spacing <- NULL
    if (is_longitudinal) {
      time_points <- if (!is.null(hierarchy$time_points)) {
        maybe_int(hierarchy$time_points)
      } else if (!is.null(hierarchy$cluster_size)) {
        maybe_int(hierarchy$cluster_size)
      } else {
        add_rationale("hierarchy.time_points", "Defaulted to 5 repeated measurement waves for linear growth-curve estimation.")
        5L
      }
      cluster_size <- if (is.null(hierarchy$cluster_size)) time_points else maybe_int(hierarchy$cluster_size)
      time_spacing <- if (is.null(hierarchy$time_spacing)) {
        if (is_whole_scalar(time_points) && as.integer(time_points) >= 1L) {
          seq(0, as.integer(time_points) - 1L)
        } else {
          NULL
        }
      } else {
        as.numeric(hierarchy$time_spacing)
      }
      cluster_name <- hierarchy$cluster_name %||% "subject"
      unit_name <- hierarchy$unit_name %||% "wave"
    } else {
      cluster_size <- if (is.null(hierarchy$cluster_size)) {
        add_rationale("hierarchy.cluster_size", "Defaulted to 20 units per cluster for reliable within-cluster estimation.")
        20L
      } else {
        maybe_int(hierarchy$cluster_size)
      }
      cluster_name <- if (is.null(hierarchy$cluster_name)) {
        add_rationale("hierarchy.cluster_name", "Defaulted cluster identifier name to 'cluster'.")
        "cluster"
      } else {
        hierarchy$cluster_name
      }
      unit_name <- if (is.null(hierarchy$unit_name)) {
        "observation"
      } else {
        hierarchy$unit_name
      }
      if (!is.null(hierarchy$time_points)) time_points <- maybe_int(hierarchy$time_points)
      if (!is.null(hierarchy$time_spacing)) time_spacing <- as.numeric(hierarchy$time_spacing)
    }

    icc <- if (is.null(hierarchy$icc)) {
      def_icc <- if (is_longitudinal) 0.40 else 0.15
      add_rationale("hierarchy.icc", sprintf("Defaulted intraclass correlation (ICC) to %.2f.", def_icc))
      def_icc
    } else {
      hierarchy$icc
    }
    cs_dist <- hierarchy$cluster_size_dist %||% "fixed"
    cs_sd <- if (identical(cs_dist, "normal")) {
      if (is.null(hierarchy$cluster_size_sd)) {
        add_rationale("hierarchy.cluster_size_sd", "Defaulted unbalanced cluster-size SD to 25% of mean cluster size.")
        max(1, round(0.25 * as.numeric(cluster_size), 2))
      } else {
        hierarchy$cluster_size_sd
      }
    } else {
      hierarchy$cluster_size_sd
    }
    hier_obj <- list(
      levels = levels_val,
      n_obs = NULL,
      n_clusters = n_clusters,
      cluster_size = cluster_size,
      cluster_size_dist = cs_dist,
      cluster_size_sd = cs_sd,
      time_points = time_points,
      time_spacing = time_spacing,
      cluster_name = cluster_name,
      unit_name = unit_name,
      icc = icc
    )
  } else {
    n_obs <- if (is.null(hierarchy$n_obs)) {
      add_rationale("hierarchy.n_obs", "Defaulted sample size to 200 observations for adequate single-level precision.")
      200L
    } else {
      maybe_int(hierarchy$n_obs)
    }
    unit_name <- if (is.null(hierarchy$unit_name)) "observation" else hierarchy$unit_name
    hier_obj <- list(
      levels = levels_val,
      n_obs = n_obs,
      n_clusters = NULL,
      cluster_size = NULL,
      cluster_size_dist = NULL,
      cluster_size_sd = NULL,
      time_points = hierarchy$time_points,
      time_spacing = hierarchy$time_spacing,
      cluster_name = NULL,
      unit_name = unit_name,
      icc = hierarchy$icc
    )
  }

  # Predictors defaults
  if (is.null(predictors)) {
    if (is_longitudinal) {
      add_rationale("predictors", "Added default longitudinal wave predictor 'time' (level-1 wave index).")
      pred_list <- list(spec_predictor("time", level = 1L, distribution = "time", centering = "none", role = "focal"))
    } else {
      add_rationale("predictors", "Added a default standardized normal focal predictor 'x1' (mean = 0, sd = 1).")
      pred_list <- list(spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "focal"))
    }
  } else {
    pred_list <- lapply(predictors, normalize_predictor)
  }

  # Outcome defaults
  out_name <- outcome$name %||% "y"
  out_family <- outcome$family %||% {
    add_rationale("outcome.family", "Defaulted outcome family to 'gaussian' for a continuous response.")
    "gaussian"
  }
  default_link <- switch(
    as.character(out_family),
    gaussian = "identity",
    binomial = "logit",
    poisson = "log",
    "identity"
  )
  out_link <- outcome$link %||% default_link
  out_resid_sd <- if (identical(out_family, "gaussian")) {
    if (is.null(outcome$residual_sd)) {
      add_rationale("outcome.residual_sd", "Defaulted level-1 residual SD to 1.0 so fixed effects are on a standardized scale.")
      1.0
    } else {
      outcome$residual_sd
    }
  } else {
    outcome$residual_sd
  }
  out_err_dist <- if (identical(out_family, "gaussian")) {
    outcome$error_dist %||% "normal"
  } else {
    outcome$error_dist
  }
  out_err_df <- if (identical(out_family, "gaussian") && identical(out_err_dist, "t")) {
    if (is.null(outcome$error_df)) {
      add_rationale("outcome.error_df", "Defaulted Student-t error degrees of freedom to 5 (moderate heavy tails with finite variance).")
      5
    } else {
      outcome$error_df
    }
  } else {
    outcome$error_df
  }
  out_err_skew <- if (identical(out_family, "gaussian") && identical(out_err_dist, "skewed")) {
    if (is.null(outcome$error_skew)) {
      add_rationale("outcome.error_skew", "Defaulted skewed error skewness to 1.5 (moderately right-skewed standardized gamma errors).")
      1.5
    } else {
      outcome$error_skew
    }
  } else {
    outcome$error_skew
  }
  out_ar1 <- if (is_longitudinal && identical(out_family, "gaussian")) {
    if (is.null(outcome$ar1_rho)) {
      add_rationale("outcome.ar1_rho", "Defaulted within-subject AR(1) residual autocorrelation to 0.30.")
      0.30
    } else {
      outcome$ar1_rho
    }
  } else {
    outcome$ar1_rho
  }
  outcome_obj <- list(
    name = out_name,
    family = out_family,
    link = out_link,
    residual_sd = out_resid_sd,
    error_dist = out_err_dist,
    error_df = out_err_df,
    error_skew = out_err_skew,
    ar1_rho = out_ar1
  )

  # Effects defaults
  pred_names <- vapply(pred_list, function(p) as.character(p$name %||% ""), character(1))
  intercept_val <- if (is.null(effects$intercept)) 0 else effects$intercept

  fixed_in <- effects$fixed %||% list()
  fixed_obj <- as.list(fixed_in)
  for (i in seq_along(pred_names)) {
    pn <- pred_names[[i]]
    if (nzchar(pn) && is.null(fixed_obj[[pn]])) {
      default_beta <- if (i == 1L) 0.3 else 0.1
      fixed_obj[[pn]] <- default_beta
      add_rationale(
        paste0("effects.fixed.", pn),
        sprintf("Defaulted fixed effect for '%s' to %.2f.", pn, default_beta)
      )
    }
  }

  focal_pred <- if (!is.null(effects$focal_predictor)) {
    effects$focal_predictor
  } else if (length(pred_names) >= 1L && nzchar(pred_names[[1L]])) {
    # Prefer a predictor with role "treatment" or "focal"
    roles <- vapply(pred_list, function(p) as.character(p$role %||% "covariate"), character(1))
    idx <- which(roles %in% c("treatment", "focal"))
    if (length(idx) >= 1L) pred_names[[idx[[1L]]]] else pred_names[[1L]]
  } else {
    NULL
  }

  random_in <- effects$random %||% list()
  rand_intercept <- if (is.null(random_in$intercept)) {
    identical(levels_val, 2L)
  } else {
    random_in$intercept
  }
  rand_slopes <- if (is.null(random_in$slopes) && is_longitudinal) {
    time_preds <- vapply(pred_list, function(p) identical(p$distribution, "time"), logical(1))
    if (any(time_preds)) {
      t_nm <- pred_list[[which(time_preds)[[1L]]]]$name
      add_rationale(paste0("effects.random.slopes.", t_nm, ".sd"), "Defaulted longitudinal random time-slope SD to 0.25.")
      list(list(predictor = t_nm, sd = 0.25))
    } else {
      list()
    }
  } else {
    normalize_slopes(random_in$slopes %||% list())
  }
  rand_corr <- normalize_corr_matrix(random_in$corr_matrix)
  q_rand <- as.integer(isTRUE(rand_intercept)) + length(rand_slopes)
  if (is.null(rand_corr) && q_rand > 1L) {
    rand_corr <- diag(q_rand)
    add_rationale(
      "effects.random.corr_matrix",
      "Defaulted random-effects correlation matrix to identity (uncorrelated random effects)."
    )
  }

  interactions_obj <- normalize_interactions(effects$interactions %||% list())

  effects_obj <- list(
    intercept = intercept_val,
    fixed = fixed_obj,
    interactions = interactions_obj,
    focal_predictor = focal_pred,
    random = list(
      intercept = rand_intercept,
      intercept_sd = random_in$intercept_sd,
      slopes = rand_slopes,
      corr_matrix = rand_corr
    )
  )

  # Missingness defaults
  miss_mech <- missingness$mechanism %||% {
    add_rationale("missingness.mechanism", "Defaulted to complete data ('none'); enable MCAR or MAR to study attrition.")
    "none"
  }
  miss_rate <- if (is.null(missingness$rate)) {
    if (identical(miss_mech, "none")) 0 else 0.15
  } else {
    missingness$rate
  }
  miss_aux <- if (!is.null(missingness$auxiliary)) {
    missingness$auxiliary
  } else if (identical(miss_mech, "MAR") && length(pred_names) >= 1L) {
    add_rationale(
      "missingness.auxiliary",
      sprintf("Defaulted MAR auxiliary driver to '%s'.", pred_names[[1L]])
    )
    pred_names[[1L]]
  } else {
    NULL
  }
  miss_strength <- if (!is.null(missingness$strength)) {
    missingness$strength
  } else if (identical(miss_mech, "MAR")) {
    add_rationale("missingness.strength", "Defaulted MAR log-odds slope to 1.0 (moderate dependence on auxiliary predictor).")
    1.0
  } else {
    NULL
  }
  missingness_obj <- list(
    mechanism = miss_mech,
    rate = miss_rate,
    target = missingness$target %||% "outcome",
    auxiliary = miss_aux,
    strength = miss_strength
  )

  # Estimator defaults
  default_engine <- infer_default_engine(levels_val, out_family, q_rand)
  est_engine <- estimator$engine %||% {
    add_rationale(
      "estimator.engine",
      sprintf("Selected '%s' estimator matching the %d-level %s model.", default_engine, levels_val, out_family)
    )
    default_engine
  }
  est_reml <- if (is.null(estimator$reml)) TRUE else estimator$reml
  est_conf <- if (is.null(estimator$conf_level)) 0.95 else estimator$conf_level
  estimator_obj <- list(
    engine = est_engine,
    reml = est_reml,
    conf_level = est_conf,
    missing_handling = estimator$missing_handling %||% "complete_case"
  )

  # Simulation defaults
  sim_reps <- if (is.null(simulation$n_reps)) {
    add_rationale("simulation.n_reps", "Defaulted to 200 Monte Carlo replications for responsive prototyping; increase to 500-2000 for publication precision.")
    200L
  } else {
    maybe_int(simulation$n_reps)
  }
  sim_seed <- if (is.null(simulation$seed)) {
    add_rationale("simulation.seed", "Set deterministic random seed to 2026 for full reproducibility.")
    2026L
  } else {
    maybe_int(simulation$seed)
  }
  sim_alpha <- if (is.null(simulation$alpha)) {
    add_rationale("simulation.alpha", "Defaulted nominal Type I error rate (alpha) to 0.05 (two-sided).")
    0.05
  } else {
    simulation$alpha
  }
  sim_grid <- normalize_parameter_grid(simulation$parameter_grid %||% list())

  simulation_obj <- list(
    n_reps = sim_reps,
    seed = sim_seed,
    alpha = sim_alpha,
    parameter_grid = sim_grid
  )

  spec <- structure(
    list(
      schema_version = schema_version,
      metadata = list(
        title = title,
        description = description,
        source_prompt = source_prompt,
        tags = as.character(tags %||% character())
      ),
      design_type = design_type,
      hierarchy = hier_obj,
      predictors = pred_list,
      predictor_corr = normalize_corr_matrix(predictor_corr),
      outcome = outcome_obj,
      effects = effects_obj,
      missingness = missingness_obj,
      estimator = estimator_obj,
      simulation = simulation_obj,
      defaults_rationale = rationale
    ),
    class = c("spec_ir", "list")
  )

  if (isTRUE(validate)) {
    assert_valid_spec_ir(spec)
  }

  spec
}

#' Test whether an object is a `spec_ir`
#'
#' @param x Object to test.
#' @return Logical scalar.
#' @export
#' @examples
#' is_spec_ir(new_spec_ir())
#' is_spec_ir(list())
is_spec_ir <- function(x) {
  inherits(x, "spec_ir")
}

#' Coerce a list into a `spec_ir` object
#'
#' Normalizes raw nested list representations (for example from JSON or YAML
#' parsing or external LLM output) into a typed `spec_ir` object without
#' evaluating any strings as R code.
#'
#' @param x A `spec_ir` object or named list matching the IR structure.
#' @param validate Logical; if `TRUE` (default), runs [assert_valid_spec_ir()].
#'
#' @return A `spec_ir` object.
#' @export
#' @examples
#' raw_list <- unclass(new_spec_ir())
#' spec <- as_spec_ir(raw_list)
#' is_spec_ir(spec)
as_spec_ir <- function(x, validate = TRUE) {
  if (!is.list(x)) {
    cli::cli_abort("Cannot coerce object of class {.cls {class(x)}} to {.cls spec_ir}.")
  }

  spec <- normalize_spec_ir(x)
  class(spec) <- c("spec_ir", "list")

  if (isTRUE(validate)) {
    assert_valid_spec_ir(spec)
  }

  spec
}

#' Compute total sample size implied by a `spec_ir`
#'
#' Computes the nominal total number of level-1 observations using double
#' arithmetic to avoid 32-bit integer overflow, returning an integer scalar when
#' within `.Machine$integer.max` and a finite numeric double otherwise.
#'
#' @param spec A `spec_ir` object.
#' @return Integer or finite numeric scalar giving total level-1 observations.
#' @export
#' @examples
#' spec <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 25L, icc = 0.2)
#' )
#' spec_total_n(spec)
spec_total_n <- function(spec) {
  if (!is.list(spec) || is.null(spec$hierarchy)) {
    return(NA_integer_)
  }
  tot <- if (identical(as.integer(spec$hierarchy$levels), 2L)) {
    as.numeric(spec$hierarchy$n_clusters) * as.numeric(spec$hierarchy$cluster_size)
  } else {
    as.numeric(spec$hierarchy$n_obs)
  }
  if (length(tot) != 1L || !is.finite(tot)) {
    return(NA_integer_)
  }
  if (tot <= .Machine$integer.max && tot == floor(tot)) {
    as.integer(tot)
  } else {
    tot
  }
}

#' Compute implied level-2 random intercept SD from ICC and outcome scale
#'
#' @param spec A `spec_ir` object with `hierarchy$levels == 2L`.
#' @return Numeric scalar random intercept standard deviation, or `0` for 1-level specs.
#' @export
#' @examples
#' spec <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2)
#' )
#' spec_implied_intercept_sd(spec)
spec_implied_intercept_sd <- function(spec) {
  if (!identical(as.integer(spec$hierarchy$levels %||% 1L), 2L)) {
    return(0)
  }
  if (!is.null(spec$effects$random$intercept_sd)) {
    return(as.numeric(spec$effects$random$intercept_sd))
  }
  icc <- as.numeric(spec$hierarchy$icc %||% 0)
  if (!is.finite(icc) || icc <= 0 || icc >= 1) {
    return(0)
  }
  level1_var <- outcome_level1_variance(spec$outcome)
  sqrt(level1_var * icc / (1 - icc))
}

outcome_level1_variance <- function(outcome) {
  fam <- outcome$family %||% "gaussian"
  lnk <- outcome$link %||% "identity"
  if (identical(fam, "gaussian")) {
    as.numeric(outcome$residual_sd %||% 1)^2
  } else if (identical(fam, "binomial") && identical(lnk, "logit")) {
    (pi^2) / 3
  } else {
    1.0
  }
}

infer_default_engine <- function(levels_val, family, q_rand) {
  if (identical(levels_val, 2L) && q_rand > 0L) {
    if (identical(family, "gaussian")) "lmer" else "glmer"
  } else {
    if (identical(family, "gaussian")) "lm" else "glm"
  }
}

maybe_int <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x)) {
    as.integer(x)
  } else {
    x
  }
}

normalize_predictor <- function(p) {
  if (!is.list(p)) return(p)
  out <- list(
    name = p$name,
    level = maybe_int(p$level %||% 1L),
    distribution = p$distribution %||% "normal",
    mean = if (is.null(p$mean)) NULL else p$mean,
    sd = if (is.null(p$sd)) NULL else p$sd,
    prob = if (is.null(p$prob)) NULL else p$prob,
    min = if (is.null(p$min)) NULL else p$min,
    max = if (is.null(p$max)) NULL else p$max,
    centering = p$centering %||% "none",
    role = p$role %||% "covariate"
  )
  extra <- setdiff(names(p), names(out))
  for (ek in extra) out[[ek]] <- p[[ek]]
  out
}

normalize_slopes <- function(slopes) {
  if (is.null(slopes) || length(slopes) == 0L) {
    return(list())
  }
  if (is.data.frame(slopes)) {
    return(lapply(seq_len(nrow(slopes)), function(i) {
      list(
        predictor = as.character(slopes$predictor[[i]]),
        sd = slopes$sd[[i]]
      )
    }))
  }
  lapply(unname(as.list(slopes)), function(s) {
    if (!is.list(s)) return(s)
    out <- list(
      predictor = s$predictor,
      sd = s$sd
    )
    extra <- setdiff(names(s), names(out))
    for (ek in extra) out[[ek]] <- s[[ek]]
    out
  })
}

normalize_interactions <- function(interactions) {
  if (is.null(interactions) || length(interactions) == 0L) {
    return(list())
  }
  if (is.data.frame(interactions)) {
    return(lapply(seq_len(nrow(interactions)), function(i) {
      list(
        terms = as.character(unlist(interactions$terms[[i]], use.names = FALSE)),
        beta = interactions$beta[[i]]
      )
    }))
  }
  lapply(unname(as.list(interactions)), function(it) {
    if (!is.list(it)) return(it)
    t_raw <- it$terms
    terms_v <- if (is.list(t_raw)) as.character(unlist(t_raw, use.names = FALSE)) else as.character(t_raw)
    out <- list(terms = terms_v, beta = it$beta)
    extra <- setdiff(names(it), names(out))
    for (ek in extra) out[[ek]] <- it[[ek]]
    out
  })
}

normalize_corr_matrix <- function(m) {
  if (is.null(m)) return(NULL)
  if (is.matrix(m)) {
    return(m)
  }
  if (is.list(m) && length(m) > 0L) {
    # List of row lists/vectors from JSON or YAML
    rows <- lapply(m, function(r) {
      if (is.list(r)) unlist(r, use.names = FALSE) else r
    })
    row_lens <- lengths(rows)
    if (all(vapply(rows, is.numeric, logical(1))) && length(unique(row_lens)) == 1L && row_lens[[1L]] > 0L) {
      return(do.call(rbind, rows))
    }
  }
  m
}

normalize_parameter_grid <- function(grid) {
  if (is.null(grid) || length(grid) == 0L) {
    return(list())
  }
  if (!is.list(grid)) return(grid)
  out <- lapply(grid, function(vals) {
    if (is.list(vals)) {
      u <- unlist(vals, use.names = FALSE)
      if (is.numeric(u)) u else vals
    } else {
      vals
    }
  })
  names(out) <- names(grid)
  out
}

normalize_spec_ir <- function(x) {
  out <- as.list(x)

  if (is.list(out$metadata)) {
    tags <- out$metadata$tags
    if (is.null(tags)) {
      out$metadata$tags <- character()
    } else if (is.list(tags)) {
      out$metadata$tags <- as.character(unlist(tags, use.names = FALSE))
    } else {
      out$metadata$tags <- as.character(tags)
    }
  }

  if (is.list(out$hierarchy)) {
    out$hierarchy$levels <- maybe_int(out$hierarchy$levels)
    out$hierarchy$n_obs <- maybe_int(out$hierarchy$n_obs)
    out$hierarchy$n_clusters <- maybe_int(out$hierarchy$n_clusters)
    out$hierarchy$cluster_size <- maybe_int(out$hierarchy$cluster_size)
    out$hierarchy$time_points <- maybe_int(out$hierarchy$time_points)
    if (is.list(out$hierarchy$time_spacing)) {
      u_ts <- unlist(out$hierarchy$time_spacing, use.names = FALSE)
      if (is.numeric(u_ts)) out$hierarchy$time_spacing <- u_ts
    }
  }

  if (is.data.frame(out$predictors)) {
    df <- out$predictors
    out$predictors <- lapply(seq_len(nrow(df)), function(i) {
      row_list <- lapply(df, function(col) col[[i]])
      normalize_predictor(row_list)
    })
  } else if (is.list(out$predictors)) {
    out$predictors <- lapply(unname(out$predictors), normalize_predictor)
  }

  if ("predictor_corr" %in% names(out)) {
    out$predictor_corr <- normalize_corr_matrix(out$predictor_corr)
  } else {
    out$predictor_corr <- NULL
  }

  if (is.list(out$effects)) {
    if (is.numeric(out$effects$fixed) && !is.null(names(out$effects$fixed))) {
      out$effects$fixed <- as.list(out$effects$fixed)
    }
    if ("interactions" %in% names(out$effects)) {
      out$effects$interactions <- normalize_interactions(out$effects$interactions)
    } else {
      out$effects$interactions <- list()
    }
    if (is.list(out$effects$random)) {
      out$effects$random$slopes <- normalize_slopes(out$effects$random$slopes)
      out$effects$random$corr_matrix <- normalize_corr_matrix(out$effects$random$corr_matrix)
    }
  }

  if (is.list(out$simulation)) {
    out$simulation$n_reps <- maybe_int(out$simulation$n_reps)
    out$simulation$seed <- maybe_int(out$simulation$seed)
    out$simulation$parameter_grid <- normalize_parameter_grid(out$simulation$parameter_grid)
  }

  if (is.null(out$defaults_rationale)) {
    out$defaults_rationale <- list()
  } else if (is.character(out$defaults_rationale) && !is.null(names(out$defaults_rationale))) {
    out$defaults_rationale <- as.list(out$defaults_rationale)
  }

  out
}

#' @export
print.spec_ir <- function(x, ...) {
  v <- validate_spec_ir(x, strict = FALSE)
  status_txt <- if (v$valid) "valid" else sprintf("INVALID (%d error%s)", nrow(v$errors), if (nrow(v$errors) == 1L) "" else "s")
  cli::cli_h2("spec_ir v{x$schema_version %||% '?'} [{status_txt}]")
  cli::cli_text("{.strong Title:} {x$metadata$title %||% '<none>'}")
  cli::cli_text("{.strong Design:} {x$design_type %||% '?'} | {.strong Estimator:} {x$estimator$engine %||% '?'}")

  if (identical(x$design_type, "longitudinal_growth")) {
    cli::cli_text(
      "{.strong Hierarchy:} Longitudinal ({x$hierarchy$n_clusters} {x$hierarchy$cluster_name}s x {x$hierarchy$time_points %||% x$hierarchy$cluster_size} {x$hierarchy$unit_name}s = {spec_total_n(x)} total; ICC = {x$hierarchy$icc})"
    )
  } else if (identical(as.integer(x$hierarchy$levels %||% 1L), 2L)) {
    cli::cli_text(
      "{.strong Hierarchy:} 2-level ({x$hierarchy$n_clusters} {x$hierarchy$cluster_name}s x {x$hierarchy$cluster_size} {x$hierarchy$unit_name}s = {spec_total_n(x)} total; ICC = {x$hierarchy$icc})"
    )
  } else {
    cli::cli_text("{.strong Hierarchy:} 1-level (N = {x$hierarchy$n_obs} {x$hierarchy$unit_name}s)")
  }

  pred_summary <- vapply(x$predictors, function(p) {
    beta <- x$effects$fixed[[p$name]]
    sprintf("%s [L%s, %s, beta=%s]", p$name, p$level, p$distribution, if (is.null(beta)) "?" else format(beta))
  }, character(1))
  ar1_txt <- if (identical(x$design_type, "longitudinal_growth") && identical(x$outcome$family, "gaussian") && !is.null(x$outcome$ar1_rho)) {
    sprintf(", AR(1) rho = %s", format(x$outcome$ar1_rho))
  } else {
    ""
  }
  cli::cli_text("{.strong Outcome:} {x$outcome$name} ({x$outcome$family}, link = {x$outcome$link}{ar1_txt})")
  cli::cli_text("{.strong Predictors:} {paste(pred_summary, collapse = ', ')}")

  if (length(x$effects$random$slopes) > 0L) {
    sl <- vapply(x$effects$random$slopes, function(s) sprintf("%s (sd=%s)", s$predictor, s$sd), character(1))
    cli::cli_text("{.strong Random Slopes:} {paste(sl, collapse = ', ')}")
  }

  if (!identical(x$missingness$mechanism, "none")) {
    cli::cli_text("{.strong Missingness:} {x$missingness$mechanism} ({round(100 * x$missingness$rate, 1)}% on {x$missingness$target})")
  }

  grid_len <- length(x$simulation$parameter_grid %||% list())
  grid_txt <- if (grid_len > 0L) {
    paste(sprintf("%s=[%s]", names(x$simulation$parameter_grid), vapply(x$simulation$parameter_grid, function(v) paste(v, collapse = ","), character(1))), collapse = "; ")
  } else {
    "none"
  }
  cli::cli_text("{.strong Simulation:} {x$simulation$n_reps} reps | alpha = {x$simulation$alpha} | seed = {x$simulation$seed} | grid: {grid_txt}")
  invisible(x)
}

#' @export
summary.spec_ir <- function(object, ...) {
  v <- validate_spec_ir(object, strict = FALSE)
  math <- if (v$valid) spec_to_math(object) else list(formula = "<invalid>", text = "<invalid>")
  structure(
    list(
      title = object$metadata$title,
      design_type = object$design_type,
      levels = object$hierarchy$levels,
      total_n = spec_total_n(object),
      n_predictors = length(object$predictors),
      n_random_slopes = length(object$effects$random$slopes %||% list()),
      estimator = object$estimator$engine,
      n_reps = object$simulation$n_reps,
      grid_cells = if (length(object$simulation$parameter_grid) > 0L) prod(lengths(object$simulation$parameter_grid)) else 1L,
      formula = math$formula,
      valid = v$valid,
      n_errors = nrow(v$errors),
      n_defaults = length(object$defaults_rationale)
    ),
    class = "summary.spec_ir"
  )
}

#' @export
print.summary.spec_ir <- function(x, ...) {
  cli::cli_h3("Summary: {x$title}")
  cli::cli_ul(c(
    "Design: {x$design_type} ({x$levels}-level, total N = {x$total_n})",
    "Model formula: {x$formula}",
    "Estimator: {x$estimator} | Replications: {x$n_reps} x {x$grid_cells} cell(s)",
    "Validation: {if (x$valid) 'PASS' else paste0('FAIL (', x$n_errors, ' errors)')} | Inferred defaults documented: {x$n_defaults}"
  ))
  invisible(x)
}
