#' Translate a natural-language study description into a validated `spec_ir`
#'
#' Compiles an English statistical or simulation study description into a typed
#' `spec_ir` specification using a pluggable translator backend
#' ([translator_rule()], [translator_openai()], or [translator_ollama()]),
#' returns structured clarifying questions for underspecified parameters, and
#' populates transparent plain-language rationales in `spec$defaults_rationale`.
#'
#' @param prompt Character scalar describing the desired statistical study or simulation.
#' @param translator A `spec_translator` backend object (defaults to [translator_rule()]).
#' @param validate Logical; if `TRUE` (default), enforces [assert_valid_spec_ir()].
#'
#' @return An S3 object of class `spec_translation` with elements:
#'   - `spec`: the `spec_ir` object.
#'   - `clarifications`: list of structured clarification objects.
#'   - `validation`: `spec_validation` report.
#'   - `backend`: character scalar naming the translator backend.
#'   - `prompt`: original natural-language prompt.
#'   - `parsed_fields`: character vector of IR paths explicitly extracted from `prompt`.
#' @export
#' @examples
#' res <- nl_to_spec(
#'   "2-level multilevel power simulation with 60 schools, 25 students per school,
#'    ICC 0.2, treatment effect 0.35, random slope for x1, 15% MAR missingness,
#'    500 replications"
#' )
#' print(res)
nl_to_spec <- function(prompt, translator = translator_rule(), validate = TRUE) {
  if (!is_chr_scalar(prompt) || !nzchar(trimws(prompt))) {
    cli::cli_abort("{.arg prompt} must be a non-empty character scalar.")
  }
  if (!inherits(translator, "spec_translator")) {
    cli::cli_abort("{.arg translator} must be a {.cls spec_translator} backend (e.g., {.fn translator_rule}).")
  }

  raw_res <- translate_prompt_backend(translator, prompt)
  val <- validate_spec_ir(raw_res$spec, strict = isTRUE(validate))

  structure(
    list(
      spec = raw_res$spec,
      clarifications = raw_res$clarifications %||% list(),
      validation = val,
      backend = translator$name,
      prompt = prompt,
      parsed_fields = raw_res$parsed_fields %||% character()
    ),
    class = "spec_translation"
  )
}

#' Iteratively refine an existing `spec_ir` via natural language
#'
#' Applies a conversational modification instruction to an existing `spec_ir`
#' (or `spec_translation`), validates the updated specification, and computes a
#' semantic [diff_spec_ir()] highlighting exactly what changed.
#'
#' @param spec A `spec_ir` or `spec_translation` object.
#' @param instruction Character scalar specifying the desired edit (e.g.,
#'   `"increase clusters to 80"`, `"add random slope for x1 with sd 0.25"`,
#'   `"set 20% MAR missingness"`, `"sweep ICC across 0.1, 0.2, 0.3"`).
#' @param translator A `spec_translator` backend (defaults to [translator_rule()]).
#' @param validate Logical; if `TRUE` (default), enforces [assert_valid_spec_ir()].
#'
#' @return An S3 object of class `c("spec_refinement", "spec_translation")` with
#'   elements `spec`, `previous_spec`, `diff`, `clarifications`, `validation`,
#'   `instruction`, `backend`, `prompt`, and `parsed_fields`.
#' @export
#' @examples
#' base <- nl_to_spec("2-level study with 40 schools, 20 students per school, ICC 0.15")
#' ref <- refine_spec(base, "increase clusters to 80 and add 15% MAR missingness")
#' ref$diff
refine_spec <- function(spec, instruction, translator = translator_rule(), validate = TRUE) {
  if (inherits(spec, "spec_translation")) {
    spec <- spec$spec
  }
  if (!is_spec_ir(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls spec_ir} or {.cls spec_translation} object.")
  }
  if (!is_chr_scalar(instruction) || !nzchar(trimws(instruction))) {
    cli::cli_abort("{.arg instruction} must be a non-empty character scalar.")
  }
  if (!inherits(translator, "spec_translator")) {
    cli::cli_abort("{.arg translator} must be a {.cls spec_translator} backend.")
  }

  raw_res <- refine_spec_backend(translator, spec, instruction)
  val <- validate_spec_ir(raw_res$spec, strict = isTRUE(validate))
  diff_obj <- diff_spec_ir(spec, raw_res$spec)

  structure(
    list(
      spec = raw_res$spec,
      previous_spec = spec,
      diff = diff_obj,
      clarifications = raw_res$clarifications %||% list(),
      validation = val,
      instruction = instruction,
      backend = translator$name,
      prompt = spec$metadata$source_prompt,
      parsed_fields = raw_res$parsed_fields %||% character()
    ),
    class = c("spec_refinement", "spec_translation")
  )
}

#' Apply user answers to structured clarification questions
#'
#' Updates a `spec_ir` using a named list of clarification answers (keyed by
#' clarification `id` or dotted IR `path`), updates `defaults_rationale` to
#' record user confirmation, validates the specification, and returns the
#' updated `spec_ir` alongside a semantic `spec_diff`.
#'
#' @param spec A `spec_ir` or `spec_translation` object.
#' @param answers Named list mapping clarification IDs (or IR paths) to chosen values.
#' @param clarifications Optional list of clarification objects (automatically
#'   extracted if `spec` is a `spec_translation`).
#' @param validate Logical; if `TRUE` (default), enforces [assert_valid_spec_ir()].
#'
#' @return A list with `spec`, `diff`, `validation`, and remaining `clarifications`.
#' @export
#' @examples
#' res <- nl_to_spec("2-level power analysis")
#' updated <- answer_clarifications(res, list(n_clusters = 60, icc = 0.25))
#' updated$diff
answer_clarifications <- function(spec, answers, clarifications = NULL, validate = TRUE) {
  if (inherits(spec, "spec_translation")) {
    if (is.null(clarifications)) clarifications <- spec$clarifications
    spec <- spec$spec
  }
  if (!is_spec_ir(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls spec_ir} or {.cls spec_translation} object.")
  }
  if (!is.list(answers) || (length(answers) > 0L && is.null(names(answers)))) {
    cli::cli_abort("{.arg answers} must be a named list.")
  }

  old_spec <- spec
  new_spec <- spec
  clarifs <- clarifications %||% list()

  id_to_path <- list()
  for (cl in clarifs) {
    if (!is.null(cl$id) && !is.null(cl$path)) {
      id_to_path[[cl$id]] <- cl$path
    }
  }

  answered_ids <- character()
  for (nm in names(answers)) {
    val <- answers[[nm]]
    if (is.null(val)) next
    path <- id_to_path[[nm]] %||% nm
    new_spec <- apply_ir_path_value(new_spec, path, val)
    new_spec$defaults_rationale[[path]] <- sprintf("Explicitly confirmed by user (%s).", format_diff_val(val))
    answered_ids <- c(answered_ids, nm)
  }

  new_spec <- as_spec_ir(new_spec, validate = FALSE)
  val_rep <- validate_spec_ir(new_spec, strict = isTRUE(validate))
  diff_obj <- diff_spec_ir(old_spec, new_spec)

  remaining <- Filter(function(cl) {
    !(cl$id %in% answered_ids || cl$path %in% answered_ids)
  }, clarifs)

  list(
    spec = new_spec,
    diff = diff_obj,
    validation = val_rep,
    clarifications = remaining
  )
}

#' @export
print.spec_translation <- function(x, ...) {
  cli::cli_h2("spec_translation [backend: {x$backend}]")
  print(x$spec)
  n_cl <- length(x$clarifications)
  if (n_cl > 0L) {
    cli::cli_h3("Open Clarifications ({n_cl})")
    for (cl in x$clarifications) {
      cli::cli_bullets(c("i" = sprintf("[%s] %s (default: %s)", cl$id, cl$question, format_diff_val(cl$current_default))))
    }
  } else {
    cli::cli_alert_success("No open clarifications required.")
  }
  invisible(x)
}

#' @export
print.spec_refinement <- function(x, ...) {
  cli::cli_h2("spec_refinement [backend: {x$backend}]")
  cli::cli_text("{.strong Instruction:} {x$instruction}")
  print(x$diff)
  invisible(x)
}

# ==============================================================================
# Deterministic Offline Rule-Based NL Parser (translator_rule)
# ==============================================================================

#' @export
translate_prompt_backend.spec_translator_rule <- function(translator, prompt) {
  txt <- gsub("\\s+", " ", trimws(prompt))
  low <- tolower(txt)

  parsed <- character()
  rationale <- list()
  clarifs <- list()

  mark_parsed <- function(path) {
    parsed <<- unique(c(parsed, path))
  }

  add_clarif <- function(id, path, question, current_default, options, rationale_txt) {
    clarifs[[length(clarifs) + 1L]] <<- list(
      id = id,
      path = path,
      question = question,
      current_default = current_default,
      options = options,
      rationale = rationale_txt
    )
  }

  # --- 1. Hierarchy & Design Type ---
  cluster_nouns <- c("schools?", "clusters?", "hospitals?", "clinics?", "classrooms?", "groups?", "teams?", "sites?", "wards?", "centers?", "centres?", "classes?")
  unit_nouns <- c("students?", "patients?", "participants?", "subjects?", "individuals?", "observations?", "pupils?", "employees?", "respondents?", "units?", "children")
  cluster_re <- paste(cluster_nouns, collapse = "|")
  unit_re <- paste(unit_nouns, collapse = "|")

  is_multilevel <- grepl(
    "\\b(2[- ]?level|two[- ]?level|multilevel|multi[- ]?level|hierarchical|nested|mixed[- ]?effects?|mlm|hlm|icc|intraclass|random\\s+(?:intercept|slope))\\b",
    low, perl = TRUE
  ) || grepl(sprintf("\\b\\d+\\s+(?:%s)\\b", cluster_re), low, perl = TRUE) ||
    grepl(sprintf("\\bper\\s+(?:%s)\\b", cluster_re), low, perl = TRUE)

  levels_val <- if (is_multilevel) 2L else 1L
  is_power <- grepl("\\b(power|sample\\s+size\\s+plan|detect\\s+effect)\\b", low, perl = TRUE)
  is_mlm_explicit <- grepl("\\b(two[- ]?level\\s+mlm|2[- ]?level\\s+mlm)\\b", low, perl = TRUE)

  design_type <- if (is_power) {
    "power_analysis"
  } else if (is_mlm_explicit) {
    "two_level_mlm"
  } else if (levels_val == 2L) {
    "multilevel_sim"
  } else {
    "monte_carlo_lm"
  }
  mark_parsed("design_type")
  mark_parsed("hierarchy.levels")

  # Extract cluster & unit names + sizes
  n_obs <- NULL
  n_clusters <- NULL
  cluster_size <- NULL
  cluster_name <- "cluster"
  unit_name <- "observation"
  icc <- NULL

  if (levels_val == 2L) {
    # Cluster count e.g., "60 schools", "J = 50", "50 clusters"
    m_cl <- reg_capture(sprintf("\\b(\\d+)\\s+(%s)\\b", cluster_re), low)
    if (!is.null(m_cl)) {
      n_clusters <- as.integer(m_cl[[1L]])
      cluster_name <- singularize_noun(m_cl[[2L]])
      mark_parsed("hierarchy.n_clusters")
      mark_parsed("hierarchy.cluster_name")
    } else {
      m_j <- reg_capture("\\b(?:j|n_clusters|clusters)\\s*(?:=|of|is)?\\s*(\\d+)\\b", low)
      if (!is.null(m_j)) {
        n_clusters <- as.integer(m_j[[1L]])
        mark_parsed("hierarchy.n_clusters")
      }
    }

    # Cluster size e.g., "25 students per school", "cluster size 20", "n_j = 15", "20 per cluster"
    m_cs <- reg_capture(sprintf("\\b(\\d+)\\s+(%s)\\s+(?:per|in\\s+each|within\\s+each)\\s+(%s)\\b", unit_re, cluster_re), low)
    if (!is.null(m_cs)) {
      cluster_size <- as.integer(m_cs[[1L]])
      unit_name <- singularize_noun(m_cs[[2L]])
      if (!("hierarchy.cluster_name" %in% parsed)) {
        cluster_name <- singularize_noun(m_cs[[3L]])
        mark_parsed("hierarchy.cluster_name")
      }
      mark_parsed("hierarchy.cluster_size")
      mark_parsed("hierarchy.unit_name")
    } else {
      m_cs2 <- reg_capture(sprintf("\\b(\\d+)\\s+(?:(%s)\\s+)?per\\s+(?:%s)\\b", unit_re, cluster_re), low)
      if (!is.null(m_cs2)) {
        cluster_size <- as.integer(m_cs2[[1L]])
        if (nzchar(m_cs2[[2L]])) {
          unit_name <- singularize_noun(m_cs2[[2L]])
          mark_parsed("hierarchy.unit_name")
        }
        mark_parsed("hierarchy.cluster_size")
      } else {
        m_cs3 <- reg_capture("\\b(?:cluster\\s+size|cluster_size|n_j|nj)\\s*(?:=|of|is)?\\s*(\\d+)\\b", low)
        if (!is.null(m_cs3)) {
          cluster_size <- as.integer(m_cs3[[1L]])
          mark_parsed("hierarchy.cluster_size")
        }
      }
    }

    # ICC (supports both proportions "ICC 0.2" and percentages "ICC 20%" / "20% ICC")
    m_icc <- reg_capture("\\b(?:icc|intraclass\\s+correlation)\\s*(?:=|of|is|:)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
    if (is.null(m_icc)) {
      m_icc <- reg_capture("\\b(\\d+(?:\\.\\d+)?)\\s*(%|percent)\\s+(?:icc|intraclass\\s+correlation)\\b", low)
    }
    if (!is.null(m_icc)) {
      raw_icc <- as.numeric(m_icc[[1L]])
      has_pct <- length(m_icc) >= 2L && nzchar(m_icc[[2L]])
      icc <- if (has_pct || raw_icc > 1) raw_icc / 100 else raw_icc
      mark_parsed("hierarchy.icc")
    }

    if (is.null(n_clusters)) {
      n_clusters <- 40L
      rationale[["hierarchy.n_clusters"]] <- "Number of clusters was not specified; defaulted to 40 clusters for stable random-effects estimation."
      add_clarif("n_clusters", "hierarchy.n_clusters",
                 sprintf("How many %ss (level-2 clusters) should be simulated?", cluster_name),
                 n_clusters, c(20L, 40L, 60L, 100L), rationale[["hierarchy.n_clusters"]])
    }
    if (is.null(cluster_size)) {
      cluster_size <- 20L
      rationale[["hierarchy.cluster_size"]] <- "Cluster size was not specified; defaulted to 20 units per cluster."
      add_clarif("cluster_size", "hierarchy.cluster_size",
                 sprintf("How many %ss per %s (level-1 cluster size)?", unit_name, cluster_name),
                 cluster_size, c(10L, 20L, 30L, 50L), rationale[["hierarchy.cluster_size"]])
    }
    if (is.null(icc)) {
      icc <- 0.15
      rationale[["hierarchy.icc"]] <- "ICC was not specified; defaulted to 0.15 (moderate clustering)."
      add_clarif("icc", "hierarchy.icc",
                 "What intraclass correlation (ICC) do you expect?",
                 icc, c(0.05, 0.15, 0.25, 0.35), rationale[["hierarchy.icc"]])
    }
  } else {
    # Single-level sample size
    m_n <- reg_capture(sprintf("\\b(?:sample\\s+size|n_obs|n)\\s*(?:=|of|is)?\\s*(\\d+)\\b"), low)
    if (!is.null(m_n)) {
      n_obs <- as.integer(m_n[[1L]])
      mark_parsed("hierarchy.n_obs")
    } else {
      m_n2 <- reg_capture(sprintf("\\b(\\d+)\\s+(%s)\\b", unit_re), low)
      if (!is.null(m_n2)) {
        n_obs <- as.integer(m_n2[[1L]])
        unit_name <- singularize_noun(m_n2[[2L]])
        mark_parsed("hierarchy.n_obs")
        mark_parsed("hierarchy.unit_name")
      }
    }
    if (is.null(n_obs)) {
      n_obs <- 200L
      rationale[["hierarchy.n_obs"]] <- "Sample size was not specified; defaulted to N = 200 observations."
      add_clarif("n_obs", "hierarchy.n_obs",
                 "What total sample size (N) should be simulated?",
                 n_obs, c(100L, 200L, 500L, 1000L), rationale[["hierarchy.n_obs"]])
    }
  }

  # Unbalanced cluster sizes (2-level only)
  cluster_size_dist <- if (levels_val == 2L) "fixed" else NULL
  cluster_size_sd <- NULL
  if (levels_val == 2L) {
    m_cssd <- reg_capture("\\bcluster\\s+size\\s+sd\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
    if (grepl("\\bpoisson\\s+cluster\\s+sizes?\\b", low, perl = TRUE)) {
      cluster_size_dist <- "poisson"
      mark_parsed("hierarchy.cluster_size_dist")
    } else if (!is.null(m_cssd)) {
      cluster_size_dist <- "normal"
      cluster_size_sd <- as.numeric(m_cssd[[1L]])
      mark_parsed("hierarchy.cluster_size_dist")
      mark_parsed("hierarchy.cluster_size_sd")
    } else if (grepl("\\b(unbalanced|unequal|varying)\\s+cluster\\s+sizes?\\b", low, perl = TRUE)) {
      cluster_size_dist <- "normal"
      cluster_size_sd <- max(1, round(0.25 * as.numeric(cluster_size), 2))
      mark_parsed("hierarchy.cluster_size_dist")
      rationale[["hierarchy.cluster_size_sd"]] <- sprintf("Unbalanced cluster sizes requested without explicit SD; defaulted cluster_size_sd to %s (25%% of mean).", format(cluster_size_sd))
    }
  }

  # --- 2. Outcome & Family ---
  out_family <- "gaussian"
  out_link <- "identity"
  if (grepl("\\b(binary\\s+outcome|dichotomous|logistic|binomial|logit|probit)\\b", low, perl = TRUE)) {
    out_family <- "binomial"
    out_link <- if (grepl("\\bprobit\\b", low)) "probit" else "logit"
    mark_parsed("outcome.family")
    mark_parsed("outcome.link")
  } else if (grepl("\\b(count\\s+outcome|poisson(?!\\s+cluster\\s+sizes?))\\b", low, perl = TRUE)) {
    out_family <- "poisson"
    out_link <- "log"
    mark_parsed("outcome.family")
    mark_parsed("outcome.link")
  } else {
    rationale[["outcome.family"]] <- "Outcome distribution was not specified; defaulted to Gaussian (continuous outcome with identity link)."
  }

  residual_sd <- NULL
  error_dist <- NULL
  error_df <- NULL
  error_skew <- NULL
  if (out_family == "gaussian") {
    m_rsd <- reg_capture("\\b(?:residual\\s+sd|error\\s+sd|sigma)\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
    m_rvar <- reg_capture("\\b(?:residual\\s+variance|error\\s+variance)\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
    if (!is.null(m_rsd)) {
      residual_sd <- as.numeric(m_rsd[[1L]])
      mark_parsed("outcome.residual_sd")
    } else if (!is.null(m_rvar)) {
      residual_sd <- sqrt(as.numeric(m_rvar[[1L]]))
      mark_parsed("outcome.residual_sd")
    } else {
      residual_sd <- 1.0
      rationale[["outcome.residual_sd"]] <- "Residual SD was not specified; defaulted to 1.0 so effect sizes are interpretable in SD units."
    }

    error_dist <- "normal"
    if (grepl("\\b(heavy[- ]?tailed|t[- ]?distributed|student[- ]?t)\\s+(?:errors?|residuals?)\\b", low, perl = TRUE)) {
      error_dist <- "t"
      m_df <- reg_capture("\\b(?:df|degrees\\s+of\\s+freedom)\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
      error_df <- if (!is.null(m_df) && as.numeric(m_df[[1L]]) > 2) {
        mark_parsed("outcome.error_df")
        as.numeric(m_df[[1L]])
      } else {
        rationale[["outcome.error_df"]] <- "Heavy-tailed t errors requested without explicit df; defaulted error_df to 5."
        5
      }
      mark_parsed("outcome.error_dist")
    } else if (grepl("\\b(skewed|asymmetric)\\s+(?:errors?|residuals?)\\b|\\berror\\s+skew(?:ness)?\\b", low, perl = TRUE)) {
      error_dist <- "skewed"
      m_sk <- reg_capture("\\b(?:error\\s+)?skew(?:ness)?\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
      error_skew <- if (!is.null(m_sk) && as.numeric(m_sk[[1L]]) != 0 && abs(as.numeric(m_sk[[1L]])) <= 4) {
        mark_parsed("outcome.error_skew")
        as.numeric(m_sk[[1L]])
      } else {
        rationale[["outcome.error_skew"]] <- "Skewed errors requested without explicit skewness; defaulted error_skew to 1.5."
        1.5
      }
      mark_parsed("outcome.error_dist")
    }
  }

  # --- 3. Predictors & Fixed Effects ---
  predictors <- list()
  fixed_effects <- list()
  has_pred <- function(nm) any(vapply(predictors, function(p) identical(p$name, nm), logical(1)))
  add_pred <- function(p, beta = NULL) {
    if (!has_pred(p$name)) {
      predictors[[length(predictors) + 1L]] <<- p
    }
    if (!is.null(beta)) {
      fixed_effects[[p$name]] <<- as.numeric(beta)
    }
  }

  # Treatment / intervention predictor
  has_treatment <- grepl("\\b(treatment|intervention|trt|experimental\\s+group)\\b", low, perl = TRUE)
  if (has_treatment) {
    trt_level <- if (levels_val == 2L && grepl(sprintf("\\b(level[- ]?2|cluster[- ]?(?:level|randomized)|%s[- ]?level)\\s+(?:treatment|intervention|trt)\\b", cluster_name), low, perl = TRUE)) {
      2L
    } else {
      1L
    }
    m_trt_raw <- reg_capture("\\b(negative\\s+)?(?:treatment|intervention|trt)\\s+(?:effect(?:\\s+size)?|beta|coef(?:ficient)?)\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
    if (is.null(m_trt_raw)) {
      m_trt_raw <- reg_capture("\\b(negative\\s+)?effect\\s+size\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
    }
    m_trt <- if (!is.null(m_trt_raw)) {
      val_t <- as.numeric(m_trt_raw[[2L]])
      if (nzchar(m_trt_raw[[1L]]) && val_t > 0) val_t <- -val_t
      as.character(val_t)
    } else {
      NULL
    }
    trt_beta <- if (!is.null(m_trt)) {
      mark_parsed("effects.fixed.treatment")
      as.numeric(m_trt[[1L]])
    } else {
      rationale[["effects.fixed.treatment"]] <- "Treatment effect size was not specified; defaulted to 0.35 (small-to-medium effect)."
      add_clarif("focal_effect", "effects.fixed.treatment",
                 "What true treatment effect size should be used?",
                 0.35, c(0.2, 0.35, 0.5, 0.8), rationale[["effects.fixed.treatment"]])
      0.35
    }
    m_tprev <- reg_capture("\\b(?:treatment|intervention|trt)\\s+(?:prevalence|prob(?:ability)?|allocation)\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
    trt_prob <- if (!is.null(m_tprev)) {
      rp <- as.numeric(m_tprev[[1L]])
      if (rp > 1 || (length(m_tprev) >= 2L && nzchar(m_tprev[[2L]]))) rp / 100 else rp
    } else {
      0.5
    }
    if (trt_prob <= 0 || trt_prob >= 1) trt_prob <- 0.5
    add_pred(
      spec_predictor("treatment", level = trt_level, distribution = "binary", prob = trt_prob, centering = "none", role = "treatment"),
      beta = trt_beta
    )
    mark_parsed("predictors.treatment")
    if (levels_val == 2L && !grepl("\\b(level[- ]?[12]|cluster[- ]?(?:level|randomized)|student[- ]?level|unit[- ]?level)\\b", low, perl = TRUE)) {
      rationale[["predictors.treatment.level"]] <- "Treatment assignment level was not stated; defaulted to level-1 (within-cluster randomization)."
      add_clarif(
        "treatment_level", "predictors.treatment.level",
        sprintf("Is 'treatment' randomized within %ss (level 1) or at the %s level (level 2)?", cluster_name, cluster_name),
        1L, list(1L, 2L), rationale[["predictors.treatment.level"]]
      )
    }
  }

  # Detect explicitly named predictors (x1..x9, z1..z9, ses, age, baseline, pretest, income, score, time)
  named_matches <- regmatches(low, gregexpr("\\b(x[1-9]|z[1-9]|ses|age|baseline|pretest|income|time)\\b", low, perl = TRUE))[[1L]]
  named_matches <- unique(named_matches)
  for (nm in named_matches) {
    # Check if level-2 explicitly mentioned near predictor
    p_lvl <- if (levels_val == 2L && grepl(sprintf("\\b(?:level[- ]?2|cluster[- ]?level)\\s+%s\\b", nm), low, perl = TRUE)) 2L else 1L
    p_dist <- if (grepl(sprintf("\\bbinary\\s+%s\\b", nm), low, perl = TRUE)) {
      "binary"
    } else if (grepl(sprintf("\\buniform\\s+%s\\b", nm), low, perl = TRUE)) {
      "uniform"
    } else {
      "normal"
    }
    p_cent <- if (levels_val == 2L && p_lvl == 1L && grepl("\\b(group[- ]?mean|cluster[- ]?mean)\\s+center", low, perl = TRUE)) {
      "group_mean"
    } else if (grepl("\\bgrand[- ]?mean\\s+center", low, perl = TRUE)) {
      "grand_mean"
    } else {
      "none"
    }
    m_b <- reg_capture(sprintf("\\b%s\\s+(?:effect|slope|beta|coef(?:ficient)?)\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", nm), low)
    if (is.null(m_b)) {
      m_b <- reg_capture(sprintf("\\b(?:effect|slope|beta)\\s+(?:of|for)\\s+%s\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", nm), low)
    }
    if (is.null(m_b)) {
      m_b <- reg_capture(sprintf("\\b%s\\s*=\\s*(-?\\d+(?:\\.\\d+)?)\\b", nm), low)
    }
    b_val <- if (!is.null(m_b)) {
      mark_parsed(paste0("effects.fixed.", nm))
      as.numeric(m_b[[1L]])
    } else if (!has_treatment && identical(nm, named_matches[[1L]])) {
      m_gen <- reg_capture("\\b(negative\\s+)?(?:effect(?:\\s+size)?|beta|slope)\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
      if (!is.null(m_gen)) {
        mark_parsed(paste0("effects.fixed.", nm))
        gv <- as.numeric(m_gen[[2L]])
        if (nzchar(m_gen[[1L]]) && gv > 0) gv <- -gv
        gv
      } else {
        rationale[[paste0("effects.fixed.", nm)]] <- sprintf("Fixed effect for '%s' was not specified; defaulted to 0.30.", nm)
        0.30
      }
    } else {
      rationale[[paste0("effects.fixed.", nm)]] <- sprintf("Fixed effect for covariate '%s' was not specified; defaulted to 0.20.", nm)
      0.20
    }
    role_val <- if (!has_treatment && identical(nm, named_matches[[1L]])) "focal" else "covariate"
    add_pred(
      spec_predictor(nm, level = p_lvl, distribution = p_dist, centering = p_cent, role = role_val),
      beta = b_val
    )
    mark_parsed(paste0("predictors.", nm))
  }

  # Handle "k covariates" or "k predictors"
  m_ncov <- reg_capture("\\b([2-5])\\s+(?:continuous\\s+|normal\\s+)?(?:covariates|predictors)\\b", low)
  if (!is.null(m_ncov)) {
    k_cov <- as.integer(m_ncov[[1L]])
    for (k in seq_len(k_cov)) {
      nm <- paste0("x", k)
      if (!has_pred(nm)) {
        add_pred(spec_predictor(nm, level = 1L, distribution = "normal", role = if (length(predictors) == 0L) "focal" else "covariate"), beta = 0.2)
        rationale[[paste0("effects.fixed.", nm)]] <- sprintf("Defaulted fixed effect for generated covariate '%s' to 0.20.", nm)
      }
    }
  }

  # Ensure at least 1 predictor exists
  if (length(predictors) == 0L) {
    m_gen <- reg_capture("\\b(?:effect(?:\\s+size)?|beta|slope)\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
    b1 <- if (!is.null(m_gen)) {
      mark_parsed("effects.fixed.x1")
      as.numeric(m_gen[[1L]])
    } else {
      rationale[["effects.fixed.x1"]] <- "Focal effect size was not specified; defaulted to 0.30."
      add_clarif("focal_effect", "effects.fixed.x1",
                 "What true effect size (beta) should be simulated for the focal predictor?",
                 0.30, c(0.15, 0.30, 0.50, 0.80), rationale[["effects.fixed.x1"]])
      0.30
    }
    p_dist <- if (grepl("\\bbinary\\s+predictor\\b", low)) "binary" else "normal"
    add_pred(spec_predictor("x1", level = 1L, distribution = p_dist, role = "focal"), beta = b1)
    rationale[["predictors"]] <- "No predictor names were stated; created default focal predictor 'x1'."
  }

  focal_predictor <- if (has_treatment) "treatment" else predictors[[1L]]$name

  # Optional two-way / cross-level interaction terms
  interactions <- list()
  m_int <- reg_capture("\\b(?:cross[- ]?level\\s+)?interaction\\s+(?:between\\s+)?([a-z][a-z0-9_]*)\\s*(?:and|:|\\*)\\s*([a-z][a-z0-9_]*)(?:\\s*(?:effect|beta|of|=|is)?\\s*(-?\\d+(?:\\.\\d+)?))?", low)
  if (is.null(m_int)) {
    m_int <- reg_capture("\\b([a-z][a-z0-9_]*):([a-z][a-z0-9_]*)\\s*(?:interaction)?\\s*(?:effect|beta|of|=|is)?\\s*(-?\\d+(?:\\.\\d+)?)?", low)
  }
  if (!is.null(m_int)) {
    t1 <- m_int[[1L]]
    t2 <- m_int[[2L]]
    if (is_valid_r_ident(t1) && is_valid_r_ident(t2) && !identical(t1, t2)) {
      if (!has_pred(t1)) {
        add_pred(spec_predictor(t1, level = 1L, distribution = "normal", role = "covariate"), beta = 0.2)
      }
      if (!has_pred(t2)) {
        t2_lvl <- if (levels_val == 2L && (grepl("cross[- ]?level", low) || grepl("^z[1-9]$", t2))) 2L else 1L
        add_pred(spec_predictor(t2, level = t2_lvl, distribution = "normal", role = "covariate"), beta = 0.2)
      }
      ibeta <- if (length(m_int) >= 3L && nzchar(m_int[[3L]])) {
        as.numeric(m_int[[3L]])
      } else {
        m_ib <- reg_capture("\\binteraction\\s+(?:effect|beta)\\s*(?:=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
        if (!is.null(m_ib)) as.numeric(m_ib[[1L]]) else 0.25
      }
      p_names_now <- vapply(predictors, `[[`, character(1), "name")
      ct <- canonical_interaction_terms(c(t1, t2), p_names_now)
      interactions[[1L]] <- spec_interaction(ct[[1L]], ct[[2L]], beta = ibeta)
      mark_parsed("effects.interactions")
      if (grepl("\\bpower\\s+(?:(?:analysis|study|simulation)\\s+)?(?:for|to\\s+detect)\\s+(?:the\\s+)?(?:cross[- ]?level\\s+)?interaction\\b", low, perl = TRUE)) {
        focal_predictor <- paste(ct, collapse = ":")
      }
    }
  }

  # Optional predictor correlation matrix (same-level predictors)
  pred_corr <- NULL
  m_pcorr <- reg_capture("\\b(?:predictor|covariate)\\s+cor(?:relation)?\\s*(?:=|of|is)?\\s*(-?0?\\.\\d+)\\b", low)
  if (is.null(m_pcorr)) {
    m_pcorr <- reg_capture("\\bcorrelated\\s+(?:predictors|covariates)\\s+(?:with\\s+)?(?:r|rho|cor(?:relation)?)\\s*(?:=|of|is)?\\s*(-?0?\\.\\d+)\\b", low)
  }
  if (!is.null(m_pcorr) && length(predictors) >= 2L) {
    rho_p <- as.numeric(m_pcorr[[1L]])
    pred_corr <- build_same_level_corr_matrix(predictors, rho_p)
    if (!is.null(pred_corr)) mark_parsed("predictor_corr")
  }

  # --- 4. Random Effects (Slopes & Correlation) ---
  rand_slopes <- list()
  rand_corr <- NULL
  if (levels_val == 2L && grepl("\\brandom\\s+slopes?\\b", low, perl = TRUE)) {
    # Look for "random slope(s) for x1 (and x2)"
    m_sl <- reg_capture("\\brandom\\s+slopes?\\s+(?:for|on)\\s+([a-z][a-z0-9_]*)(?:\\s+and\\s+([a-z][a-z0-9_]*))?", low)
    slope_targets <- character()
    if (!is.null(m_sl)) {
      slope_targets <- Filter(nzchar, m_sl)
    } else {
      # Default to first level-1 predictor
      l1_preds <- Filter(function(p) identical(as.integer(p$level), 1L), predictors)
      if (length(l1_preds) > 0L) slope_targets <- l1_preds[[1L]]$name
    }

    m_ssd <- reg_capture("\\brandom\\s+slope(?:s)?\\s+(?:for\\s+[a-z][a-z0-9_]*\\s+)?(?:with\\s+)?sd\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
    if (is.null(m_ssd)) {
      m_ssd <- reg_capture("\\bslope\\s+sd\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
    }
    slope_sd <- if (!is.null(m_ssd)) {
      mark_parsed("effects.random.slopes")
      as.numeric(m_ssd[[1L]])
    } else {
      0.20
    }

    for (st in unique(slope_targets)) {
      if (!is_valid_r_ident(st)) next
      if (!has_pred(st)) {
        add_pred(spec_predictor(st, level = 1L, distribution = "normal", role = "covariate"), beta = 0.2)
        rationale[[paste0("predictors.", st)]] <- sprintf("Added level-1 normal predictor '%s' to support the requested random slope.", st)
      } else {
        # Ensure level 1 for identifiability
        for (idx in seq_along(predictors)) {
          if (identical(predictors[[idx]]$name, st)) predictors[[idx]]$level <- 1L
        }
      }
      rand_slopes[[length(rand_slopes) + 1L]] <- list(predictor = st, sd = slope_sd)
      if (is.null(m_ssd)) {
        rpath <- paste0("effects.random.slopes.", st, ".sd")
        rationale[[rpath]] <- sprintf("Random slope SD for '%s' was not specified; defaulted to 0.20.", st)
        add_clarif(
          paste0("slope_sd_", st), rpath,
          sprintf("What random slope standard deviation (SD) should be used for '%s'?", st),
          slope_sd, c(0.10, 0.20, 0.35, 0.50), rationale[[rpath]]
        )
      }
    }

    q_rand <- 1L + length(rand_slopes)
    if (q_rand > 1L) {
      m_cor <- reg_capture("\\b(?:random[- ]?effects?\\s+)?cor(?:relation)?\\s*(?:=|of|is)?\\s*(-?0?\\.\\d+)\\b", low)
      if (!is.null(m_cor)) {
        rho <- as.numeric(m_cor[[1L]])
        if (abs(rho) < 0.95) {
          rand_corr <- matrix(rho, nrow = q_rand, ncol = q_rand)
          diag(rand_corr) <- 1
          mark_parsed("effects.random.corr_matrix")
        }
      }
      if (is.null(rand_corr)) {
        rand_corr <- diag(q_rand)
        rationale[["effects.random.corr_matrix"]] <- "Random-effects correlation was not specified; defaulted to 0 (uncorrelated intercept and slope)."
      }
    }
  }

  # --- 5. Missingness ---
  miss_mech <- "none"
  miss_rate <- 0
  miss_aux <- NULL
  miss_strength <- NULL

  has_mar <- grepl("\\b(mar|missing\\s+at\\s+random)\\b", low, perl = TRUE)
  has_mcar <- grepl("\\b(mcar|missing\\s+completely\\s+at\\s+random)\\b", low, perl = TRUE)
  has_miss_generic <- grepl("\\b(missingness|missing\\s+data|attrition|dropout|missing\\s+rate)\\b", low, perl = TRUE)

  if (has_mar || has_mcar || has_miss_generic) {
    miss_mech <- if (has_mar) "MAR" else if (has_mcar) "MCAR" else "MAR"
    if (!has_mar && !has_mcar) {
      rationale[["missingness.mechanism"]] <- "Missingness was requested without specifying mechanism; defaulted to MAR."
      add_clarif("missing_mechanism", "missingness.mechanism",
                 "Which missingness mechanism should be simulated?",
                 "MAR", c("MCAR", "MAR"), rationale[["missingness.mechanism"]])
    } else {
      mark_parsed("missingness.mechanism")
    }

    m_pct <- reg_capture("\\b(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\s*(?:mar|mcar)?\\s*(?:missingness|missing|attrition|dropout)\\b", low)
    if (is.null(m_pct)) {
      m_pct <- reg_capture("\\b(?:missingness|missing\\s+rate|attrition|dropout)\\s*(?:of|=|is)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
    }
    if (!is.null(m_pct)) {
      raw_r <- as.numeric(m_pct[[1L]])
      has_pct <- length(m_pct) >= 2L && nzchar(m_pct[[2L]])
      miss_rate <- if (has_pct || raw_r > 1) raw_r / 100 else raw_r
      if (miss_rate >= 0.95) miss_rate <- 0.90
      if (miss_rate <= 0) miss_rate <- 0.15
      mark_parsed("missingness.rate")
    } else {
      miss_rate <- 0.15
      rationale[["missingness.rate"]] <- "Missingness proportion was not specified; defaulted to 15%."
      add_clarif("missing_rate", "missingness.rate",
                 "What proportion of outcome observations should be missing?",
                 0.15, c(0.10, 0.15, 0.25, 0.40), rationale[["missingness.rate"]])
    }

    if (miss_mech == "MAR") {
      # Prefer a continuous covariate if available, else first predictor
      cont_preds <- Filter(function(p) identical(p$distribution, "normal"), predictors)
      miss_aux <- if (length(cont_preds) > 0L) cont_preds[[1L]]$name else predictors[[1L]]$name
      miss_strength <- 1.0
      rationale[["missingness.auxiliary"]] <- sprintf("Selected '%s' as the observed auxiliary predictor driving MAR missingness.", miss_aux)
      rationale[["missingness.strength"]] <- "Defaulted MAR log-odds slope to 1.0 (moderate dependence on auxiliary variable)."
    }
  } else {
    rationale[["missingness.mechanism"]] <- "No missingness was mentioned; defaulted to complete data ('none')."
  }

  # --- 6. Simulation settings & Parameter Grid ---
  m_reps <- reg_capture("\\b(\\d+)\\s*(?:replications?|reps?|simulations?|sims?|iterations?|mc\\s+runs?)\\b", low)
  if (is.null(m_reps)) {
    m_reps <- reg_capture("\\b(?:n_reps|replications|reps)\\s*(?:=|of|is)?\\s*(\\d+)\\b", low)
  }
  n_reps <- if (!is.null(m_reps)) {
    mark_parsed("simulation.n_reps")
    as.integer(m_reps[[1L]])
  } else {
    rationale[["simulation.n_reps"]] <- "Number of replications was not specified; defaulted to 200 replications for fast exploration."
    200L
  }

  m_alpha <- reg_capture("\\balpha\\s*(?:=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
  if (is.null(m_alpha)) {
    m_alpha <- reg_capture("\\b(\\d+(?:\\.\\d+)?)\\s*(%|percent)\\s+significance\\b", low)
  }
  alpha_val <- if (!is.null(m_alpha)) {
    mark_parsed("simulation.alpha")
    ra <- as.numeric(m_alpha[[1L]])
    if ((length(m_alpha) >= 2L && nzchar(m_alpha[[2L]])) || ra >= 1) ra / 100 else ra
  } else {
    rationale[["simulation.alpha"]] <- "Significance level alpha was not specified; defaulted to 0.05 (two-sided)."
    0.05
  }

  m_seed <- reg_capture("\\bseed\\s*(?:=|of|is)?\\s*(\\d+)\\b", low)
  seed_val <- if (!is.null(m_seed)) {
    mark_parsed("simulation.seed")
    as.integer(m_seed[[1L]])
  } else {
    rationale[["simulation.seed"]] <- "Random seed was not specified; set deterministic seed 2026 for reproducibility."
    2026L
  }

  param_grid <- extract_grid_from_text(low, levels_val, miss_mech, out_family)
  if (length(param_grid) > 0L) {
    mark_parsed("simulation.parameter_grid")
  }

  # --- 7. Estimator ---
  q_rand_total <- if (levels_val == 2L) 1L + length(rand_slopes) else 0L
  engine <- infer_default_engine(levels_val, out_family, q_rand_total)
  reml_val <- !grepl("\\b(maximum\\s+likelihood|\\bml\\s+estimation|\\buse\\s+ml\\b)\\b", low, perl = TRUE)
  rationale[["estimator.engine"]] <- sprintf("Selected '%s' estimator matching the %d-level %s model specification.", engine, levels_val, out_family)

  # Title synthesis
  title_str <- synthesize_title(design_type, levels_val, out_family, cluster_name, length(rand_slopes) > 0L, miss_mech)

  hier_arg <- if (levels_val == 2L) {
    list(
      levels = 2L,
      n_clusters = n_clusters,
      cluster_size = cluster_size,
      cluster_size_dist = cluster_size_dist,
      cluster_size_sd = cluster_size_sd,
      cluster_name = cluster_name,
      unit_name = unit_name,
      icc = icc
    )
  } else {
    list(
      levels = 1L,
      n_obs = n_obs,
      unit_name = unit_name
    )
  }

  spec <- new_spec_ir(
    design_type = design_type,
    title = title_str,
    description = sprintf("Compiled from natural-language prompt via rule-based translator."),
    source_prompt = prompt,
    tags = unique(c(design_type, paste0(levels_val, "-level"), out_family, if (miss_mech != "none") tolower(miss_mech))),
    hierarchy = hier_arg,
    predictors = predictors,
    predictor_corr = pred_corr,
    outcome = list(
      name = "y", family = out_family, link = out_link,
      residual_sd = residual_sd, error_dist = error_dist,
      error_df = error_df, error_skew = error_skew
    ),
    effects = list(
      intercept = 0,
      fixed = fixed_effects,
      interactions = interactions,
      focal_predictor = focal_predictor,
      random = list(
        intercept = (levels_val == 2L),
        intercept_sd = NULL,
        slopes = rand_slopes,
        corr_matrix = rand_corr
      )
    ),
    missingness = list(
      mechanism = miss_mech,
      rate = miss_rate,
      target = "outcome",
      auxiliary = miss_aux,
      strength = miss_strength
    ),
    estimator = list(
      engine = engine,
      reml = reml_val,
      conf_level = 1 - alpha_val,
      missing_handling = "complete_case"
    ),
    simulation = list(
      n_reps = n_reps,
      seed = seed_val,
      alpha = alpha_val,
      parameter_grid = param_grid
    ),
    defaults_rationale = rationale,
    validate = FALSE
  )

  list(
    spec = spec,
    clarifications = clarifs,
    parsed_fields = parsed
  )
}

#' @export
refine_spec_backend.spec_translator_rule <- function(translator, spec, instruction) {
  txt <- gsub("\\s+", " ", trimws(instruction))
  low <- tolower(txt)
  s <- spec
  parsed <- character()
  clarifs <- list()

  mark <- function(p) parsed <<- unique(c(parsed, p))

  # 1. Switch between 1-level and 2-level
  if (grepl("\\b(switch\\s+to\\s+2[- ]?level|make\\s+(?:it\\s+)?multilevel|add\\s+clustering)\\b", low, perl = TRUE) &&
      identical(as.integer(s$hierarchy$levels), 1L)) {
    s$hierarchy$levels <- 2L
    s$hierarchy$n_obs <- NULL
    s$hierarchy$n_clusters <- 40L
    s$hierarchy$cluster_size <- 20L
    s$hierarchy$cluster_name <- "cluster"
    s$hierarchy$icc <- 0.15
    s$effects$random$intercept <- TRUE
    if (s$design_type == "monte_carlo_lm") s$design_type <- "multilevel_sim"
    s$estimator$engine <- infer_default_engine(2L, s$outcome$family, 1L)
    s$simulation$parameter_grid$n_obs <- NULL
    mark("hierarchy.levels")
  } else if (grepl("\\b(switch\\s+to\\s+1[- ]?level|make\\s+(?:it\\s+)?single[- ]?level|remove\\s+clustering)\\b", low, perl = TRUE) &&
             identical(as.integer(s$hierarchy$levels), 2L)) {
    s$hierarchy$levels <- 1L
    s$hierarchy$n_obs <- max(50L, as.integer(spec_total_n(s)))
    s$hierarchy$n_clusters <- NULL
    s$hierarchy$cluster_size <- NULL
    s$hierarchy$cluster_size_dist <- NULL
    s$hierarchy$cluster_size_sd <- NULL
    s$hierarchy$cluster_name <- NULL
    s$hierarchy$icc <- NULL
    for (idx in seq_along(s$predictors)) {
      s$predictors[[idx]]$level <- 1L
      if (identical(s$predictors[[idx]]$centering, "group_mean")) {
        s$predictors[[idx]]$centering <- "grand_mean"
      }
    }
    s$effects$random$intercept <- FALSE
    s$effects$random$intercept_sd <- NULL
    s$effects$random$slopes <- list()
    s$effects$random$corr_matrix <- NULL
    if (s$design_type %in% c("multilevel_sim", "two_level_mlm")) s$design_type <- "monte_carlo_lm"
    s$estimator$engine <- infer_default_engine(1L, s$outcome$family, 0L)
    s$simulation$parameter_grid$n_clusters <- NULL
    s$simulation$parameter_grid$cluster_size <- NULL
    s$simulation$parameter_grid$icc <- NULL
    mark("hierarchy.levels")
  }

  # 2. Hierarchy numeric updates
  if (identical(as.integer(s$hierarchy$levels), 2L)) {
    m_cl <- reg_capture("\\b(?:clusters?|n_clusters|schools?|hospitals?|clinics?|classrooms?|groups?)\\s+(?:to|=|of)?\\s*(\\d+)\\b", low)
    if (is.null(m_cl)) {
      m_cl <- reg_capture("\\b(?:use|with|set)\\s+(\\d+)\\s+(?:clusters?|schools?|hospitals?|clinics?|classrooms?|groups?)\\b", low)
    }
    if (!is.null(m_cl)) {
      s$hierarchy$n_clusters <- as.integer(m_cl[[1L]])
      mark("hierarchy.n_clusters")
    }

    m_cs <- reg_capture("\\b(?:cluster\\s+size|cluster_size|n_j)\\s+(?:to|=|of)?\\s*(\\d+)\\b", low)
    if (is.null(m_cs)) {
      m_cs <- reg_capture("\\b(\\d+)\\s+(?:students?|patients?|units?|participants?|observations?)\\s+per\\s+(?:cluster|school|hospital|clinic|classroom|group)\\b", low)
    }
    if (!is.null(m_cs)) {
      s$hierarchy$cluster_size <- as.integer(m_cs[[1L]])
      mark("hierarchy.cluster_size")
    }

    m_icc <- reg_capture("\\b(?:icc|intraclass\\s+correlation)\\s*(?:to|=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
    if (!is.null(m_icc)) {
      raw_icc <- as.numeric(m_icc[[1L]])
      has_pct <- length(m_icc) >= 2L && nzchar(m_icc[[2L]])
      s$hierarchy$icc <- if (has_pct || raw_icc > 1) raw_icc / 100 else raw_icc
      s$effects$random$intercept_sd <- NULL
      mark("hierarchy.icc")
    }

    if (grepl("\\bfixed\\s+cluster\\s+sizes?\\b", low, perl = TRUE)) {
      s$hierarchy$cluster_size_dist <- "fixed"
      s$hierarchy$cluster_size_sd <- NULL
      mark("hierarchy.cluster_size_dist")
    } else if (grepl("\\bpoisson\\s+cluster\\s+sizes?\\b", low, perl = TRUE)) {
      s$hierarchy$cluster_size_dist <- "poisson"
      s$hierarchy$cluster_size_sd <- NULL
      mark("hierarchy.cluster_size_dist")
    } else {
      m_cssd <- reg_capture("\\bcluster\\s+size\\s+sd\\s*(?:to|=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
      if (!is.null(m_cssd)) {
        s$hierarchy$cluster_size_dist <- "normal"
        s$hierarchy$cluster_size_sd <- as.numeric(m_cssd[[1L]])
        mark("hierarchy.cluster_size_dist")
        mark("hierarchy.cluster_size_sd")
      } else if (grepl("\\b(unbalanced|unequal)\\s+cluster\\s+sizes?\\b", low, perl = TRUE)) {
        s$hierarchy$cluster_size_dist <- "normal"
        s$hierarchy$cluster_size_sd <- max(1, round(0.25 * as.numeric(s$hierarchy$cluster_size), 2))
        mark("hierarchy.cluster_size_dist")
      }
    }
  } else {
    m_n <- reg_capture("\\b(?:sample\\s+size|n_obs|n)\\s*(?:to|=|of)?\\s*(\\d+)\\b", low)
    if (!is.null(m_n)) {
      s$hierarchy$n_obs <- as.integer(m_n[[1L]])
      mark("hierarchy.n_obs")
    }
  }

  # 3. Outcome family / link / residual_sd
  if (grepl("\\b(binomial|binary\\s+outcome|logistic)\\b", low, perl = TRUE)) {
    s$outcome$family <- "binomial"
    s$outcome$link <- if (grepl("\\bprobit\\b", low)) "probit" else "logit"
    s$outcome$residual_sd <- NULL
    s$outcome$error_dist <- NULL
    s$outcome$error_df <- NULL
    s$outcome$error_skew <- NULL
    s$simulation$parameter_grid$residual_sd <- NULL
    q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
    s$estimator$engine <- infer_default_engine(as.integer(s$hierarchy$levels), "binomial", q_r)
    mark("outcome.family")
  } else if (grepl("\\b(poisson(?!\\s+cluster\\s+sizes?)|count\\s+outcome)\\b", low, perl = TRUE)) {
    s$outcome$family <- "poisson"
    s$outcome$link <- "log"
    s$outcome$residual_sd <- NULL
    s$outcome$error_dist <- NULL
    s$outcome$error_df <- NULL
    s$outcome$error_skew <- NULL
    s$simulation$parameter_grid$residual_sd <- NULL
    q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
    s$estimator$engine <- infer_default_engine(as.integer(s$hierarchy$levels), "poisson", q_r)
    mark("outcome.family")
  } else if (grepl("\\b(gaussian|continuous\\s+outcome|linear\\s+outcome)\\b", low, perl = TRUE)) {
    s$outcome$family <- "gaussian"
    s$outcome$link <- "identity"
    if (is.null(s$outcome$residual_sd)) s$outcome$residual_sd <- 1.0
    if (is.null(s$outcome$error_dist)) s$outcome$error_dist <- "normal"
    q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
    s$estimator$engine <- infer_default_engine(as.integer(s$hierarchy$levels), "gaussian", q_r)
    mark("outcome.family")
  }

  if (identical(s$outcome$family, "gaussian")) {
    if (grepl("\\bnormal\\s+(?:errors?|residuals?)\\b", low, perl = TRUE)) {
      s$outcome$error_dist <- "normal"
      s$outcome$error_df <- NULL
      s$outcome$error_skew <- NULL
      mark("outcome.error_dist")
    } else if (grepl("\\b(heavy[- ]?tailed|t[- ]?distributed|student[- ]?t)\\s+(?:errors?|residuals?)\\b", low, perl = TRUE)) {
      s$outcome$error_dist <- "t"
      m_df <- reg_capture("\\b(?:df|degrees\\s+of\\s+freedom)\\s*(?:to|=|of|is)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
      s$outcome$error_df <- if (!is.null(m_df)) as.numeric(m_df[[1L]]) else (s$outcome$error_df %||% 5)
      s$outcome$error_skew <- NULL
      mark("outcome.error_dist")
    } else if (grepl("\\b(skewed|asymmetric)\\s+(?:errors?|residuals?)\\b|\\berror\\s+skew(?:ness)?\\b", low, perl = TRUE)) {
      s$outcome$error_dist <- "skewed"
      m_sk <- reg_capture("\\b(?:error\\s+)?skew(?:ness)?\\s*(?:to|=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
      s$outcome$error_skew <- if (!is.null(m_sk)) as.numeric(m_sk[[1L]]) else (s$outcome$error_skew %||% 1.5)
      s$outcome$error_df <- NULL
      mark("outcome.error_dist")
    }
  }

  if (grepl("\\bprobit\\s+link\\b|\\buse\\s+probit\\b", low, perl = TRUE) && identical(s$outcome$family, "binomial")) {
    s$outcome$link <- "probit"
    mark("outcome.link")
  } else if (grepl("\\blogit\\s+link\\b|\\buse\\s+logit\\b", low, perl = TRUE) && identical(s$outcome$family, "binomial")) {
    s$outcome$link <- "logit"
    mark("outcome.link")
  }

  m_rsd <- reg_capture("\\b(?:residual\\s+sd|sigma)\\s*(?:to|=|of)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
  if (!is.null(m_rsd) && identical(s$outcome$family, "gaussian")) {
    s$outcome$residual_sd <- as.numeric(m_rsd[[1L]])
    mark("outcome.residual_sd")
  }

  # 4. Add / remove predictors (and keep predictor_corr synchronized)
  m_addp <- reg_capture("\\badd\\s+(?:a\\s+)?(?:(binary|uniform|normal)\\s+)?(?:(level[- ]?2|cluster[- ]?level)\\s+)?(?:predictor|covariate)\\s+([a-z][a-z0-9_]*)\\b", low)
  if (!is.null(m_addp)) {
    p_dist <- if (nzchar(m_addp[[1L]])) m_addp[[1L]] else "normal"
    p_lvl <- if (nzchar(m_addp[[2L]]) && identical(as.integer(s$hierarchy$levels), 2L)) 2L else 1L
    p_name <- m_addp[[3L]]
    existing <- vapply(s$predictors, `[[`, character(1), "name")
    if (is_valid_r_ident(p_name) && !(p_name %in% existing)) {
      s$predictors[[length(s$predictors) + 1L]] <- spec_predictor(p_name, level = p_lvl, distribution = p_dist)
      s$effects$fixed[[p_name]] <- 0.2
      if (is.matrix(s$predictor_corr)) {
        s$predictor_corr <- resize_corr_matrix(s$predictor_corr, length(s$predictors))
      }
      mark(paste0("predictors.", p_name))
    }
  }

  m_remp <- reg_capture("\\b(?:remove|drop)\\s+(?:(?:predictor|covariate)\\s+)?([a-z][a-z0-9_]*)\\b", low)
  if (!is.null(m_remp)) {
    rem_nm <- m_remp[[1L]]
    existing <- vapply(s$predictors, `[[`, character(1), "name")
    if (rem_nm %in% existing && length(s$predictors) > 1L) {
      keep_idx <- which(existing != rem_nm)
      s$predictors <- s$predictors[keep_idx]
      if (is.matrix(s$predictor_corr)) {
        s$predictor_corr <- if (length(keep_idx) >= 2L) s$predictor_corr[keep_idx, keep_idx, drop = FALSE] else NULL
      }
      s$effects$fixed[[rem_nm]] <- NULL
      if (length(s$effects$interactions %||% list()) > 0L) {
        s$effects$interactions <- Filter(function(it) !(rem_nm %in% it$terms), s$effects$interactions)
      }
      if (identical(s$effects$focal_predictor, rem_nm) || grepl(paste0("(^|:)", rem_nm, "(:|$)"), s$effects$focal_predictor %||% "")) {
        s$effects$focal_predictor <- s$predictors[[1L]]$name
      }
      if (identical(s$missingness$auxiliary, rem_nm)) {
        s$missingness$auxiliary <- s$predictors[[1L]]$name
      }
      s <- remove_random_slope_helper(s, rem_nm)
      mark(paste0("predictors.", rem_nm))
    }
  }

  # 4b. Predictor correlation updates
  if (grepl("\\b(?:remove|clear|drop)\\s+predictor\\s+cor(?:relation)?\\b", low, perl = TRUE)) {
    s$predictor_corr <- NULL
    mark("predictor_corr")
  } else {
    m_pc <- reg_capture("\\b(?:predictor|covariate)\\s+cor(?:relation)?\\s*(?:to|=|of)?\\s*(-?0?\\.\\d+)\\b", low)
    if (!is.null(m_pc) && length(s$predictors) >= 2L) {
      pcm <- build_same_level_corr_matrix(s$predictors, as.numeric(m_pc[[1L]]))
      if (!is.null(pcm)) {
        s$predictor_corr <- pcm
        mark("predictor_corr")
      }
    }
  }

  # 5. Centering updates
  m_cent <- reg_capture("\\b(group[- ]?mean|cluster[- ]?mean|grand[- ]?mean)\\s+center(?:ed|ing)?\\s+(?:for\\s+)?([a-z][a-z0-9_]*)\\b", low)
  if (!is.null(m_cent)) {
    cmode <- if (grepl("grand", m_cent[[1L]])) "grand_mean" else "group_mean"
    target_p <- m_cent[[2L]]
    for (idx in seq_along(s$predictors)) {
      if (identical(s$predictors[[idx]]$name, target_p)) {
        s$predictors[[idx]]$centering <- cmode
        mark(paste0("predictors.", target_p, ".centering"))
      }
    }
  }

  # 6. Fixed effect updates
  pred_names <- vapply(s$predictors, `[[`, character(1), "name")
  for (pn in pred_names) {
    m_fx <- reg_capture(sprintf("\\b%s\\s+(?:effect(?:\\s+size)?|beta|slope|coef(?:ficient)?)\\s*(?:to|=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", pn), low)
    if (is.null(m_fx)) {
      m_fx <- reg_capture(sprintf("\\b(?:effect(?:\\s+size)?|beta)\\s+(?:of|for)\\s+%s\\s*(?:to|=|of|is)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", pn), low)
    }
    if (!is.null(m_fx)) {
      s$effects$fixed[[pn]] <- as.numeric(m_fx[[1L]])
      mark(paste0("effects.fixed.", pn))
    }
  }
  # Generic focal effect update (when predictor name not repeated)
  m_gen_fx <- reg_capture("\\b(?:focal\\s+effect|effect\\s+size|beta)\\s*(?:to|=|of)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
  if (!is.null(m_gen_fx) && !any(grepl("^effects\\.fixed\\.", parsed))) {
    fp <- s$effects$focal_predictor %||% pred_names[[1L]]
    s$effects$fixed[[fp]] <- as.numeric(m_gen_fx[[1L]])
    mark(paste0("effects.fixed.", fp))
  }

  # 6b. Interactions add / remove in refine_spec
  if (grepl("\\b(remove|drop)\\s+interaction\\b", low, perl = TRUE)) {
    m_rint <- reg_capture("\\b(?:remove|drop)\\s+interaction\\s+(?:between\\s+)?([a-z][a-z0-9_]*)\\s*(?:and|:|\\*)\\s*([a-z][a-z0-9_]*)\\b", low)
    if (!is.null(m_rint)) {
      pair_rm <- sort(c(m_rint[[1L]], m_rint[[2L]]))
      s$effects$interactions <- Filter(function(it) !identical(sort(it$terms), pair_rm), s$effects$interactions %||% list())
    } else {
      s$effects$interactions <- list()
    }
    if (grepl(":", s$effects$focal_predictor %||% "", fixed = TRUE)) {
      s$effects$focal_predictor <- s$predictors[[1L]]$name
    }
    mark("effects.interactions")
  } else if (grepl("\\b(add|include|set)\\s+(?:a\\s+)?(?:cross[- ]?level\\s+)?interaction\\b", low, perl = TRUE)) {
    m_aint <- reg_capture("\\binteraction\\s+(?:between\\s+)?([a-z][a-z0-9_]*)\\s*(?:and|:|\\*)\\s*([a-z][a-z0-9_]*)(?:.*?(?:beta|effect|of|=|with)\\s*(-?\\d+(?:\\.\\d+)?))?", low)
    if (!is.null(m_aint)) {
      t1 <- m_aint[[1L]]; t2 <- m_aint[[2L]]
      existing <- vapply(s$predictors, `[[`, character(1), "name")
      if (is_valid_r_ident(t1) && is_valid_r_ident(t2) && !identical(t1, t2)) {
        if (!(t1 %in% existing)) {
          s$predictors[[length(s$predictors) + 1L]] <- spec_predictor(t1, level = 1L, distribution = "normal")
          s$effects$fixed[[t1]] <- 0.2
          existing <- c(existing, t1)
        }
        if (!(t2 %in% existing)) {
          t2_lvl <- if (identical(as.integer(s$hierarchy$levels), 2L) && (grepl("cross[- ]?level", low) || grepl("^z[1-9]$", t2))) 2L else 1L
          s$predictors[[length(s$predictors) + 1L]] <- spec_predictor(t2, level = t2_lvl, distribution = "normal")
          s$effects$fixed[[t2]] <- 0.2
          existing <- c(existing, t2)
        }
        ib <- if (length(m_aint) >= 3L && nzchar(m_aint[[3L]])) as.numeric(m_aint[[3L]]) else 0.25
        ct <- canonical_interaction_terms(c(t1, t2), existing)
        inters <- s$effects$interactions %||% list()
        inters <- Filter(function(it) !identical(sort(it$terms), sort(ct)), inters)
        inters[[length(inters) + 1L]] <- spec_interaction(ct[[1L]], ct[[2L]], beta = ib)
        s$effects$interactions <- inters
        mark(paste0("effects.interactions.", paste(ct, collapse = ":"), ".beta"))
      }
    }
  }

  # 7. Random slopes add / remove / correlation
  if (identical(as.integer(s$hierarchy$levels), 2L)) {
    if (grepl("\\b(remove|drop|no)\\s+random\\s+slopes?\\b", low, perl = TRUE)) {
      m_rsl <- reg_capture("\\b(?:remove|drop)\\s+random\\s+slope\\s+(?:for|on)\\s+([a-z][a-z0-9_]*)\\b", low)
      if (!is.null(m_rsl)) {
        s <- remove_random_slope_helper(s, m_rsl[[1L]])
      } else {
        s$effects$random$slopes <- list()
        s$effects$random$corr_matrix <- NULL
      }
      mark("effects.random.slopes")
    } else if (grepl("\\b(add|include|set)\\s+(?:a\\s+)?random\\s+slope\\b", low, perl = TRUE)) {
      m_asl <- reg_capture("\\brandom\\s+slope\\s+(?:for|on)\\s+([a-z][a-z0-9_]*)\\b", low)
      target_sl <- if (!is.null(m_asl)) m_asl[[1L]] else {
        l1 <- Filter(function(p) identical(as.integer(p$level), 1L), s$predictors)
        if (length(l1) > 0L) l1[[1L]]$name else NULL
      }
      m_ssd <- reg_capture("\\b(?:sd|standard\\s+deviation)\\s*(?:to|=|of)?\\s*(\\d+(?:\\.\\d+)?)\\b", low)
      sd_val <- if (!is.null(m_ssd)) as.numeric(m_ssd[[1L]]) else 0.20

      if (!is.null(target_sl) && is_valid_r_ident(target_sl)) {
        existing <- vapply(s$predictors, `[[`, character(1), "name")
        if (!(target_sl %in% existing)) {
          s$predictors[[length(s$predictors) + 1L]] <- spec_predictor(target_sl, level = 1L, distribution = "normal")
          s$effects$fixed[[target_sl]] <- 0.2
        }
        s <- upsert_random_slope_helper(s, target_sl, sd_val)
        mark(paste0("effects.random.slopes.", target_sl, ".sd"))
      }
    }

    m_cor <- reg_capture("\\b(?:random[- ]?effects?\\s+)?cor(?:relation)?\\s*(?:to|=|of)?\\s*(-?0?\\.\\d+)\\b", low)
    if (!is.null(m_cor)) {
      q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
      rho <- as.numeric(m_cor[[1L]])
      if (q_r >= 2L && abs(rho) < 0.99) {
        cm <- matrix(rho, nrow = q_r, ncol = q_r)
        diag(cm) <- 1
        s$effects$random$corr_matrix <- cm
        mark("effects.random.corr_matrix")
      }
    }
  }

  # 8. Missingness updates
  if (grepl("\\b(remove|no|drop|disable)\\s+missing(?:ness)?\\b|\\bcomplete\\s+data\\b", low, perl = TRUE)) {
    s$missingness$mechanism <- "none"
    s$missingness$rate <- 0
    s$missingness$auxiliary <- NULL
    s$missingness$strength <- NULL
    s$simulation$parameter_grid$missing_rate <- NULL
    mark("missingness.mechanism")
  } else if (grepl("\\b(mar|mcar|missingness|missing\\s+data|missing\\s+rate|attrition)\\b", low, perl = TRUE)) {
    if (grepl("\\bmar\\b", low)) {
      s$missingness$mechanism <- "MAR"
    } else if (grepl("\\bmcar\\b", low)) {
      s$missingness$mechanism <- "MCAR"
    } else if (identical(s$missingness$mechanism, "none")) {
      s$missingness$mechanism <- "MAR"
    }

    m_pct <- reg_capture("\\b(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\s*(?:mar|mcar)?\\s*(?:missingness|missing|attrition)\\b", low)
    if (is.null(m_pct)) {
      m_pct <- reg_capture("\\b(?:missingness|missing\\s+rate|attrition)\\s*(?:to|=|of)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
    }
    if (!is.null(m_pct)) {
      r <- as.numeric(m_pct[[1L]])
      has_pct <- length(m_pct) >= 2L && nzchar(m_pct[[2L]])
      s$missingness$rate <- if (has_pct || r > 1) r / 100 else r
    } else if (s$missingness$rate <= 0) {
      s$missingness$rate <- 0.15
    }

    if (identical(s$missingness$mechanism, "MAR")) {
      m_aux <- reg_capture("\\b(?:mar\\s+)?auxiliary\\s*(?:to|=|of)?\\s*([a-z][a-z0-9_]*)\\b", low)
      if (!is.null(m_aux) && m_aux[[1L]] %in% vapply(s$predictors, `[[`, character(1), "name")) {
        s$missingness$auxiliary <- m_aux[[1L]]
        mark("missingness.auxiliary")
      } else if (is.null(s$missingness$auxiliary)) {
        s$missingness$auxiliary <- s$predictors[[1L]]$name
      }
      m_str <- reg_capture("\\b(?:mar\\s+)?strength\\s*(?:to|=|of)?\\s*(-?\\d+(?:\\.\\d+)?)\\b", low)
      if (!is.null(m_str)) {
        s$missingness$strength <- as.numeric(m_str[[1L]])
        mark("missingness.strength")
      } else if (is.null(s$missingness$strength)) {
        s$missingness$strength <- 1.0
      }
    } else {
      s$missingness$auxiliary <- NULL
      s$missingness$strength <- NULL
    }
    mark("missingness.mechanism")
    mark("missingness.rate")
  }

  # 9. Simulation parameters & parameter grid
  m_reps <- reg_capture("\\b(\\d+)\\s*(?:replications?|reps?|simulations?|sims?|iterations?)\\b", low)
  if (is.null(m_reps)) {
    m_reps <- reg_capture("\\b(?:n_reps|replications?|reps?)\\s*(?:to|=|of)?\\s*(\\d+)\\b", low)
  }
  if (!is.null(m_reps)) {
    s$simulation$n_reps <- as.integer(m_reps[[1L]])
    mark("simulation.n_reps")
  }

  m_alpha <- reg_capture("\\balpha\\s*(?:to|=|of)?\\s*(\\d+(?:\\.\\d+)?)\\s*(%|percent)?\\b", low)
  if (!is.null(m_alpha)) {
    ra <- as.numeric(m_alpha[[1L]])
    s$simulation$alpha <- if ((length(m_alpha) >= 2L && nzchar(m_alpha[[2L]])) || ra >= 1) ra / 100 else ra
    s$estimator$conf_level <- 1 - s$simulation$alpha
    mark("simulation.alpha")
  }

  m_seed <- reg_capture("\\bseed\\s*(?:to|=|of)?\\s*(\\d+)\\b", low)
  if (!is.null(m_seed)) {
    s$simulation$seed <- as.integer(m_seed[[1L]])
    mark("simulation.seed")
  }

  if (grepl("\\b(use\\s+ml|maximum\\s+likelihood)\\b", low, perl = TRUE)) {
    s$estimator$reml <- FALSE
    mark("estimator.reml")
  } else if (grepl("\\b(use\\s+reml|restricted\\s+maximum\\s+likelihood)\\b", low, perl = TRUE)) {
    s$estimator$reml <- TRUE
    mark("estimator.reml")
  }

  # Explicit estimator switch when statistically compatible (e.g., lm <-> glm for 1-level Gaussian)
  explicit_engine <- NULL
  if (grepl("\\b(?:switch\\s+to|use)\\s+glm\\b", low, perl = TRUE)) {
    explicit_engine <- "glm"
  } else if (grepl("\\b(?:switch\\s+to|use)\\s+lm\\b", low, perl = TRUE)) {
    explicit_engine <- "lm"
  }

  if (grepl("\\b(clear|remove|drop)\\s+(?:parameter\\s+)?grid\\b", low, perl = TRUE)) {
    s$simulation$parameter_grid <- list()
    mark("simulation.parameter_grid")
  } else {
    new_grid <- extract_grid_from_text(low, as.integer(s$hierarchy$levels), s$missingness$mechanism, s$outcome$family)
    if (length(new_grid) > 0L) {
      for (gn in names(new_grid)) {
        s$simulation$parameter_grid[[gn]] <- new_grid[[gn]]
        mark(paste0("simulation.parameter_grid.", gn))
      }
    }
  }

  # Refresh estimator consistency if needed
  q_r <- as.integer(isTRUE(s$effects$random$intercept)) + length(s$effects$random$slopes)
  if (!is.null(explicit_engine) && identical(as.integer(s$hierarchy$levels), 1L) && identical(s$outcome$family, "gaussian")) {
    s$estimator$engine <- explicit_engine
    mark("estimator.engine")
  } else {
    s$estimator$engine <- infer_default_engine(as.integer(s$hierarchy$levels), s$outcome$family, q_r)
  }

  list(
    spec = as_spec_ir(s, validate = FALSE),
    clarifications = clarifs,
    parsed_fields = parsed
  )
}

# ==============================================================================
# Internal Parsing & Path Mutation Helpers
# ==============================================================================

reg_capture <- function(pattern, text) {
  m <- regexec(pattern, text, perl = TRUE)
  res <- regmatches(text, m)[[1L]]
  if (length(res) <= 1L) return(NULL)
  res[-1L]
}

singularize_noun <- function(w) {
  w <- tolower(trimws(w))
  if (w == "children") return("child")
  if (w == "classes") return("class")
  if (w == "centres") return("centre")
  sub("s$", "", w)
}

parse_num_list <- function(s) {
  tokens <- regmatches(s, gregexpr("-?\\d+(?:\\.\\d+)?", s, perl = TRUE))[[1L]]
  if (length(tokens) < 2L) return(NULL)
  as.numeric(tokens)
}

extract_grid_from_text <- function(low, levels_val, miss_mech, out_family) {
  grid <- list()
  num_seq_re <- "(-?\\d+(?:\\.\\d+)?(?:\\s*,\\s*(?:and\\s+)?-?\\d+(?:\\.\\d+)?)+|-?\\d+(?:\\.\\d+)?\\s+and\\s+-?\\d+(?:\\.\\d+)?)"

  if (levels_val == 2L) {
    m_gcl <- reg_capture(sprintf("\\b(?:sweep|vary|varying|across)\\s+(?:n_clusters|clusters|schools|hospitals|groups)?\\s*(?:across|from|of|=)?\\s*%s\\s*(?:clusters|schools|hospitals|groups)?\\b", num_seq_re), low)
    if (!is.null(m_gcl) && grepl("\\b(clusters?|schools?|hospitals?|groups?)\\b", low)) {
      # Ensure it is specifically referring to clusters, not icc/effect
      if (grepl(sprintf("(?:clusters?|schools?|hospitals?|groups?)\\s*(?:across|from|of|=)?\\s*%s|%s\\s+(?:clusters?|schools?|hospitals?|groups?)", num_seq_re, num_seq_re), low, perl = TRUE)) {
        m_spec <- reg_capture(sprintf("(?:(?:n_clusters|clusters?|schools?|hospitals?|groups?)\\s*(?:across|from|of|=)?\\s*%s|%s\\s+(?:clusters?|schools?|hospitals?|groups?))", num_seq_re, num_seq_re), low)
        raw_s <- if (!is.null(m_spec)) paste(Filter(nzchar, m_spec), collapse = " ") else m_gcl[[1L]]
        vals <- parse_num_list(raw_s)
        if (!is.null(vals) && all(vals >= 3 & vals == floor(vals))) grid$n_clusters <- as.integer(vals)
      }
    }

    m_gcs <- reg_capture(sprintf("\\b(?:cluster\\s+sizes?|cluster_size)\\s*(?:across|from|of|=)?\\s*%s", num_seq_re), low)
    if (!is.null(m_gcs)) {
      vals <- parse_num_list(m_gcs[[1L]])
      if (!is.null(vals) && all(vals >= 2 & vals == floor(vals))) grid$cluster_size <- as.integer(vals)
    }

    m_gicc <- reg_capture(sprintf("\\b(?:icc|intraclass\\s+correlations?)\\s*(?:across|from|of|=)?\\s*%s", num_seq_re), low)
    if (!is.null(m_gicc)) {
      vals <- parse_num_list(m_gicc[[1L]])
      if (!is.null(vals) && all(vals >= 0 & vals < 1)) grid$icc <- vals
    }
  } else {
    m_gn <- reg_capture(sprintf("\\b(?:sample\\s+sizes?|n_obs)\\s*(?:across|from|of|=)?\\s*%s", num_seq_re), low)
    if (!is.null(m_gn)) {
      vals <- parse_num_list(m_gn[[1L]])
      if (!is.null(vals) && all(vals >= 5 & vals == floor(vals))) grid$n_obs <- as.integer(vals)
    }
  }

  m_geff <- reg_capture(sprintf("\\b(?:effect\\s+sizes?|focal\\s+effects?|betas?)\\s*(?:across|from|of|=)?\\s*%s", num_seq_re), low)
  if (!is.null(m_geff)) {
    vals <- parse_num_list(m_geff[[1L]])
    if (!is.null(vals)) grid$focal_effect <- vals
  }

  if (miss_mech != "none") {
    m_gmiss <- reg_capture(sprintf("\\b(?:missing\\s+rates?|missingness\\s+rates?)\\s*(?:across|from|of|=)?\\s*%s", num_seq_re), low)
    if (!is.null(m_gmiss)) {
      vals <- parse_num_list(m_gmiss[[1L]])
      if (!is.null(vals) && all(vals >= 0 & vals < 0.95)) grid$missing_rate <- vals
    }
  }

  grid
}

synthesize_title <- function(design_type, levels_val, family, cluster_name, has_slopes, miss_mech) {
  lvl_part <- if (levels_val == 2L) sprintf("2-Level %s", tools::toTitleCase(cluster_name)) else "Single-Level"
  fam_part <- switch(family, gaussian = "Linear", binomial = "Logistic", poisson = "Poisson")
  des_part <- switch(
    design_type,
    power_analysis = "Power Simulation",
    multilevel_sim = "Multilevel Simulation",
    two_level_mlm = "Multilevel Model Study",
    monte_carlo_lm = "Monte Carlo Simulation"
  )
  extras <- c(
    if (has_slopes) "Random Slopes" else NULL,
    if (miss_mech != "none") paste(miss_mech, "Missingness") else NULL
  )
  if (length(extras) > 0L) {
    sprintf("%s %s %s (%s)", lvl_part, fam_part, des_part, paste(extras, collapse = ", "))
  } else {
    sprintf("%s %s %s", lvl_part, fam_part, des_part)
  }
}

upsert_random_slope_helper <- function(spec, pred_name, sd_val) {
  slopes <- spec$effects$random$slopes %||% list()
  found <- FALSE
  for (i in seq_along(slopes)) {
    if (identical(slopes[[i]]$predictor, pred_name)) {
      slopes[[i]]$sd <- as.numeric(sd_val)
      found <- TRUE
    }
  }
  if (!found) {
    slopes[[length(slopes) + 1L]] <- list(predictor = pred_name, sd = as.numeric(sd_val))
  }
  spec$effects$random$slopes <- slopes
  spec$effects$random$corr_matrix <- resize_corr_matrix(
    spec$effects$random$corr_matrix,
    as.integer(isTRUE(spec$effects$random$intercept)) + length(slopes)
  )
  spec
}

remove_random_slope_helper <- function(spec, pred_name) {
  slopes <- spec$effects$random$slopes %||% list()
  keep_idx <- which(!vapply(slopes, function(sl) identical(sl$predictor, pred_name), logical(1)))
  spec$effects$random$slopes <- slopes[keep_idx]
  q_new <- as.integer(isTRUE(spec$effects$random$intercept)) + length(spec$effects$random$slopes)
  spec$effects$random$corr_matrix <- if (q_new <= 1L) NULL else resize_corr_matrix(spec$effects$random$corr_matrix, q_new)
  spec
}

resize_corr_matrix <- function(old_cm, q_new) {
  if (q_new <= 1L) return(NULL)
  new_cm <- diag(q_new)
  if (is.matrix(old_cm) && nrow(old_cm) == ncol(old_cm)) {
    k <- min(nrow(old_cm), q_new)
    new_cm[seq_len(k), seq_len(k)] <- old_cm[seq_len(k), seq_len(k)]
    diag(new_cm) <- 1
  }
  new_cm
}

build_same_level_corr_matrix <- function(predictors, rho) {
  p <- length(predictors)
  if (p < 2L || !is_num_scalar(rho) || abs(rho) >= 0.99) return(NULL)
  lvls <- vapply(predictors, function(pr) as.integer(pr$level), integer(1))
  cm <- diag(p)
  for (i in seq_len(p - 1L)) {
    for (j in seq.int(i + 1L, p)) {
      if (lvls[[i]] == lvls[[j]]) {
        cm[i, j] <- rho
        cm[j, i] <- rho
      }
    }
  }
  eigs <- eigen(cm, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigs) <= 1e-8) return(NULL)
  cm
}

apply_ir_path_value <- function(spec, path, value) {
  if (path == "hierarchy.n_obs") {
    spec$hierarchy$n_obs <- maybe_int(as.numeric(value))
  } else if (path == "hierarchy.n_clusters") {
    spec$hierarchy$n_clusters <- maybe_int(as.numeric(value))
  } else if (path == "hierarchy.cluster_size") {
    spec$hierarchy$cluster_size <- maybe_int(as.numeric(value))
  } else if (path == "hierarchy.icc") {
    spec$hierarchy$icc <- as.numeric(value)
    spec$effects$random$intercept_sd <- NULL
  } else if (grepl("^effects\\.fixed\\.", path)) {
    pname <- sub("^effects\\.fixed\\.", "", path)
    spec$effects$fixed[[pname]] <- as.numeric(value)
  } else if (grepl("^effects\\.random\\.slopes\\..+\\.sd$", path)) {
    pname <- sub("^effects\\.random\\.slopes\\.([^.]+)\\.sd$", "\\1", path)
    spec <- upsert_random_slope_helper(spec, pname, as.numeric(value))
  } else if (grepl("^predictors\\..+\\.level$", path)) {
    pname <- sub("^predictors\\.([^.]+)\\.level$", "\\1", path)
    lvl_v <- maybe_int(as.numeric(value))
    for (i in seq_along(spec$predictors)) {
      if (identical(spec$predictors[[i]]$name, pname)) {
        spec$predictors[[i]]$level <- lvl_v
        if (identical(lvl_v, 2L) && identical(spec$predictors[[i]]$centering, "group_mean")) {
          spec$predictors[[i]]$centering <- "none"
        }
      }
    }
  } else if (path == "missingness.mechanism") {
    mech <- as.character(value)
    spec$missingness$mechanism <- mech
    if (mech == "none") {
      spec$missingness$rate <- 0
      spec$missingness$auxiliary <- NULL
      spec$missingness$strength <- NULL
      spec$simulation$parameter_grid$missing_rate <- NULL
    } else {
      if (spec$missingness$rate <= 0) spec$missingness$rate <- 0.15
      if (mech == "MAR") {
        if (is.null(spec$missingness$auxiliary)) spec$missingness$auxiliary <- spec$predictors[[1L]]$name
        if (is.null(spec$missingness$strength)) spec$missingness$strength <- 1.0
      } else {
        spec$missingness$auxiliary <- NULL
        spec$missingness$strength <- NULL
      }
    }
  } else if (path == "missingness.rate") {
    spec$missingness$rate <- as.numeric(value)
  } else if (path == "simulation.n_reps") {
    spec$simulation$n_reps <- maybe_int(as.numeric(value))
  } else if (path == "simulation.alpha") {
    spec$simulation$alpha <- as.numeric(value)
    spec$estimator$conf_level <- 1 - spec$simulation$alpha
  } else {
    cli::cli_abort("Unsupported clarification path: {.val {path}}.")
  }
  spec
}
