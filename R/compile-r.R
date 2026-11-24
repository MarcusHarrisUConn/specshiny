#' Deterministically compile a validated `spec_ir` into a standalone R script
#'
#' Compiles any schema- and statistically-validated `spec_ir` into a clean,
#' readable, self-contained, and executable R simulation script using audited
#' code templates. Never evaluates untrusted text or language-model strings;
#' all identifiers and parameters pass [assert_valid_spec_ir()] before emission,
#' and every compiled script passes [verify_compiled_r_ast()] static AST
#' verification before returning.
#'
#' Generated scripts use reproducible `L'Ecuyer-CMRG` random number generation,
#' exact Cholesky draws for correlated predictors and correlated random effects,
#' calibrated logistic MAR missingness, and zero `do.call()` or `eval()` calls.
#'
#' @param spec A validated `spec_ir` object.
#' @param file Optional file path; when provided, writes the compiled R script
#'   to `file` and returns the script text invisibly.
#' @param include_header Logical; whether to include a descriptive header block.
#' @param run_on_source Logical; if `TRUE` (default), appends a top-level
#'   execution block (`results <- run_simulation(cfg)`) so the script runs
#'   standalone when sourced or executed via `Rscript`.
#'
#' @return A character scalar containing the deterministic R script (invisibly
#'   when `file` is provided).
#' @export
#' @examples
#' spec <- new_spec_ir(
#'   design_type = "monte_carlo_lm",
#'   hierarchy = list(levels = 1L, n_obs = 120L),
#'   simulation = list(n_reps = 20L, seed = 42L)
#' )
#' code <- compile_r_script(spec)
#' verify_compiled_r_ast(code)
compile_r_script <- function(spec,
                             file = NULL,
                             include_header = TRUE,
                             run_on_source = TRUE) {
  assert_valid_spec_ir(spec)

  math_info <- spec_to_math(spec)
  levels_val <- as.integer(spec$hierarchy$levels)
  engine <- spec$estimator$engine
  family <- spec$outcome$family
  link <- spec$outcome$link
  focal <- spec$effects$focal_predictor %||% spec$predictors[[1L]]$name

  sections <- character()

  # 1. Header comment (strictly sanitized single-line comments only)
  if (isTRUE(include_header)) {
    header_lines <- c(
      "# =============================================================================",
      sprintf("# %s", sanitize_r_comment(spec$metadata$title)),
      "# Generated deterministically by specshiny (AST-verified; zero eval/parse)",
      sprintf("# IR Schema Version : %s", sanitize_r_comment(spec$schema_version)),
      sprintf("# Design Type       : %s (%d-level, %s / %s link)",
              sanitize_r_comment(spec$design_type), levels_val, family, link),
      sprintf("# Model Formula     : %s", sanitize_r_comment(math_info$formula)),
      sprintf("# Estimator Engine  : %s", sanitize_r_comment(engine)),
      "# ============================================================================="
    )
    sections <- c(sections, paste(header_lines, collapse = "\n"))
  }

  # 2. Packages (minimal: only load lme4 when needed)
  if (engine %in% c("lmer", "glmer")) {
    sections <- c(sections, paste(
      "if (!requireNamespace(\"lme4\", quietly = TRUE)) {",
      "  stop(\"Package 'lme4' is required to run this multilevel simulation.\", call. = FALSE)",
      "}",
      "library(lme4)",
      sep = "\n"
    ))
  }

  # 3. Configuration block
  cfg_lines <- build_cfg_block(spec, focal)
  sections <- c(sections, cfg_lines)

  # 4. Data-generating process function
  dgp_lines <- build_dgp_block(spec)
  sections <- c(sections, dgp_lines)

  # 5. Model fitting & metric extraction function
  fit_lines <- build_fit_block(spec, math_info$formula, focal)
  sections <- c(sections, fit_lines)

  # 6. Simulation runner (handles both single-cell and parameter_grid sweeps)
  runner_lines <- build_runner_block(spec)
  sections <- c(sections, runner_lines)

  # 7. Optional top-level execution block
  if (isTRUE(run_on_source)) {
    exec_lines <- paste(
      "# Execute simulation study",
      "results <- run_simulation(cfg)",
      "print(results$summary, row.names = FALSE)",
      sep = "\n"
    )
    sections <- c(sections, exec_lines)
  }

  script <- paste(sections, collapse = "\n\n")
  script <- paste0(script, "\n")

  # Defense-in-depth post-compilation static AST safety gate
  verify_compiled_r_ast(script, strict = TRUE)

  if (!is.null(file)) {
    writeLines(script, con = file, useBytes = TRUE)
    return(invisible(script))
  }
  script
}

# ==============================================================================
# Internal Deterministic Template Builders
# ==============================================================================

fmt_num <- function(x) {
  if (!is_num_scalar(x)) {
    cli::cli_abort("Expected finite numeric scalar during code compilation.")
  }
  formatC(as.numeric(x), digits = 12, format = "fg", flag = "")
}

fmt_int <- function(x) {
  if (!is_whole_scalar(x)) {
    cli::cli_abort("Expected finite integer scalar during code compilation.")
  }
  sprintf("%dL", as.integer(x))
}

fmt_lgl <- function(x) {
  if (isTRUE(x)) "TRUE" else "FALSE"
}

fmt_num_vec <- function(v) {
  paste0("c(", paste(vapply(v, fmt_num, character(1)), collapse = ", "), ")")
}

fmt_matrix <- function(m) {
  vals <- fmt_num_vec(as.numeric(m))
  sprintf("matrix(%s, nrow = %dL, ncol = %dL)", vals, nrow(m), ncol(m))
}

build_cfg_block <- function(spec, focal) {
  levels_val <- as.integer(spec$hierarchy$levels)
  lines <- c(
    "# --- 1. Simulation Configuration ---",
    "cfg <- list(",
    sprintf("  title = %s,", deparse1(sanitize_r_comment(spec$metadata$title))),
    sprintf("  design_type = %s,", deparse1(spec$design_type)),
    sprintf("  levels = %s,", fmt_int(levels_val))
  )

  if (levels_val == 1L) {
    lines <- c(lines, sprintf("  n_obs = %s,", fmt_int(spec$hierarchy$n_obs)))
  } else {
    cs_dist <- spec$hierarchy$cluster_size_dist %||% "fixed"
    lines <- c(
      lines,
      sprintf("  n_clusters = %s,", fmt_int(spec$hierarchy$n_clusters)),
      sprintf("  cluster_size = %s,", fmt_int(spec$hierarchy$cluster_size)),
      sprintf("  cluster_size_dist = %s,", deparse1(cs_dist)),
      if (identical(cs_dist, "normal")) sprintf("  cluster_size_sd = %s,", fmt_num(spec$hierarchy$cluster_size_sd)) else NULL,
      if (!is.null(spec$hierarchy$time_points)) sprintf("  time_points = %s,", fmt_int(spec$hierarchy$time_points)) else NULL,
      if (!is.null(spec$hierarchy$time_spacing)) sprintf("  time_spacing = %s,", fmt_num_vec(spec$hierarchy$time_spacing)) else NULL,
      sprintf("  icc = %s,", fmt_num(spec$hierarchy$icc))
    )
  }

  if (identical(spec$outcome$family, "gaussian")) {
    edist <- spec$outcome$error_dist %||% "normal"
    lines <- c(
      lines,
      sprintf("  residual_sd = %s,", fmt_num(spec$outcome$residual_sd)),
      sprintf("  error_dist = %s,", deparse1(edist)),
      if (identical(edist, "t")) sprintf("  error_df = %s,", fmt_num(spec$outcome$error_df)) else NULL,
      if (identical(edist, "skewed")) sprintf("  error_skew = %s,", fmt_num(spec$outcome$error_skew)) else NULL,
      if (identical(spec$design_type, "longitudinal_growth")) sprintf("  ar1_rho = %s,", fmt_num(spec$outcome$ar1_rho %||% 0)) else NULL
    )
  }

  pred_names <- vapply(spec$predictors, `[[`, character(1), "name")
  canon_focal <- if (grepl(":", focal, fixed = TRUE)) {
    paste(canonical_interaction_terms(strsplit(focal, ":", fixed = TRUE)[[1L]], pred_names), collapse = ":")
  } else {
    focal
  }

  lines <- c(
    lines,
    sprintf("  intercept = %s,", fmt_num(spec$effects$intercept)),
    sprintf("  focal_predictor = %s,", deparse1(canon_focal))
  )

  fixed_entries <- vapply(spec$predictors, function(p) {
    b <- spec$effects$fixed[[p$name]] %||% 0
    sprintf("%s = %s", p$name, fmt_num(b))
  }, character(1))
  lines <- c(lines, sprintf("  fixed = list(%s),", paste(fixed_entries, collapse = ", ")))

  inters <- spec$effects$interactions %||% list()
  if (length(inters) > 0L) {
    int_entries <- vapply(inters, function(it) {
      ct <- canonical_interaction_terms(it$terms, pred_names)
      sprintf("%s = %s", deparse1(paste(ct, collapse = ":")), fmt_num(it$beta))
    }, character(1))
    lines <- c(lines, sprintf("  interactions = list(%s),", paste(int_entries, collapse = ", ")))
  } else {
    lines <- c(lines, "  interactions = list(),")
  }

  if (!is.null(spec$predictor_corr) && is.matrix(spec$predictor_corr)) {
    lines <- c(lines, sprintf("  predictor_corr = %s,", fmt_matrix(spec$predictor_corr)))
  }

  if (levels_val == 2L) {
    if (!is.null(spec$effects$random$intercept_sd)) {
      lines <- c(lines, sprintf("  intercept_sd = %s,", fmt_num(spec$effects$random$intercept_sd)))
    }
    slopes <- spec$effects$random$slopes %||% list()
    if (length(slopes) > 0L) {
      sl_entries <- vapply(slopes, function(sl) {
        sprintf("%s = %s", sl$predictor, fmt_num(sl$sd))
      }, character(1))
      lines <- c(lines, sprintf("  slope_sds = c(%s),", paste(sl_entries, collapse = ", ")))
    }
    q_rand <- as.integer(isTRUE(spec$effects$random$intercept)) + length(slopes)
    if (q_rand > 1L) {
      cm <- spec$effects$random$corr_matrix %||% diag(q_rand)
      lines <- c(lines, sprintf("  corr_matrix = %s,", fmt_matrix(cm)))
    }
  }

  lines <- c(
    lines,
    sprintf("  missing_mechanism = %s,", deparse1(spec$missingness$mechanism)),
    sprintf("  missing_rate = %s,", fmt_num(spec$missingness$rate))
  )
  if (identical(spec$missingness$mechanism, "MAR") ||
      (identical(spec$missingness$mechanism, "monotone_dropout") && !is.null(spec$missingness$auxiliary))) {
    lines <- c(
      lines,
      sprintf("  missing_auxiliary = %s,", deparse1(spec$missingness$auxiliary)),
      sprintf("  missing_strength = %s,", fmt_num(spec$missingness$strength %||% 1.0))
    )
  }

  pg <- spec$simulation$parameter_grid %||% list()
  if (length(pg) > 0L) {
    pg_entries <- vapply(names(pg), function(gn) {
      sprintf("%s = %s", gn, fmt_num_vec(pg[[gn]]))
    }, character(1))
    pg_str <- sprintf("list(%s)", paste(pg_entries, collapse = ", "))
  } else {
    pg_str <- "list()"
  }

  lines <- c(
    lines,
    sprintf("  n_reps = %s,", fmt_int(spec$simulation$n_reps)),
    sprintf("  seed = %s,", fmt_int(spec$simulation$seed)),
    sprintf("  alpha = %s,", fmt_num(spec$simulation$alpha)),
    sprintf("  conf_level = %s,", fmt_num(spec$estimator$conf_level)),
    sprintf("  reml = %s,", fmt_lgl(spec$estimator$reml)),
    sprintf("  parameter_grid = %s", pg_str),
    ")"
  )

  paste(lines, collapse = "\n")
}

build_dgp_block <- function(spec) {
  levels_val <- as.integer(spec$hierarchy$levels)
  y_name <- spec$outcome$name
  fam <- spec$outcome$family
  lnk <- spec$outcome$link
  cl_name <- spec$hierarchy$cluster_name

  lines <- c(
    "# --- 2. Data-Generating Process ---",
    "simulate_one_dataset <- function(params) {"
  )

  if (levels_val == 1L) {
    lines <- c(
      lines,
      "  n <- as.integer(params$n_obs)",
      "  dat <- data.frame(row_id = seq_len(n))"
    )
  } else {
    cs_dist <- spec$hierarchy$cluster_size_dist %||% "fixed"
    q_rand_min <- max(2L, as.integer(isTRUE(spec$effects$random$intercept)) + length(spec$effects$random$slopes %||% list()) + 1L)
    if (identical(cs_dist, "normal")) {
      lines <- c(
        lines,
        "  n_clusters <- as.integer(params$n_clusters)",
        "  cluster_size <- as.integer(params$cluster_size)",
        sprintf("  min_nj <- %dL", q_rand_min),
        "  cluster_sizes <- pmax(min_nj, as.integer(round(stats::rnorm(n_clusters, mean = cluster_size, sd = params$cluster_size_sd))))",
        "  n <- sum(cluster_sizes)",
        "  cluster_idx <- rep(seq_len(n_clusters), times = cluster_sizes)",
        sprintf("  dat <- data.frame(%s = factor(cluster_idx))", cl_name)
      )
    } else if (identical(cs_dist, "poisson")) {
      lines <- c(
        lines,
        "  n_clusters <- as.integer(params$n_clusters)",
        "  cluster_size <- as.integer(params$cluster_size)",
        sprintf("  min_nj <- %dL", q_rand_min),
        "  cluster_sizes <- pmax(min_nj, stats::rpois(n_clusters, lambda = cluster_size))",
        "  n <- sum(cluster_sizes)",
        "  cluster_idx <- rep(seq_len(n_clusters), times = cluster_sizes)",
        sprintf("  dat <- data.frame(%s = factor(cluster_idx))", cl_name)
      )
    } else {
      lines <- c(
        lines,
        "  n_clusters <- as.integer(params$n_clusters)",
        "  cluster_size <- as.integer(params$cluster_size)",
        "  n <- n_clusters * cluster_size",
        "  cluster_idx <- rep(seq_len(n_clusters), each = cluster_size)",
        if (identical(spec$design_type, "longitudinal_growth")) "  wave_idx <- rep(seq_len(cluster_size), times = n_clusters)" else NULL,
        sprintf("  dat <- data.frame(%s = factor(cluster_idx))", cl_name)
      )
    }
  }

  # Predictor generation: exact Cholesky draw when predictor_corr is specified
  use_pcorr <- !is.null(spec$predictor_corr) && is.matrix(spec$predictor_corr)
  if (use_pcorr) {
    p_lvls <- vapply(spec$predictors, function(p) as.integer(p$level), integer(1))
    emit_level_corr_preds <- function(lvl_idx, z_sym, draw_n, expand_cluster) {
      k <- length(lvl_idx)
      if (k == 0L) return(character())
      idx_vec <- paste0("c(", paste(sprintf("%dL", lvl_idx), collapse = ", "), ")")
      out <- character()
      if (k == 1L) {
        out <- c(out, sprintf("  %s <- matrix(stats::rnorm(%s), nrow = %s, ncol = 1L)", z_sym, draw_n, draw_n))
      } else {
        out <- c(
          out,
          sprintf("  R_%s <- params$predictor_corr[%s, %s, drop = FALSE]", z_sym, idx_vec, idx_vec),
          sprintf("  %s <- matrix(stats::rnorm(%s * %dL), nrow = %s, ncol = %dL) %%*%% chol(R_%s)",
                  z_sym, draw_n, k, draw_n, k, z_sym)
        )
      }
      for (pos in seq_len(k)) {
        p <- spec$predictors[[lvl_idx[[pos]]]]
        z_col <- sprintf("%s[, %dL, drop = TRUE]", z_sym, pos)
        raw_expr <- switch(
          p$distribution,
          time = "params$time_spacing[wave_idx]",
          normal = sprintf("%s + %s * %s", fmt_num(p$mean), fmt_num(p$sd), z_col),
          binary = sprintf("as.integer(stats::pnorm(%s) < %s)", z_col, fmt_num(p$prob)),
          uniform = sprintf("%s + (%s - %s) * stats::pnorm(%s)",
                            fmt_num(p$min), fmt_num(p$max), fmt_num(p$min), z_col)
        )
        if (isTRUE(expand_cluster)) {
          out <- c(out, sprintf("  dat$%s <- (%s)[cluster_idx]", p$name, raw_expr))
        } else {
          out <- c(out, sprintf("  dat$%s <- %s", p$name, raw_expr))
        }
        if (identical(p$centering, "grand_mean")) {
          out <- c(out, sprintf("  dat$%s <- dat$%s - mean(dat$%s)", p$name, p$name, p$name))
        } else if (identical(p$centering, "group_mean") && levels_val == 2L) {
          out <- c(out, sprintf("  dat$%s <- dat$%s - stats::ave(dat$%s, dat$%s, FUN = mean)",
                                p$name, p$name, p$name, cl_name))
        }
      }
      out
    }

    lines <- c(
      lines,
      emit_level_corr_preds(which(p_lvls == 1L), "Z_l1", "n", FALSE),
      if (levels_val == 2L) emit_level_corr_preds(which(p_lvls == 2L), "Z_l2", "n_clusters", TRUE) else character()
    )
  } else {
    for (p in spec$predictors) {
      pname <- p$name
      is_l2 <- (levels_val == 2L && identical(as.integer(p$level), 2L))
      draw_n <- if (is_l2) "n_clusters" else "n"

      draw_expr <- switch(
        p$distribution,
        time = "params$time_spacing[wave_idx]",
        normal = sprintf("stats::rnorm(%s, mean = %s, sd = %s)", draw_n, fmt_num(p$mean), fmt_num(p$sd)),
        binary = sprintf("stats::rbinom(%s, size = 1L, prob = %s)", draw_n, fmt_num(p$prob)),
        uniform = sprintf("stats::runif(%s, min = %s, max = %s)", draw_n, fmt_num(p$min), fmt_num(p$max))
      )

      if (is_l2) {
        lines <- c(lines, sprintf("  dat$%s <- %s[cluster_idx]", pname, draw_expr))
      } else {
        lines <- c(lines, sprintf("  dat$%s <- %s", pname, draw_expr))
      }

      if (identical(p$centering, "grand_mean")) {
        lines <- c(lines, sprintf("  dat$%s <- dat$%s - mean(dat$%s)", pname, pname, pname))
      } else if (identical(p$centering, "group_mean") && levels_val == 2L) {
        lines <- c(lines, sprintf("  dat$%s <- dat$%s - stats::ave(dat$%s, dat$%s, FUN = mean)", pname, pname, pname, cl_name))
      }
    }
  }

  # Linear predictor (fixed main effects + interactions)
  lines <- c(lines, "  eta <- rep(params$intercept, n)")
  for (p in spec$predictors) {
    lines <- c(lines, sprintf("  eta <- eta + params$fixed$%s * dat$%s", p$name, p$name))
  }
  pred_names <- vapply(spec$predictors, `[[`, character(1), "name")
  for (it in spec$effects$interactions %||% list()) {
    ct <- canonical_interaction_terms(it$terms, pred_names)
    iname <- paste(ct, collapse = ":")
    lines <- c(lines, sprintf("  eta <- eta + params$interactions[[%s]] * dat$%s * dat$%s",
                              deparse1(iname), ct[[1L]], ct[[2L]]))
  }

  # Random effects (2-level only)
  if (levels_val == 2L) {
    has_rint <- isTRUE(spec$effects$random$intercept)
    slopes <- spec$effects$random$slopes %||% list()
    q_rand <- as.integer(has_rint) + length(slopes)

    if (q_rand > 0L) {
      l1_var_expr <- if (fam == "gaussian") {
        "params$residual_sd^2"
      } else if (fam == "binomial" && lnk == "logit") {
        "(pi^2) / 3"
      } else {
        "1.0"
      }

      icc_in_grid <- "icc" %in% names(spec$simulation$parameter_grid %||% list())
      if (has_rint) {
        if (!is.null(spec$effects$random$intercept_sd) && !icc_in_grid) {
          lines <- c(lines, "  tau0 <- params$intercept_sd")
        } else {
          lines <- c(
            lines,
            sprintf("  l1_var <- %s", l1_var_expr),
            "  tau0 <- if (params$icc > 0) sqrt(l1_var * params$icc / (1 - params$icc)) else 0"
          )
        }
      }

      if (q_rand == 1L && has_rint) {
        lines <- c(
          lines,
          "  u0 <- if (tau0 > 0) stats::rnorm(n_clusters, mean = 0, sd = tau0) else rep(0, n_clusters)",
          "  eta <- eta + u0[cluster_idx]"
        )
      } else {
        sd_parts <- c(
          if (has_rint) "tau0" else character(),
          vapply(slopes, function(sl) sprintf("params$slope_sds[[%s]]", deparse1(sl$predictor)), character(1))
        )
        lines <- c(
          lines,
          sprintf("  re_sds <- unname(c(%s))", paste(sd_parts, collapse = ", ")),
          sprintf("  q_rand <- %dL", q_rand),
          "  U <- matrix(0, nrow = n_clusters, ncol = q_rand)",
          "  pos_idx <- which(re_sds > 0)",
          "  if (length(pos_idx) == 1L) {",
          "    U[, pos_idx] <- stats::rnorm(n_clusters, mean = 0, sd = re_sds[pos_idx])",
          "  } else if (length(pos_idx) > 1L) {",
          "    R_sub <- params$corr_matrix[pos_idx, pos_idx, drop = FALSE]",
          "    D_sub <- diag(re_sds[pos_idx], nrow = length(pos_idx))",
          "    Sigma_sub <- D_sub %*% R_sub %*% D_sub",
          "    U[, pos_idx] <- matrix(stats::rnorm(n_clusters * length(pos_idx)), nrow = n_clusters) %*% chol(Sigma_sub)",
          "  }"
        )
        col_i <- 1L
        if (has_rint) {
          lines <- c(lines, "  eta <- eta + U[cluster_idx, 1L, drop = TRUE]")
          col_i <- 2L
        }
        for (sl in slopes) {
          lines <- c(lines, sprintf("  eta <- eta + U[cluster_idx, %dL, drop = TRUE] * dat$%s", col_i, sl$predictor))
          col_i <- col_i + 1L
        }
      }
    }
  }

  # Outcome draw from closed family/link/error_dist/AR(1) switch
  if (fam == "gaussian") {
    edist <- spec$outcome$error_dist %||% "normal"
    if (identical(edist, "t")) {
      lines <- c(
        lines,
        "  eps_innov <- params$residual_sd * stats::rt(n, df = params$error_df) * sqrt((params$error_df - 2) / params$error_df)"
      )
    } else if (identical(edist, "skewed")) {
      lines <- c(
        lines,
        "  k_shape <- 4 / (params$error_skew^2)",
        "  g_raw <- stats::rgamma(n, shape = k_shape, rate = 1)",
        "  eps_innov <- params$residual_sd * sign(params$error_skew) * (g_raw - k_shape) / sqrt(k_shape)"
      )
    } else {
      lines <- c(lines, "  eps_innov <- stats::rnorm(n, mean = 0, sd = params$residual_sd)")
    }

    if (identical(spec$design_type, "longitudinal_growth")) {
      lines <- c(
        lines,
        "  eps <- eps_innov",
        "  if (abs(params$ar1_rho) > 0 && cluster_size >= 2L) {",
        "    rho <- params$ar1_rho",
        "    for (s_i in seq_len(n_clusters)) {",
        "      idx_s <- ((s_i - 1L) * cluster_size + 1L):(s_i * cluster_size)",
        "      for (w_t in 2L:cluster_size) {",
        "        dt_w <- if (rho >= 0) params$time_spacing[w_t] - params$time_spacing[w_t - 1L] else 1",
        "        phi_w <- if (rho >= 0) rho^dt_w else rho",
        "        eps[idx_s[w_t]] <- phi_w * eps[idx_s[w_t - 1L]] + sqrt(1 - phi_w^2) * eps_innov[idx_s[w_t]]",
        "      }",
        "    }",
        "  }",
        sprintf("  dat$%s <- eta + eps", y_name)
      )
    } else {
      lines <- c(lines, sprintf("  dat$%s <- eta + eps_innov", y_name))
    }
  } else {
    outcome_expr <- if (fam == "binomial" && lnk == "logit") {
      "stats::rbinom(n, size = 1L, prob = stats::plogis(eta))"
    } else if (fam == "binomial" && lnk == "probit") {
      "stats::rbinom(n, size = 1L, prob = stats::pnorm(eta))"
    } else {
      "stats::rpois(n, lambda = exp(pmin(eta, 15)))"
    }
    lines <- c(lines, sprintf("  dat$%s <- %s", y_name, outcome_expr))
  }

  # Missingness mechanism
  mech <- spec$missingness$mechanism
  if (identical(mech, "MCAR")) {
    lines <- c(
      lines,
      "  if (params$missing_rate > 0) {",
      "    is_miss <- stats::rbinom(n, size = 1L, prob = params$missing_rate) == 1L",
      sprintf("    dat$%s[is_miss] <- NA", y_name),
      "  }"
    )
  } else if (identical(mech, "MAR")) {
    aux <- spec$missingness$auxiliary
    lines <- c(
      lines,
      "  if (params$missing_rate > 0) {",
      sprintf("    aux_x <- as.numeric(dat$%s)", aux),
      "    aux_sd <- stats::sd(aux_x)",
      "    aux_z <- if (is.finite(aux_sd) && aux_sd > 0) (aux_x - mean(aux_x)) / aux_sd else rep(0, n)",
      "    gamma <- params$missing_strength",
      "    target_rate <- params$missing_rate",
      "    a0 <- tryCatch(",
      "      stats::uniroot(function(a) mean(stats::plogis(a + gamma * aux_z)) - target_rate, interval = c(-25, 25))$root,",
      "      error = function(e) stats::qlogis(target_rate)",
      "    )",
      "    p_miss <- stats::plogis(a0 + gamma * aux_z)",
      "    is_miss <- stats::rbinom(n, size = 1L, prob = p_miss) == 1L",
      sprintf("    dat$%s[is_miss] <- NA", y_name),
      "  }"
    )
  } else if (identical(mech, "monotone_dropout")) {
    has_aux <- !is.null(spec$missingness$auxiliary)
    lines <- c(
      lines,
      "  if (params$missing_rate > 0 && cluster_size >= 2L) {",
      "    h_base <- 1 - (1 - params$missing_rate)^(1 / (cluster_size - 1L))",
      if (has_aux) c(
        sprintf("    aux_subj <- as.numeric(dat$%s[seq_len(n_clusters) * cluster_size - (cluster_size - 1L)])", spec$missingness$auxiliary),
        "    aux_sd <- stats::sd(aux_subj)",
        "    aux_z <- if (is.finite(aux_sd) && aux_sd > 0) (aux_subj - mean(aux_subj)) / aux_sd else rep(0, n_clusters)",
        "    gamma_d <- params$missing_strength",
        "    a0_d <- tryCatch(",
        "      stats::uniroot(function(a) mean(1 - (1 - stats::plogis(a + gamma_d * aux_z))^(cluster_size - 1L)) - params$missing_rate, interval = c(-25, 25))$root,",
        "      error = function(e) stats::qlogis(h_base)",
        "    )",
        "    h_subj <- stats::plogis(a0_d + gamma_d * aux_z)"
      ) else "    h_subj <- rep(h_base, n_clusters)",
      "    for (s_i in seq_len(n_clusters)) {",
      "      idx_s <- ((s_i - 1L) * cluster_size + 1L):(s_i * cluster_size)",
      "      dropped <- FALSE",
      "      for (w_t in 2L:cluster_size) {",
      "        if (!dropped && stats::rbinom(1L, size = 1L, prob = h_subj[s_i]) == 1L) {",
      "          dropped <- TRUE",
      "        }",
      "        if (dropped) {",
      sprintf("          dat$%s[idx_s[w_t]] <- NA", y_name),
      "        }",
      "      }",
      "    }",
      "  }"
    )
  }

  lines <- c(lines, "  dat", "}")
  paste(lines, collapse = "\n")
}

build_fit_block <- function(spec, formula_str, focal) {
  engine <- spec$estimator$engine
  fam <- spec$outcome$family
  lnk <- spec$outcome$link

  fit_body <- switch(
    engine,
    lm = c(
      sprintf("      fit <- stats::lm(%s, data = dat_cc)", formula_str),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L, drop = TRUE])",
      "      se <- unname(sm[focal, 2L, drop = TRUE])",
      "      p_val <- unname(sm[focal, 4L, drop = TRUE])",
      "      crit <- stats::qt(1 - (1 - params$conf_level) / 2, df = fit$df.residual)",
      "      conv_ok <- is.finite(est) && is.finite(se) && se > 0"
    ),
    glm = c(
      sprintf("      fit <- stats::glm(%s, data = dat_cc, family = stats::%s(link = %s))",
              formula_str, fam, deparse1(lnk)),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L, drop = TRUE])",
      "      se <- unname(sm[focal, 2L, drop = TRUE])",
      "      p_val <- unname(sm[focal, 4L, drop = TRUE])",
      "      crit <- stats::qnorm(1 - (1 - params$conf_level) / 2)",
      "      conv_ok <- isTRUE(fit$converged) && is.finite(est) && is.finite(se) && se > 0"
    ),
    lmer = c(
      sprintf("      fit <- suppressMessages(lme4::lmer(%s, data = dat_cc, REML = isTRUE(params$reml)))", formula_str),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L, drop = TRUE])",
      "      se <- unname(sm[focal, 2L, drop = TRUE])",
      "      t_val <- unname(sm[focal, 3L, drop = TRUE])",
      "      p_val <- 2 * stats::pnorm(-abs(t_val))",
      "      crit <- stats::qnorm(1 - (1 - params$conf_level) / 2)",
      "      is_sing <- lme4::isSingular(fit)",
      "      opt_code <- fit@optinfo$conv$opt",
      "      opt_msgs <- fit@optinfo$conv$lme4$messages",
      "      opt_ok <- (is.null(opt_code) || isTRUE(opt_code == 0)) && (is.null(opt_msgs) || length(opt_msgs) == 0L)",
      "      conv_ok <- !is_sing && opt_ok && is.finite(est) && is.finite(se) && se > 0"
    ),
    glmer = c(
      sprintf("      fit <- suppressMessages(lme4::glmer(%s, data = dat_cc, family = stats::%s(link = %s)))",
              formula_str, fam, deparse1(lnk)),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L, drop = TRUE])",
      "      se <- unname(sm[focal, 2L, drop = TRUE])",
      "      p_val <- unname(sm[focal, 4L, drop = TRUE])",
      "      crit <- stats::qnorm(1 - (1 - params$conf_level) / 2)",
      "      is_sing <- lme4::isSingular(fit)",
      "      opt_code <- fit@optinfo$conv$opt",
      "      opt_msgs <- fit@optinfo$conv$lme4$messages",
      "      opt_ok <- (is.null(opt_code) || isTRUE(opt_code == 0)) && (is.null(opt_msgs) || length(opt_msgs) == 0L)",
      "      conv_ok <- !is_sing && opt_ok && is.finite(est) && is.finite(se) && se > 0"
    )
  )

  lines <- c(
    "# --- 3. Model Fitting & Metric Extraction ---",
    "fit_and_extract <- function(dat, params) {",
    "  dat_cc <- stats::na.omit(dat)",
    "  focal <- params$focal_predictor",
    "  true_beta <- if (!is.null(params$fixed[[focal]])) params$fixed[[focal]] else params$interactions[[focal]]",
    "  tryCatch(",
    "    {",
    fit_body,
    "      ci_low <- est - crit * se",
    "      ci_high <- est + crit * se",
    "      list(",
    "        converged = conv_ok,",
    "        est = est,",
    "        se = se,",
    "        p_value = p_val,",
    "        reject = is.finite(p_val) && (p_val < params$alpha),",
    "        covered = is.finite(ci_low) && is.finite(ci_high) && (true_beta >= ci_low && true_beta <= ci_high)",
    "      )",
    "    },",
    "    error = function(e) {",
    "      list(converged = FALSE, est = NA_real_, se = NA_real_, p_value = NA_real_, reject = NA, covered = NA)",
    "    }",
    "  )",
    "}"
  )

  paste(lines, collapse = "\n")
}

build_runner_block <- function(spec) {
  lines <- c(
    "# --- 4. Monte Carlo Simulation Runner ---",
    "run_simulation <- function(cfg) {",
    "  old_kind <- RNGkind()",
    "  has_seed <- exists(\".Random.seed\", envir = .GlobalEnv, inherits = FALSE)",
    "  old_seed <- if (has_seed) .GlobalEnv$.Random.seed else NULL",
    "  on.exit({",
    "    RNGkind(kind = old_kind[1L], normal.kind = old_kind[2L], sample.kind = old_kind[3L])",
    "    if (has_seed) {",
    "      .GlobalEnv$.Random.seed <- old_seed",
    "    }",
    "  }, add = TRUE)",
    "  set.seed(cfg$seed, kind = \"L'Ecuyer-CMRG\", normal.kind = \"Inversion\", sample.kind = \"Rejection\")",
    "  has_grid <- length(cfg$parameter_grid) > 0L",
    "  grid_df <- if (has_grid) {",
    "    expand.grid(cfg$parameter_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)",
    "  } else {",
    "    data.frame(cell_id = 1L)",
    "  }",
    "",
    "  summary_rows <- vector(\"list\", nrow(grid_df))",
    "  raw_rows <- vector(\"list\", nrow(grid_df))",
    "",
    "  for (g in seq_len(nrow(grid_df))) {",
    "    cell_params <- cfg",
    "    if (has_grid) {",
    "      for (gn in names(cfg$parameter_grid)) {",
    "        gval <- grid_df[[gn]][[g]]",
    "        if (gn == \"focal_effect\") {",
    "          fp <- cell_params$focal_predictor",
    "          if (!is.null(cell_params$fixed[[fp]])) {",
    "            cell_params$fixed[[fp]] <- gval",
    "          } else {",
    "            cell_params$interactions[[fp]] <- gval",
    "          }",
    "        } else {",
    "          cell_params[[gn]] <- gval",
    "        }",
    "      }",
    "    }",
    "",
    "    rep_res <- lapply(seq_len(cell_params$n_reps), function(r) {",
    "      dat <- simulate_one_dataset(cell_params)",
    "      fit_and_extract(dat, cell_params)",
    "    })",
    "",
    "    conv <- vapply(rep_res, function(x) x$converged, logical(1))",
    "    ests <- vapply(rep_res, function(x) x$est, numeric(1))",
    "    ses  <- vapply(rep_res, function(x) x$se, numeric(1))",
    "    rejs <- vapply(rep_res, function(x) x$reject, logical(1))",
    "    covs <- vapply(rep_res, function(x) x$covered, logical(1))",
    "    fp_now <- cell_params$focal_predictor",
    "    true_beta <- if (!is.null(cell_params$fixed[[fp_now]])) cell_params$fixed[[fp_now]] else cell_params$interactions[[fp_now]]",
    "",
    "    ok <- which(conv)",
    "    n_ok <- length(ok)",
    "    mean_est <- if (n_ok > 0L) mean(ests[ok]) else NA_real_",
    "    bias     <- if (n_ok > 0L) mean_est - true_beta else NA_real_",
    "    emp_se   <- if (n_ok > 1L) stats::sd(ests[ok]) else NA_real_",
    "    mean_se  <- if (n_ok > 0L) mean(ses[ok]) else NA_real_",
    "    rmse     <- if (n_ok > 0L) sqrt(mean((ests[ok] - true_beta)^2)) else NA_real_",
    "    power    <- if (n_ok > 0L) mean(rejs[ok]) else NA_real_",
    "    coverage <- if (n_ok > 0L) mean(covs[ok]) else NA_real_",
    "",
    "    cell_meta <- if (has_grid) grid_df[g, , drop = FALSE] else data.frame(cell_id = g)",
    "    summary_rows[[g]] <- cbind(",
    "      cell_meta,",
    "      data.frame(",
    "        n_reps = cell_params$n_reps,",
    "        n_converged = n_ok,",
    "        true_effect = true_beta,",
    "        mean_est = round(mean_est, 4),",
    "        bias = round(bias, 4),",
    "        emp_se = round(emp_se, 4),",
    "        mean_se = round(mean_se, 4),",
    "        rmse = round(rmse, 4),",
    "        power = round(power, 4),",
    "        coverage = round(coverage, 4),",
    "        stringsAsFactors = FALSE",
    "      )",
    "    )",
    "    raw_rows[[g]] <- data.frame(",
    "      cell_id = g,",
    "      rep = seq_len(cell_params$n_reps),",
    "      converged = conv,",
    "      est = ests,",
    "      se = ses,",
    "      reject = rejs,",
    "      covered = covs,",
    "      stringsAsFactors = FALSE",
    "    )",
    "  }",
    "",
    "  summary_df <- summary_rows[[1L]]",
    "  raw_df <- raw_rows[[1L]]",
    "  if (length(summary_rows) > 1L) {",
    "    for (k in 2L:length(summary_rows)) {",
    "      summary_df <- rbind(summary_df, summary_rows[[k]])",
    "      raw_df <- rbind(raw_df, raw_rows[[k]])",
    "    }",
    "  }",
    "",
    "  list(",
    "    summary = summary_df,",
    "    raw = raw_df",
    "  )",
    "}"
  )

  paste(lines, collapse = "\n")
}
