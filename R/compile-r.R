#' Deterministically compile a validated `spec_ir` into a standalone R script
#'
#' Compiles any schema- and statistically-validated `spec_ir` into a clean,
#' readable, self-contained, and executable R simulation script using audited
#' code templates. Never evaluates untrusted text or language-model strings;
#' all identifiers and parameters pass [assert_valid_spec_ir()] and strict
#' literal formatting before emission.
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
#' cat(substr(code, 1, 400))
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
  y_name <- spec$outcome$name
  focal <- spec$effects$focal_predictor %||% spec$predictors[[1L]]$name

  sections <- character()

  # 1. Header comment (sanitized comments only)
  if (isTRUE(include_header)) {
    header_lines <- c(
      "# =============================================================================",
      sprintf("# %s", sanitize_comment(spec$metadata$title)),
      "# Generated deterministically by specshiny (zero eval/parse of untrusted text)",
      sprintf("# IR Schema Version : %s", sanitize_comment(spec$schema_version)),
      sprintf("# Design Type       : %s (%d-level, %s / %s link)",
              sanitize_comment(spec$design_type), levels_val, family, link),
      sprintf("# Model Formula     : %s", sanitize_comment(math_info$formula)),
      sprintf("# Estimator Engine  : %s", sanitize_comment(engine)),
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

  if (!is.null(file)) {
    writeLines(script, con = file, useBytes = TRUE)
    return(invisible(script))
  }
  script
}

# ==============================================================================
# Internal Deterministic Template Builders
# ==============================================================================

sanitize_comment <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(x)
}

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
    sprintf("  title = %s,", deparse1(sanitize_comment(spec$metadata$title))),
    sprintf("  design_type = %s,", deparse1(spec$design_type)),
    sprintf("  levels = %s,", fmt_int(levels_val))
  )

  if (levels_val == 1L) {
    lines <- c(lines, sprintf("  n_obs = %s,", fmt_int(spec$hierarchy$n_obs)))
  } else {
    lines <- c(
      lines,
      sprintf("  n_clusters = %s,", fmt_int(spec$hierarchy$n_clusters)),
      sprintf("  cluster_size = %s,", fmt_int(spec$hierarchy$cluster_size)),
      sprintf("  icc = %s,", fmt_num(spec$hierarchy$icc))
    )
  }

  if (identical(spec$outcome$family, "gaussian")) {
    lines <- c(lines, sprintf("  residual_sd = %s,", fmt_num(spec$outcome$residual_sd)))
  }

  lines <- c(
    lines,
    sprintf("  intercept = %s,", fmt_num(spec$effects$intercept)),
    sprintf("  focal_predictor = %s,", deparse1(focal))
  )

  fixed_entries <- vapply(spec$predictors, function(p) {
    b <- spec$effects$fixed[[p$name]] %||% 0
    sprintf("%s = %s", p$name, fmt_num(b))
  }, character(1))
  lines <- c(lines, sprintf("  fixed = list(%s),", paste(fixed_entries, collapse = ", ")))

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
  if (identical(spec$missingness$mechanism, "MAR")) {
    lines <- c(
      lines,
      sprintf("  missing_auxiliary = %s,", deparse1(spec$missingness$auxiliary)),
      sprintf("  missing_strength = %s,", fmt_num(spec$missingness$strength))
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
    lines <- c(
      lines,
      "  n_clusters <- as.integer(params$n_clusters)",
      "  cluster_size <- as.integer(params$cluster_size)",
      "  n <- n_clusters * cluster_size",
      "  cluster_idx <- rep(seq_len(n_clusters), each = cluster_size)",
      sprintf("  dat <- data.frame(%s = factor(cluster_idx))", cl_name)
    )
  }

  # Generate each predictor deterministically from closed distribution switch
  for (p in spec$predictors) {
    pname <- p$name
    is_l2 <- (levels_val == 2L && identical(as.integer(p$level), 2L))
    draw_n <- if (is_l2) "n_clusters" else "n"

    draw_expr <- switch(
      p$distribution,
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

  # Linear predictor (fixed part)
  lines <- c(lines, "  eta <- rep(params$intercept, n)")
  for (p in spec$predictors) {
    lines <- c(lines, sprintf("  eta <- eta + params$fixed$%s * dat$%s", p$name, p$name))
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

      if (has_rint) {
        if (!is.null(spec$effects$random$intercept_sd)) {
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
          vapply(slopes, function(sl) sprintf("params$slope_sds[%s]", deparse1(sl$predictor)), character(1))
        )
        lines <- c(
          lines,
          sprintf("  re_sds <- as.numeric(c(%s))", paste(sd_parts, collapse = ", ")),
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
          lines <- c(lines, "  eta <- eta + U[cluster_idx, 1L]")
          col_i <- 2L
        }
        for (sl in slopes) {
          lines <- c(lines, sprintf("  eta <- eta + U[cluster_idx, %dL] * dat$%s", col_i, sl$predictor))
          col_i <- col_i + 1L
        }
      }
    }
  }

  # Outcome draw from closed family/link switch
  outcome_expr <- if (fam == "gaussian") {
    "stats::rnorm(n, mean = eta, sd = params$residual_sd)"
  } else if (fam == "binomial" && lnk == "logit") {
    "stats::rbinom(n, size = 1L, prob = stats::plogis(eta))"
  } else if (fam == "binomial" && lnk == "probit") {
    "stats::rbinom(n, size = 1L, prob = stats::pnorm(eta))"
  } else {
    "stats::rpois(n, lambda = exp(pmin(eta, 15)))"
  }
  lines <- c(lines, sprintf("  dat$%s <- %s", y_name, outcome_expr))

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
      "      stats::uniroot(function(a) mean(stats::plogis(a + gamma * aux_z)) - target_rate, interval = c(-20, 20))$root,",
      "      error = function(e) stats::qlogis(target_rate)",
      "    )",
      "    p_miss <- stats::plogis(a0 + gamma * aux_z)",
      "    is_miss <- stats::rbinom(n, size = 1L, prob = p_miss) == 1L",
      sprintf("    dat$%s[is_miss] <- NA", y_name),
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
      "      est <- unname(sm[focal, 1L])",
      "      se <- unname(sm[focal, 2L])",
      "      p_val <- unname(sm[focal, 4L])",
      "      crit <- stats::qt(1 - (1 - params$conf_level) / 2, df = fit$df.residual)"
    ),
    glm = c(
      sprintf("      fit <- stats::glm(%s, data = dat_cc, family = stats::%s(link = %s))",
              formula_str, fam, deparse1(lnk)),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L])",
      "      se <- unname(sm[focal, 2L])",
      "      p_val <- unname(sm[focal, 4L])",
      "      crit <- stats::qnorm(1 - (1 - params$conf_level) / 2)"
    ),
    lmer = c(
      sprintf("      fit <- suppressMessages(lme4::lmer(%s, data = dat_cc, REML = isTRUE(params$reml)))", formula_str),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L])",
      "      se <- unname(sm[focal, 2L])",
      "      t_val <- unname(sm[focal, 3L])",
      "      p_val <- 2 * stats::pnorm(-abs(t_val))",
      "      crit <- stats::qnorm(1 - (1 - params$conf_level) / 2)"
    ),
    glmer = c(
      sprintf("      fit <- suppressMessages(lme4::glmer(%s, data = dat_cc, family = stats::%s(link = %s)))",
              formula_str, fam, deparse1(lnk)),
      "      sm <- stats::coef(summary(fit))",
      "      est <- unname(sm[focal, 1L])",
      "      se <- unname(sm[focal, 2L])",
      "      p_val <- unname(sm[focal, 4L])",
      "      crit <- stats::qnorm(1 - (1 - params$conf_level) / 2)"
    )
  )

  lines <- c(
    "# --- 3. Model Fitting & Metric Extraction ---",
    "fit_and_extract <- function(dat, params) {",
    "  dat_cc <- stats::na.omit(dat)",
    "  focal <- params$focal_predictor",
    "  true_beta <- params$fixed[[focal]]",
    "  tryCatch(",
    "    {",
    fit_body,
    "      ci_low <- est - crit * se",
    "      ci_high <- est + crit * se",
    "      list(",
    "        converged = is.finite(est) && is.finite(se) && se > 0,",
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
    "  set.seed(cfg$seed)",
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
    "          cell_params$fixed[[cell_params$focal_predictor]] <- gval",
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
    "    conv <- vapply(rep_res, `[[`, logical(1), \"converged\")",
    "    ests <- vapply(rep_res, `[[`, numeric(1), \"est\")",
    "    ses  <- vapply(rep_res, `[[`, numeric(1), \"se\")",
    "    rejs <- vapply(rep_res, `[[`, logical(1), \"reject\")",
    "    covs <- vapply(rep_res, `[[`, logical(1), \"covered\")",
    "    true_beta <- cell_params$fixed[[cell_params$focal_predictor]]",
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
    "  list(",
    "    summary = do.call(rbind, summary_rows),",
    "    raw = do.call(rbind, raw_rows)",
    "  )",
    "}"
  )

  paste(lines, collapse = "\n")
}
