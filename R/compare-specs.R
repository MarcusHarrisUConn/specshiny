#' Compare two or more validated `spec_ir` study designs side by side
#'
#' Computes analytical design efficiency metrics for two or more validated
#' `spec_ir` objects---including total sample size \eqn{N}, the Kish /
#' Eldridge-Ashby-Kerry cluster design effect
#' \deqn{\mathrm{DEFF} = 1 + \left(\bar{n}_j (1 + \mathrm{CV}^2) - 1\right)\mathrm{ICC},}
#' effective sample size \eqn{N_{\mathrm{eff}} = N / \mathrm{DEFF}}, level-1
#' and level-2 variance decomposition, AR(1) autocorrelation, synthesized UI
#' control counts, pairwise semantic IR diffs against the baseline scenario,
#' and optional bounded Monte Carlo comparative power estimates.
#'
#' @param ... Two or more validated `spec_ir` objects (named or unnamed), or a
#'   single list of two or more validated `spec_ir` objects.
#' @param run_power Logical; if `TRUE`, runs a bounded comparative preview
#'   simulation via the pure-R preview simulator for each scenario and appends
#'   estimated power, bias, empirical mean SE, and CI coverage columns.
#' @param preview_reps Positive integer number of Monte Carlo replications per
#'   scenario when `run_power = TRUE` (default `20L`, capped at `500L`).
#' @param max_preview_n Positive integer observation budget per replication
#'   when `run_power = TRUE` (default `2000L`).
#' @param max_preview_seconds Positive numeric wall-clock budget in seconds per
#'   scenario when `run_power = TRUE` (default `5`).
#' @param seed Optional integer random seed override applied to every scenario
#'   when `run_power = TRUE` so scenarios share a common seed baseline.
#'
#' @return An S3 object of class `spec_comparison` containing:
#'   - `summary`: `data.frame` with one row per scenario and analytical
#'     (plus optional power) comparison metrics.
#'   - `baseline`: Character scalar naming the baseline (first) scenario.
#'   - `diffs`: Named list of [diff_spec_ir()] objects comparing each
#'     non-baseline scenario to the baseline.
#'   - `deltas`: `data.frame` of numeric metric deltas relative to baseline.
#'   - `run_power`: Logical indicating whether comparative power was run.
#'   - `specs`: Named list of the validated `spec_ir` objects compared.
#' @export
#' @examples
#' s_ols <- new_spec_ir(
#'   design_type = "monte_carlo_lm",
#'   hierarchy = list(levels = 1L, n_obs = 400L)
#' )
#' s_mlm <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 10L, icc = 0.2)
#' )
#' s_unbal <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   hierarchy = list(
#'     levels = 2L, n_clusters = 40L, cluster_size = 10L,
#'     cluster_size_dist = "normal", cluster_size_sd = 4, icc = 0.2
#'   )
#' )
#' cmp <- compare_specs(ols = s_ols, balanced_mlm = s_mlm, unbalanced_mlm = s_unbal)
#' cmp
#' as.data.frame(cmp)[, c("scenario", "total_n", "deff", "n_eff")]
compare_specs <- function(...,
                          run_power = FALSE,
                          preview_reps = 20L,
                          max_preview_n = 2000L,
                          max_preview_seconds = 5,
                          seed = NULL) {
  dots <- list(...)
  if (length(dots) == 1L && is.list(dots[[1L]]) && !is_spec_ir(dots[[1L]])) {
    specs <- dots[[1L]]
  } else {
    specs <- dots
  }

  if (length(specs) < 2L) {
    cli::cli_abort("{.fn compare_specs} requires at least two {.cls spec_ir} objects.")
  }

  raw_names <- names(specs)
  if (is.null(raw_names)) {
    raw_names <- rep("", length(specs))
  }
  default_names <- paste0("spec_", seq_along(specs))
  scen_names <- ifelse(nzchar(trimws(raw_names)), trimws(raw_names), default_names)
  scen_names <- make.unique(scen_names, sep = "_")
  names(specs) <- scen_names

  for (i in seq_along(specs)) {
    if (!is_spec_ir(specs[[i]])) {
      cli::cli_abort("Scenario {.val {scen_names[[i]]}} must be a {.cls spec_ir} object.")
    }
    assert_valid_spec_ir(specs[[i]])
  }

  if (!is_lgl_scalar(run_power)) {
    cli::cli_abort("{.arg run_power} must be TRUE or FALSE.")
  }
  if (isTRUE(run_power)) {
    if (!is_whole_scalar(preview_reps) || as.integer(preview_reps) < 1L || as.integer(preview_reps) > 500L) {
      cli::cli_abort("{.arg preview_reps} must be an integer between 1 and 500.")
    }
    if (!is_whole_scalar(max_preview_n) || as.integer(max_preview_n) < 20L) {
      cli::cli_abort("{.arg max_preview_n} must be an integer >= 20.")
    }
    if (!is_num_scalar(max_preview_seconds) || max_preview_seconds <= 0) {
      cli::cli_abort("{.arg max_preview_seconds} must be a positive number.")
    }
    if (!is.null(seed) && !is_whole_scalar(seed)) {
      cli::cli_abort("{.arg seed} must be NULL or a finite integer scalar.")
    }
  }

  rows <- lapply(seq_along(specs), function(i) {
    nm <- scen_names[[i]]
    sp <- specs[[i]]
    lvls <- as.integer(sp$hierarchy$levels)
    n_cl <- if (lvls == 2L) as.integer(sp$hierarchy$n_clusters) else NA_integer_
    cs <- if (lvls == 2L) as.integer(sp$hierarchy$cluster_size) else NA_integer_
    cs_dist <- if (lvls == 2L) as.character(sp$hierarchy$cluster_size_dist %||% "fixed") else "none"

    # Exact cluster-size moments accounting for left-censoring at min_nj
    min_nj <- if (lvls == 2L) {
      max(2L, as.integer(isTRUE(sp$effects$random$intercept)) + length(sp$effects$random$slopes %||% list()) + 1L)
    } else {
      1L
    }

    if (lvls == 1L) {
      eff_mean_cs <- NA_real_
      cv <- 0
      total_n <- as.numeric(spec_total_n(sp))
    } else if (identical(cs_dist, "normal")) {
      mu_c <- as.numeric(cs)
      sd_c <- as.numeric(sp$hierarchy$cluster_size_sd)
      a_c <- as.numeric(min_nj)
      alpha_c <- (a_c - mu_c) / sd_c
      Phi_a <- stats::pnorm(alpha_c)
      phi_a <- stats::dnorm(alpha_c)
      # Exact moments of left-censored Y = max(a, X), X ~ N(mu, sd^2)
      eff_mean_cs <- mu_c + sd_c * (alpha_c * Phi_a + phi_a)
      e_y2 <- (a_c^2) * Phi_a + (mu_c^2 + sd_c^2) * (1 - Phi_a) + sd_c * (mu_c + a_c) * phi_a
      var_y <- max(0, e_y2 - eff_mean_cs^2)
      cv <- if (eff_mean_cs > 0) sqrt(var_y) / eff_mean_cs else 0
      total_n <- round(as.numeric(n_cl) * eff_mean_cs)
    } else if (identical(cs_dist, "poisson")) {
      lam <- as.numeric(cs)
      ks <- seq.int(0L, min_nj - 1L)
      pk <- stats::dpois(ks, lambda = lam)
      eff_mean_cs <- lam + sum((min_nj - ks) * pk)
      e_y2 <- lam * (lam + 1) + sum((min_nj^2 - ks^2) * pk)
      var_y <- max(0, e_y2 - eff_mean_cs^2)
      cv <- if (eff_mean_cs > 0) sqrt(var_y) / eff_mean_cs else 0
      total_n <- round(as.numeric(n_cl) * eff_mean_cs)
    } else {
      eff_mean_cs <- as.numeric(cs)
      cv <- 0
      total_n <- as.numeric(spec_total_n(sp))
    }

    icc_v <- if (lvls == 2L) as.numeric(sp$hierarchy$icc %||% 0) else 0
    deff <- if (lvls == 1L) {
      1
    } else {
      1 + (eff_mean_cs * (1 + cv^2) - 1) * icc_v
    }
    n_eff <- as.numeric(total_n) / deff

    v1 <- outcome_level1_variance(sp$outcome)
    v2 <- if (lvls == 2L) spec_implied_intercept_sd(sp)^2 else 0
    v_tot <- v1 + v2
    prop_l2 <- if (v_tot > 0) v2 / v_tot else 0

    ar1_v <- if (identical(sp$design_type, "longitudinal_growth") && !is.null(sp$outcome$ar1_rho)) {
      as.numeric(sp$outcome$ar1_rho)
    } else {
      NA_real_
    }

    focal_term <- sp$effects$focal_predictor %||% sp$predictors[[1L]]$name
    focal_b <- as.numeric(spec_focal_beta(sp))
    miss_lbl <- if (identical(sp$missingness$mechanism, "none")) {
      "none"
    } else {
      sprintf("%s (%.0f%%)", sp$missingness$mechanism, 100 * as.numeric(sp$missingness$rate))
    }
    ctrl_cnt <- as.integer(spec_to_controls(sp)$control_count)

    row_df <- data.frame(
      scenario = nm,
      design_type = as.character(sp$design_type),
      levels = lvls,
      n_clusters = n_cl,
      cluster_size = cs,
      total_n = total_n,
      cluster_size_dist = cs_dist,
      cluster_size_cv = round(cv, 4),
      icc = round(icc_v, 4),
      deff = round(deff, 4),
      n_eff = round(n_eff, 2),
      var_level1 = round(v1, 4),
      var_level2 = round(v2, 4),
      var_total = round(v_tot, 4),
      prop_level2 = round(prop_l2, 4),
      ar1_rho = if (is.na(ar1_v)) NA_real_ else round(ar1_v, 4),
      estimator = as.character(sp$estimator$engine),
      focal_term = as.character(focal_term),
      focal_effect = round(focal_b, 4),
      n_predictors = as.integer(length(sp$predictors)),
      n_random_slopes = as.integer(length(sp$effects$random$slopes %||% list())),
      missingness = miss_lbl,
      n_controls = ctrl_cnt,
      stringsAsFactors = FALSE
    )

    if (isTRUE(run_power)) {
      sp_run <- sp
      sp_run$simulation$n_reps <- as.integer(preview_reps)
      if (!is.null(seed)) {
        sp_run$simulation$seed <- as.integer(seed)
      }
      sim_out <- run_spec_preview_sim(
        sp_run,
        max_reps = as.integer(preview_reps),
        max_n = as.integer(max_preview_n),
        max_seconds = max_preview_seconds
      )
      sm <- sim_out$summary
      row_df$est_power <- sm$power[[1L]]
      row_df$est_bias <- sm$bias[[1L]]
      row_df$est_mean_se <- sm$mean_se[[1L]]
      row_df$est_coverage <- sm$coverage[[1L]]
      row_df$preview_reps <- as.integer(sm$n_reps[[1L]])
      row_df$preview_converged <- as.integer(sm$n_converged[[1L]])
    }

    row_df
  })

  summary_df <- do.call(rbind, rows)
  rownames(summary_df) <- NULL

  base_nm <- scen_names[[1L]]
  base_sp <- specs[[1L]]
  diffs <- list()
  for (i in seq.int(2L, length(specs))) {
    diffs[[scen_names[[i]]]] <- diff_spec_ir(base_sp, specs[[i]])
  }

  deltas_df <- data.frame(
    scenario = summary_df$scenario,
    delta_total_n = summary_df$total_n - summary_df$total_n[[1L]],
    delta_deff = round(summary_df$deff - summary_df$deff[[1L]], 4),
    delta_n_eff = round(summary_df$n_eff - summary_df$n_eff[[1L]], 2),
    delta_n_controls = summary_df$n_controls - summary_df$n_controls[[1L]],
    n_changed_paths = c(0L, unname(vapply(diffs, function(d) as.integer(d$n_changes), integer(1)))),
    stringsAsFactors = FALSE
  )
  rownames(deltas_df) <- NULL
  if (isTRUE(run_power)) {
    deltas_df$delta_power <- round(summary_df$est_power - summary_df$est_power[[1L]], 4)
  }

  structure(
    list(
      summary = summary_df,
      baseline = base_nm,
      diffs = diffs,
      deltas = deltas_df,
      run_power = isTRUE(run_power),
      specs = specs
    ),
    class = "spec_comparison"
  )
}

#' Coerce a `spec_comparison` object to a data frame
#'
#' @param x A `spec_comparison` object returned by [compare_specs()].
#' @param row.names Optional character vector of row names.
#' @param optional Logical; passed to `as.data.frame` semantics (unused).
#' @param ... Unused additional arguments.
#'
#' @return A `data.frame` containing the side-by-side scenario metrics.
#' @export
as.data.frame.spec_comparison <- function(x, row.names = NULL, optional = FALSE, ...) {
  df <- x$summary
  if (!is.null(row.names)) {
    rownames(df) <- row.names
  }
  df
}

#' @export
print.spec_comparison <- function(x, ...) {
  n_sc <- nrow(x$summary)
  cli::cli_h2("spec_comparison ({n_sc} scenarios; baseline = '{x$baseline}')")
  cols <- c("scenario", "design_type", "total_n", "icc", "deff", "n_eff", "focal_effect", "missingness")
  if (any(!is.na(x$summary$ar1_rho))) {
    cols <- c(cols, "ar1_rho")
  }
  if (isTRUE(x$run_power)) {
    cols <- c(cols, "est_power", "est_mean_se")
  }
  tbl <- x$summary[, cols, drop = FALSE]
  print(tbl, row.names = FALSE)

  cli::cli_h3("Differences vs. baseline ('{x$baseline}')")
  for (nm in names(x$diffs)) {
    d <- x$diffs[[nm]]
    idx <- which(x$deltas$scenario == nm)[[1L]]
    d_neff <- x$deltas$delta_n_eff[[idx]]
    d_deff <- x$deltas$delta_deff[[idx]]
    pwr_txt <- if (isTRUE(x$run_power)) {
      sprintf(", delta power = %+.3f", x$deltas$delta_power[[idx]])
    } else {
      ""
    }
    line_txt <- sprintf(
      "%s: %d IR path diff(s) | delta N_eff = %+.1f, delta DEFF = %+.3f%s",
      nm, d$n_changes, d_neff, d_deff, pwr_txt
    )
    cli::cli_bullets(c("*" = "{line_txt}"))
  }
  invisible(x)
}
