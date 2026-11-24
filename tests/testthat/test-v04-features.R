test_that("longitudinal_growth IR v1.2.0 builds, round-trips JSON/YAML, diffs, and enforces guardrails", {
  sp <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(
      levels = 2L,
      n_clusters = 90L,
      time_points = 5L,
      time_spacing = c(0, 1, 2, 4, 6),
      icc = 0.35
    ),
    predictors = list(
      spec_predictor("time", level = 1L, distribution = "time"),
      spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5, role = "treatment")
    ),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, ar1_rho = 0.45),
    effects = list(
      intercept = 0,
      fixed = list(time = 0.35, treatment = 0.10),
      interactions = list(spec_interaction("time", "treatment", beta = 0.25)),
      focal_predictor = "time:treatment",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "time", sd = 0.20)),
        corr_matrix = matrix(c(1, 0.15, 0.15, 1), nrow = 2L)
      )
    ),
    missingness = list(mechanism = "monotone_dropout", rate = 0.25),
    simulation = list(n_reps = 50L, seed = 2026L, parameter_grid = list(ar1_rho = c(0, 0.4)))
  )

  expect_true(is_valid_spec_ir(sp))
  expect_equal(sp$schema_version, "1.2.0")
  expect_equal(sp$hierarchy$time_points, 5L)
  expect_equal(sp$hierarchy$cluster_size, 5L)
  expect_equal(sp$hierarchy$time_spacing, c(0, 1, 2, 4, 6))
  expect_equal(sp$outcome$ar1_rho, 0.45)

  # JSON & YAML round-trip
  f_json <- tempfile(fileext = ".json")
  f_yaml <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(f_json, f_yaml)), add = TRUE)
  write_spec_ir(sp, f_json)
  write_spec_ir(sp, f_yaml)
  sp_j <- read_spec_ir(f_json)
  sp_y <- read_spec_ir(f_yaml)
  expect_false(diff_spec_ir(sp, sp_j)$has_changes)
  expect_false(diff_spec_ir(sp, sp_y)$has_changes)

  # Print, summary, math, Quarto, and AST-verified R codegen
  expect_no_error(suppressMessages(print(sp)))
  expect_true(inherits(summary(sp), "summary.spec_ir"))
  m <- spec_to_math(sp)
  expect_true(grepl("AR\\(1\\)", m$latex))
  expect_true(grepl("monotone", m$latex, ignore.case = TRUE))

  r_code <- compile_r_script(sp)
  expect_true(verify_compiled_r_ast(r_code, strict = TRUE)$safe)
  qmd <- compile_quarto_report(sp)
  expect_true(grepl("Monotone dropout", qmd, fixed = TRUE))

  # Controls synthesis & patching
  desc <- spec_to_controls(sp)
  expect_true("time_points" %in% desc$control_ids)
  expect_true("ar1_rho" %in% desc$control_ids)
  expect_false("cluster_size" %in% desc$control_ids)
  expect_false("cluster_size_dist" %in% desc$control_ids)
  expect_true("ar1_rho" %in% desc$available_grid_axes)
  expect_false("cluster_size" %in% desc$available_grid_axes)

  p_tp <- patch_spec_from_control(sp, "time_points", 6L)
  expect_true(p_tp$validation$valid)
  expect_equal(p_tp$spec$hierarchy$time_points, 6L)
  expect_equal(p_tp$spec$hierarchy$cluster_size, 6L)
  expect_equal(p_tp$spec$hierarchy$time_spacing, c(0, 1, 2, 3, 4, 5))
  expect_true("hierarchy.time_points" %in% p_tp$diff$changed_paths)

  p_rho <- patch_spec_from_control(sp, "ar1_rho", -0.35)
  expect_true(p_rho$validation$valid)
  expect_equal(p_rho$spec$outcome$ar1_rho, -0.35)
  expect_true("outcome.ar1_rho" %in% p_rho$diff$changed_paths)

  # Switching outcome family clears ar1_rho and restores on switch back
  p_bin <- patch_spec_from_control(sp, "outcome_family", "binomial")
  expect_true(p_bin$validation$valid)
  expect_null(p_bin$spec$outcome$ar1_rho)
  p_back <- patch_spec_from_control(p_bin$spec, "outcome_family", "gaussian")
  expect_true(p_back$validation$valid)
  expect_equal(p_back$spec$outcome$ar1_rho, 0.30)

  # Guardrails: invalid AR(1) rho, time_points < 3, non-increasing time_spacing, wrong length
  bad_rho <- sp; bad_rho$outcome$ar1_rho <- 0.96
  expect_false(validate_spec_ir(bad_rho)$valid)
  bad_rho_neg <- sp; bad_rho_neg$outcome$ar1_rho <- -0.95
  expect_false(validate_spec_ir(bad_rho_neg)$valid)

  bad_tp <- sp; bad_tp$hierarchy$time_points <- 2L; bad_tp$hierarchy$cluster_size <- 2L; bad_tp$hierarchy$time_spacing <- c(0, 1)
  expect_false(validate_spec_ir(bad_tp)$valid)

  bad_spacing <- sp; bad_spacing$hierarchy$time_spacing <- c(0, 2, 2, 4, 5)
  expect_false(validate_spec_ir(bad_spacing)$valid)

  bad_spacing_len <- sp; bad_spacing_len$hierarchy$time_spacing <- c(0, 1, 2)
  expect_false(validate_spec_ir(bad_spacing_len)$valid)

  bad_mismatch <- sp; bad_mismatch$hierarchy$cluster_size <- 4L
  expect_false(validate_spec_ir(bad_mismatch)$valid)

  bad_dist <- sp; bad_dist$hierarchy$cluster_size_dist <- "poisson"
  expect_false(validate_spec_ir(bad_dist)$valid)

  bad_grid_cs <- sp; bad_grid_cs$simulation$parameter_grid <- list(cluster_size = c(4, 6))
  expect_false(validate_spec_ir(bad_grid_cs)$valid)

  bad_grid_rho <- sp; bad_grid_rho$simulation$parameter_grid <- list(ar1_rho = c(0, 0.99))
  expect_false(validate_spec_ir(bad_grid_rho)$valid)

  # Longitudinal-only features rejected on non-longitudinal designs
  s_nonlong <- new_spec_ir(design_type = "multilevel_sim", hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 10L, icc = 0.2))
  s_bad_ar1 <- s_nonlong; s_bad_ar1$outcome$ar1_rho <- 0.3
  expect_false(validate_spec_ir(s_bad_ar1)$valid)

  s_bad_drop <- s_nonlong; s_bad_drop$missingness <- list(mechanism = "monotone_dropout", rate = 0.2, target = "outcome", auxiliary = NULL, strength = NULL)
  expect_false(validate_spec_ir(s_bad_drop)$valid)

  s_bad_time <- s_nonlong; s_bad_time$predictors[[1L]]$distribution <- "time"
  expect_false(validate_spec_ir(s_bad_time)$valid)

  s_bad_ar1_grid <- s_nonlong; s_bad_ar1_grid$simulation$parameter_grid <- list(ar1_rho = c(0.1, 0.4))
  expect_false(validate_spec_ir(s_bad_ar1_grid)$valid)
})

test_that("NL translator and refiner parse longitudinal growth, AR(1), waves, and monotone dropout", {
  tr <- nl_to_spec(paste(
    "Longitudinal growth curve study with 100 subjects across 6 waves,",
    "ICC 0.4, AR(1) correlation 0.45, growth slope 0.35,",
    "treatment effect 0.2, random slope for time, 20% monotone dropout, 150 replications"
  ))
  expect_true(is_valid_spec_ir(tr$spec))
  expect_equal(tr$spec$design_type, "longitudinal_growth")
  expect_equal(tr$spec$hierarchy$n_clusters, 100L)
  expect_equal(tr$spec$hierarchy$time_points, 6L)
  expect_equal(tr$spec$hierarchy$time_spacing, 0:5)
  expect_equal(tr$spec$outcome$ar1_rho, 0.45)
  expect_equal(tr$spec$effects$fixed$time, 0.35)
  expect_equal(tr$spec$effects$fixed$treatment, 0.2)
  expect_equal(tr$spec$missingness$mechanism, "monotone_dropout")
  expect_equal(tr$spec$missingness$rate, 0.20)

  # Refine waves, AR(1) rho, and dropout rate
  r1 <- refine_spec(tr$spec, "increase to 8 waves and set AR(1) rho to 0.6")
  expect_true(is_valid_spec_ir(r1$spec))
  expect_equal(r1$spec$hierarchy$time_points, 8L)
  expect_equal(r1$spec$hierarchy$cluster_size, 8L)
  expect_equal(r1$spec$hierarchy$time_spacing, 0:7)
  expect_equal(r1$spec$outcome$ar1_rho, 0.6)

  r2 <- refine_spec(r1$spec, "add 30% monotone dropout")
  expect_true(is_valid_spec_ir(r2$spec))
  expect_equal(r2$spec$missingness$mechanism, "monotone_dropout")
  expect_equal(r2$spec$missingness$rate, 0.30)
})

test_that("compiled longitudinal R script empirically recovers AR(1) autocorrelation and growth slopes", {
  skip_if_not_installed("lme4")

  # Part A: Pure AR(1) residual recovery (icc = 0, no random slopes, large J = 400 x 6 waves)
  sp_ar1 <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(levels = 2L, n_clusters = 400L, time_points = 6L, time_spacing = 0:5, icc = 0),
    predictors = list(spec_predictor("time", level = 1L, distribution = "time")),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, ar1_rho = 0.60),
    effects = list(
      intercept = 1.0,
      fixed = list(time = 0.40),
      focal_predictor = "time",
      random = list(intercept = FALSE, slopes = list())
    ),
    simulation = list(n_reps = 2L, seed = 2026L)
  )
  script_ar1 <- compile_r_script(sp_ar1)
  expect_true(verify_compiled_r_ast(script_ar1, strict = TRUE)$safe)

  f_ar1 <- tempfile(fileext = ".R")
  on.exit(unlink(f_ar1), add = TRUE)
  writeLines(script_ar1, f_ar1)
  env_ar1 <- new.env(parent = baseenv())
  capture.output(sys.source(f_ar1, envir = env_ar1))
  set.seed(1234L)
  dat_ar1 <- env_ar1$simulate_one_dataset(env_ar1$cfg)
  expect_equal(nrow(dat_ar1), 2400L)

  # True residual = y - (1.0 + 0.40 * time)
  eps_obs <- dat_ar1$y - (1.0 + 0.40 * dat_ar1$time)
  eps_mat <- matrix(eps_obs, nrow = 400L, ncol = 6L, byrow = TRUE)
  lag1_cor <- stats::cor(as.vector(eps_mat[, 1:5]), as.vector(eps_mat[, 2:6]))
  lag2_cor <- stats::cor(as.vector(eps_mat[, 1:4]), as.vector(eps_mat[, 3:6]))
  expect_lt(abs(lag1_cor - 0.60), 0.05)
  expect_lt(abs(lag2_cor - 0.36), 0.06)
  expect_lt(abs(stats::sd(eps_obs) - 1.0), 0.05)

  # Part B: Full longitudinal random-slope growth model with time:treatment & AR(1) grid sweep
  sp_full <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(levels = 2L, n_clusters = 250L, time_points = 5L, time_spacing = 0:4, icc = 0.35),
    predictors = list(
      spec_predictor("time", level = 1L, distribution = "time"),
      spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5)
    ),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, ar1_rho = 0.40),
    effects = list(
      intercept = 0,
      fixed = list(time = 0.40, treatment = 0.20),
      interactions = list(spec_interaction("time", "treatment", beta = 0.30)),
      focal_predictor = "time:treatment",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "time", sd = 0.20)),
        corr_matrix = matrix(c(1, 0.1, 0.1, 1), nrow = 2L)
      )
    ),
    simulation = list(n_reps = 2L, seed = 2026L, parameter_grid = list(ar1_rho = c(0, 0.5)))
  )
  script_full <- compile_r_script(sp_full)
  expect_true(verify_compiled_r_ast(script_full, strict = TRUE)$safe)

  f_full <- tempfile(fileext = ".R")
  on.exit(unlink(f_full), add = TRUE)
  writeLines(script_full, f_full)
  env_full <- new.env(parent = baseenv())
  capture.output(sys.source(f_full, envir = env_full))
  set.seed(2026L)
  dat_full <- env_full$simulate_one_dataset(env_full$cfg)
  fit_full <- suppressMessages(lme4::lmer(y ~ time + treatment + time:treatment + (1 + time | subject), data = dat_full))
  fe <- lme4::fixef(fit_full)
  expect_lt(abs(unname(fe[["time"]]) - 0.40), 0.08)
  expect_lt(abs(unname(fe[["time:treatment"]]) - 0.30), 0.12)

  # Run full grid sweep through run_simulation()
  grid_out <- env_full$run_simulation(env_full$cfg)
  expect_equal(nrow(grid_out$summary), 2L)
  expect_equal(grid_out$summary$ar1_rho, c(0, 0.5))
  expect_true(all(grid_out$summary$n_converged == 2L))
})

test_that("compiled longitudinal monotone_dropout script enforces strict monotonicity and target attrition", {
  # Unconditional monotone dropout
  sp_drop <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(levels = 2L, n_clusters = 500L, time_points = 5L, time_spacing = 0:4, icc = 0.35),
    predictors = list(
      spec_predictor("time", level = 1L, distribution = "time"),
      spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5)
    ),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, ar1_rho = 0.30),
    effects = list(
      intercept = 0,
      fixed = list(time = 0.30, treatment = 0.25),
      focal_predictor = "time",
      random = list(intercept = TRUE, slopes = list())
    ),
    missingness = list(mechanism = "monotone_dropout", rate = 0.30),
    simulation = list(n_reps = 2L, seed = 2026L)
  )
  script_drop <- compile_r_script(sp_drop)
  expect_true(verify_compiled_r_ast(script_drop, strict = TRUE)$safe)

  f_drop <- tempfile(fileext = ".R")
  on.exit(unlink(f_drop), add = TRUE)
  writeLines(script_drop, f_drop)
  env_drop <- new.env(parent = baseenv())
  capture.output(sys.source(f_drop, envir = env_drop))
  set.seed(999L)
  dat <- env_drop$simulate_one_dataset(env_drop$cfg)
  miss_mat <- matrix(is.na(dat$y), nrow = 500L, ncol = 5L, byrow = TRUE)

  # Wave 1 completely observed; strictly monotone for every subject; final-wave dropout ~ 0.30
  expect_equal(sum(miss_mat[, 1L]), 0L)
  monotone_ok <- apply(miss_mat, 1L, function(row) all(diff(as.integer(row)) >= 0L))
  expect_true(all(monotone_ok))
  expect_lt(abs(mean(miss_mat[, 5L]) - 0.30), 0.05)

  # Auxiliary-driven MAR monotone dropout
  sp_mar_drop <- sp_drop
  sp_mar_drop$missingness$auxiliary <- "treatment"
  sp_mar_drop$missingness$strength <- 1.5
  expect_true(is_valid_spec_ir(sp_mar_drop))
  script_md <- compile_r_script(sp_mar_drop)
  expect_true(verify_compiled_r_ast(script_md, strict = TRUE)$safe)

  f_md <- tempfile(fileext = ".R")
  on.exit(unlink(f_md), add = TRUE)
  writeLines(script_md, f_md)
  env_md <- new.env(parent = baseenv())
  capture.output(sys.source(f_md, envir = env_md))
  set.seed(1001L)
  dat_md <- env_md$simulate_one_dataset(env_md$cfg)
  miss_md <- matrix(is.na(dat_md$y), nrow = 500L, ncol = 5L, byrow = TRUE)
  trt_sub <- dat_md$treatment[seq.int(1L, nrow(dat_md), by = 5L)]
  expect_equal(sum(miss_md[, 1L]), 0L)
  expect_true(all(apply(miss_md, 1L, function(row) all(diff(as.integer(row)) >= 0L))))
  expect_gt(mean(miss_md[trt_sub == 1L, 5L]), mean(miss_md[trt_sub == 0L, 5L]) + 0.10)
})

test_that("compare_specs computes exact DEFF, N_eff, variance decomposition, diffs, and optional power", {
  s_ols <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 400L)
  )
  s_bal <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 10L, icc = 0.2)
  )
  s_norm <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(
      levels = 2L, n_clusters = 40L, cluster_size = 10L,
      cluster_size_dist = "normal", cluster_size_sd = 4, icc = 0.2
    )
  )
  s_pois <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(
      levels = 2L, n_clusters = 40L, cluster_size = 10L,
      cluster_size_dist = "poisson", icc = 0.2
    )
  )
  s_long <- use_spec_template("longitudinal_growth_ar1", n_clusters = 80L, ar1_rho = 0.5)

  cmp <- compare_specs(ols = s_ols, balanced = s_bal, normal_unbal = s_norm, pois_unbal = s_pois, growth = s_long)
  expect_s3_class(cmp, "spec_comparison")
  df <- as.data.frame(cmp)
  expect_equal(nrow(df), 5L)
  expect_equal(df$scenario, c("ols", "balanced", "normal_unbal", "pois_unbal", "growth"))

  # Verify exact Kish / Eldridge-Ashby-Kerry formulas
  expect_equal(df$deff[[1L]], 1.0)
  expect_equal(df$n_eff[[1L]], 400.0)
  expect_equal(df$deff[[2L]], 1 + (10 - 1) * 0.2)
  expect_equal(df$n_eff[[2L]], round(400 / 2.8, 2))
  expect_equal(df$deff[[3L]], round(1 + (10 * (1 + 0.4^2) - 1) * 0.2, 4))
  expect_equal(df$deff[[4L]], round(1 + (10 * (1 + 1 / 10) - 1) * 0.2, 4))
  expect_equal(df$deff[[5L]], round(1 + (5 - 1) * 0.4, 4))
  expect_equal(df$ar1_rho[[5L]], 0.5)
  expect_true(is.na(df$ar1_rho[[1L]]))
  expect_equal(df$prop_level2[[2L]], 0.2)

  # Pairwise diffs and deltas
  expect_equal(names(cmp$diffs), c("balanced", "normal_unbal", "pois_unbal", "growth"))
  expect_true(all(vapply(cmp$diffs, inherits, logical(1), "spec_diff")))
  expect_equal(cmp$deltas$n_changed_paths[[1L]], 0L)
  expect_true(all(cmp$deltas$n_changed_paths[-1L] > 0L))

  # Print method runs cleanly and as.data.frame respects row.names
  expect_no_error(suppressMessages(capture.output(print(cmp))))
  df_rn <- as.data.frame(cmp, row.names = paste0("r", 1:5))
  expect_equal(rownames(df_rn), paste0("r", 1:5))

  # List input & unnamed auto-naming
  cmp_list <- compare_specs(list(s_ols, s_bal))
  expect_equal(cmp_list$summary$scenario, c("spec_1", "spec_2"))

  # Comparative preview power
  cmp_pwr <- compare_specs(ols = s_ols, growth = s_long, run_power = TRUE, preview_reps = 4L, seed = 77L)
  expect_true(cmp_pwr$run_power)
  expect_true(all(c("est_power", "est_bias", "est_mean_se", "est_coverage") %in% names(cmp_pwr$summary)))
  expect_true("delta_power" %in% names(cmp_pwr$deltas))
  expect_no_error(suppressMessages(capture.output(print(cmp_pwr))))

  # Input validation errors
  expect_error(compare_specs(s_ols), "at least two")
  expect_error(compare_specs(s_ols, list(a = 1)), "must be a")
  expect_error(compare_specs(s_ols, s_bal, run_power = "yes"), "TRUE or FALSE")
  expect_error(compare_specs(s_ols, s_bal, run_power = TRUE, preview_reps = 0L), "between 1 and 500")
  expect_error(compare_specs(s_ols, s_bal, run_power = TRUE, max_preview_n = 5L), ">= 20")
  expect_error(compare_specs(s_ols, s_bal, run_power = TRUE, max_preview_seconds = -1), "positive")
  expect_error(compare_specs(s_ols, s_bal, run_power = TRUE, seed = "abc"), "integer")
})

test_that("domain template registry supports built-ins, overrides, NL refinement, and safe custom registration", {
  tbl <- list_spec_templates()
  expect_true(is.data.frame(tbl))
  expected_builtins <- c("cluster_rct", "cross_level_mlm", "longitudinal_growth_ar1", "logistic_power", "factorial_ols")
  expect_true(all(expected_builtins %in% tbl$name))

  # Every built-in template validates AND passes strict AST check
  for (nm in expected_builtins) {
    sp <- use_spec_template(nm)
    expect_true(is_valid_spec_ir(sp))
    expect_true(verify_compiled_r_ast(compile_r_script(sp), strict = TRUE)$safe)
  }

  # Category filtering
  long_tbl <- list_spec_templates(category = "longitudinal")
  expect_equal(long_tbl$name, "longitudinal_growth_ar1")
  expect_error(list_spec_templates(category = 123), "character scalar")

  # Overrides via control IDs, dotted paths, shortcuts, and NL refinement
  s_ov <- use_spec_template(
    "longitudinal_growth_ar1",
    overrides = list(
      "hierarchy.n_clusters" = 70L,
      "outcome.ar1_rho" = 0.55,
      title = "Custom Growth Study",
      description = "Updated description",
      parameter_grid = list(ar1_rho = c(0.1, 0.5))
    ),
    time_spacing = c(0, 1, 3, 6),
    refine = "set replications to 100"
  )
  expect_true(is_valid_spec_ir(s_ov))
  expect_equal(s_ov$hierarchy$n_clusters, 70L)
  expect_equal(s_ov$hierarchy$time_points, 4L)
  expect_equal(s_ov$hierarchy$time_spacing, c(0, 1, 3, 6))
  expect_equal(s_ov$outcome$ar1_rho, 0.55)
  expect_equal(s_ov$metadata$title, "Custom Growth Study")
  expect_equal(s_ov$simulation$n_reps, 100L)

  # Dotted fixed-effect override & hierarchy.time_points override
  s_ov2 <- use_spec_template("cluster_rct", overrides = list("effects.fixed.treatment" = 0.5, "simulation.seed" = 99L))
  expect_equal(s_ov2$effects$fixed$treatment, 0.5)
  expect_equal(s_ov2$simulation$seed, 99L)

  # Register / list / use / overwrite / unregister custom template lifecycle
  tpl_name <- "test_custom_growth_v04"
  on.exit({
    if (tpl_name %in% list_spec_templates()$name) unregister_spec_template(tpl_name)
  }, add = TRUE)

  rec <- register_spec_template(tpl_name, s_ov, description = "My custom growth study", category = "clinical")
  expect_equal(rec$name, tpl_name)
  expect_true(tpl_name %in% list_spec_templates(category = "clinical")$name)

  s_from_custom <- use_spec_template(tpl_name, n_clusters = 95L)
  expect_equal(s_from_custom$hierarchy$n_clusters, 95L)

  # Duplicate registration blocked unless overwrite = TRUE
  expect_error(register_spec_template(tpl_name, s_ov), "already exists")
  expect_no_error(register_spec_template(tpl_name, s_ov, description = "Updated", overwrite = TRUE))

  # Built-ins cannot be overwritten or unregistered
  expect_error(register_spec_template("cluster_rct", s_ov, overwrite = TRUE), "built-in")
  expect_error(unregister_spec_template("cluster_rct"), "built-in")

  # Invalid template names, invalid specs, unknown templates, bad overrides rejected
  expect_error(register_spec_template("123_bad", s_ov), "valid ASCII identifier")
  expect_error(register_spec_template("ok_name", list(a = 1)), "must be a")
  bad_sp <- s_ov; bad_sp$outcome$ar1_rho <- 0.99
  expect_error(register_spec_template("bad_spec_tpl", bad_sp))
  expect_error(use_spec_template("does_not_exist"), "Unknown template")
  expect_error(use_spec_template("cluster_rct", nonexistent_field = 1), "Unrecognized template override")
  expect_error(use_spec_template("cluster_rct", 42), "named list")
  expect_error(use_spec_template("cluster_rct", overrides = list(), refine = 42), "character scalar")
  expect_error(use_spec_template("cluster_rct", overrides = list(), refine = NULL, 42), "must be named")

  expect_true(unregister_spec_template(tpl_name))
  expect_false(tpl_name %in% list_spec_templates()$name)
  expect_error(unregister_spec_template(tpl_name), "not registered")
})

test_that("v0.4.0 latent bug regressions stay fixed", {
  # Bug 1: Adding interaction/slope with new predictor resizes predictor_corr cleanly
  s_corr <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2),
    predictors = list(
      spec_predictor("x1", level = 1L),
      spec_predictor("x2", level = 1L)
    ),
    predictor_corr = matrix(c(1, 0.3, 0.3, 1), nrow = 2L),
    effects = list(intercept = 0, fixed = list(x1 = 0.3, x2 = 0.2), focal_predictor = "x1", random = list(intercept = TRUE, slopes = list()))
  )
  ref_int <- refine_spec(s_corr, "add interaction between x1 and z1 of 0.25")
  expect_true(is_valid_spec_ir(ref_int$spec))
  expect_equal(dim(ref_int$spec$predictor_corr), c(3L, 3L))
  expect_equal(ref_int$spec$predictor_corr[1L, 2L], 0.3)

  ref_sl <- refine_spec(s_corr, "add random slope for x3")
  expect_true(is_valid_spec_ir(ref_sl$spec))
  expect_equal(dim(ref_sl$spec$predictor_corr), c(3L, 3L))

  # Bug 2: Preview simulator scales large unbalanced normal clusters & large longitudinal specs safely
  s_big_unbal <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 150L, cluster_size = 40L, cluster_size_dist = "normal", cluster_size_sd = 12, icc = 0.2),
    simulation = list(n_reps = 100L, seed = 42L)
  )
  prev_unbal <- run_spec_preview_sim(s_big_unbal, max_reps = 2L, max_n = 500L)
  expect_equal(prev_unbal$summary$n_reps, 2L)
  expect_true(any(grepl("scaled down", prev_unbal$warnings)))

  s_big_long <- use_spec_template("longitudinal_growth_ar1", n_clusters = 600L)
  prev_long <- run_spec_preview_sim(s_big_long, max_reps = 2L, max_n = 400L)
  expect_equal(prev_long$summary$n_reps, 2L)
  expect_true(any(grepl("scaled down", prev_long$warnings)))

  # Bug 3: patch_spec_from_control safely collapses vector grid input
  p_vec <- patch_spec_from_control(s_corr, "grid_n_clusters", c("30", "60"))
  expect_true(p_vec$validation$valid)
  expect_equal(p_vec$spec$simulation$parameter_grid$n_clusters, c(30, 60))
})
