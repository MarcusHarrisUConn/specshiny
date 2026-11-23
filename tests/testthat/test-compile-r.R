run_compiled_script_in_fresh_env <- function(spec) {
  script_path <- tempfile(fileext = ".R")
  on.exit(unlink(script_path), add = TRUE)
  compile_r_script(spec, file = script_path, run_on_source = TRUE)

  # Must parse cleanly as valid R syntax
  exprs <- parse(file = script_path)
  expect_true(length(exprs) >= 5L)

  # Execute standalone script in an isolated environment inheriting globalenv
  env <- new.env(parent = globalenv())
  capture.output(sys.source(script_path, envir = env))
  env
}

test_that("compile_r_script is byte-for-byte deterministic and blocks invalid specs", {
  spec <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 80L),
    simulation = list(n_reps = 5L, seed = 42L)
  )
  code1 <- compile_r_script(spec)
  code2 <- compile_r_script(spec)
  expect_identical(code1, code2)

  bad <- spec
  bad$outcome$residual_sd <- -1
  expect_error(compile_r_script(bad), class = "specshiny_validation_error")
})

test_that("compile_r_script sanitizes untrusted title/description strings against code injection", {
  spec <- new_spec_ir(
    design_type = "monte_carlo_lm",
    title = "Study\nstop('INJECTED_CODE_EXECUTED')\n\" ; stop('INJECTED_2')",
    description = "Desc\nsystem('echo bad')",
    hierarchy = list(levels = 1L, n_obs = 60L),
    simulation = list(n_reps = 3L, seed = 11L)
  )
  env <- run_compiled_script_in_fresh_env(spec)
  expect_true(is.list(env$results))
  expect_equal(nrow(env$results$summary), 1L)
  expect_equal(env$results$summary$n_converged, 3L)
})

test_that("compiled standalone R scripts actually execute cleanly for 1-level Gaussian, Binomial MCAR, and Poisson", {
  # 1. Single-level Gaussian lm
  s_lm <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 100L),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, centering = "grand_mean", role = "focal"),
      spec_predictor("u1", level = 1L, distribution = "uniform", min = -1, max = 1)
    ),
    effects = list(intercept = 0.5, fixed = list(x1 = 0.4, u1 = -0.2), focal_predictor = "x1"),
    simulation = list(n_reps = 6L, seed = 101L)
  )
  env_lm <- run_compiled_script_in_fresh_env(s_lm)
  expect_equal(env_lm$results$summary$n_reps, 6L)
  expect_equal(env_lm$results$summary$n_converged, 6L)
  expect_true(is.finite(env_lm$results$summary$mean_est))
  expect_true(env_lm$results$summary$power >= 0 && env_lm$results$summary$power <= 1)

  # 2. Single-level Binomial logistic + MCAR missingness
  s_glm <- new_spec_ir(
    design_type = "power_analysis",
    hierarchy = list(levels = 1L, n_obs = 150L),
    predictors = list(spec_predictor("trt", level = 1L, distribution = "binary", prob = 0.5, role = "treatment")),
    outcome = list(name = "y", family = "binomial", link = "logit"),
    effects = list(intercept = -0.2, fixed = list(trt = 0.7), focal_predictor = "trt"),
    missingness = list(mechanism = "MCAR", rate = 0.15, target = "outcome"),
    simulation = list(n_reps = 5L, seed = 202L)
  )
  env_glm <- run_compiled_script_in_fresh_env(s_glm)
  expect_equal(env_glm$results$summary$n_converged, 5L)
  expect_true(is.finite(env_glm$results$summary$mean_se))

  # 3. Single-level Poisson
  s_pois <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 120L),
    outcome = list(name = "counts", family = "poisson", link = "log"),
    effects = list(intercept = 0.2, fixed = list(x1 = 0.3), focal_predictor = "x1"),
    simulation = list(n_reps = 5L, seed = 303L)
  )
  env_pois <- run_compiled_script_in_fresh_env(s_pois)
  expect_equal(env_pois$results$summary$n_converged, 5L)
  expect_true(is.finite(env_pois$results$summary$mean_est))
})

test_that("compiled standalone R scripts execute cleanly for 2-level lmer with random slopes, MAR, and parameter grid", {
  skip_if_not_installed("lme4")

  # Complex 2-level MLM: random intercept + correlated random slope + group-mean centering + MAR
  s_mlm <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 20L, cluster_size = 15L, cluster_name = "school", unit_name = "student", icc = 0.2),
    predictors = list(
      spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5, role = "treatment"),
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, centering = "group_mean")
    ),
    effects = list(
      intercept = 0,
      fixed = list(treatment = 0.4, x1 = 0.25),
      focal_predictor = "treatment",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "x1", sd = 0.2)),
        corr_matrix = matrix(c(1, 0.3, 0.3, 1), 2, 2)
      )
    ),
    missingness = list(mechanism = "MAR", rate = 0.15, target = "outcome", auxiliary = "x1", strength = 1.0),
    simulation = list(n_reps = 4L, seed = 404L)
  )
  env_mlm <- run_compiled_script_in_fresh_env(s_mlm)
  expect_equal(nrow(env_mlm$results$summary), 1L)
  expect_equal(env_mlm$results$summary$n_converged, 4L)
  expect_true(is.finite(env_mlm$results$summary$mean_est))

  # 2-level Power Study with multi-cell parameter grid (2 x 2 = 4 cells)
  s_grid <- new_spec_ir(
    design_type = "power_analysis",
    hierarchy = list(levels = 2L, n_clusters = 15L, cluster_size = 10L, cluster_name = "school", unit_name = "student", icc = 0.15),
    predictors = list(spec_predictor("treatment", level = 1L, distribution = "binary", prob = 0.5, role = "treatment")),
    effects = list(intercept = 0, fixed = list(treatment = 0.4), focal_predictor = "treatment"),
    simulation = list(
      n_reps = 3L,
      seed = 505L,
      parameter_grid = list(n_clusters = c(12L, 18L), icc = c(0.1, 0.25))
    )
  )
  env_grid <- run_compiled_script_in_fresh_env(s_grid)
  expect_equal(nrow(env_grid$results$summary), 4L)
  expect_true(all(c("n_clusters", "icc", "power", "coverage", "bias") %in% names(env_grid$results$summary)))
  expect_true(all(env_grid$results$summary$n_converged == 3L))
})
