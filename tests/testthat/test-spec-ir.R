test_that("new_spec_ir creates valid 1-level and 2-level specs with defaults rationale", {
  s1 <- new_spec_ir()
  expect_true(is_spec_ir(s1))
  expect_true(is_valid_spec_ir(s1))
  expect_equal(s1$schema_version, "1.1.0")
  expect_equal(s1$hierarchy$levels, 1L)
  expect_equal(spec_total_n(s1), 200L)
  expect_equal(spec_implied_intercept_sd(s1), 0)
  expect_true(length(s1$defaults_rationale) >= 3L)

  s2 <- new_spec_ir(
    design_type = "multilevel_sim",
    title = "Clinic Outcomes",
    hierarchy = list(levels = 2L, n_clusters = 50L, cluster_size = 15L, cluster_name = "clinic", unit_name = "patient", icc = 0.2),
    predictors = list(
      spec_predictor("trt", level = 2L, distribution = "binary", prob = 0.5, role = "treatment"),
      spec_predictor("age", level = 1L, distribution = "normal", mean = 0, sd = 1, centering = "group_mean")
    ),
    effects = list(
      intercept = 0.5,
      fixed = list(trt = 0.4, age = 0.15),
      focal_predictor = "trt",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "age", sd = 0.2)),
        corr_matrix = matrix(c(1, 0.25, 0.25, 1), 2, 2)
      )
    )
  )

  expect_true(is_valid_spec_ir(s2))
  expect_equal(spec_total_n(s2), 750L)
  expect_equal(spec_implied_intercept_sd(s2), sqrt(1 * 0.2 / 0.8), tolerance = 1e-8)
  expect_equal(s2$estimator$engine, "lmer")
  expect_true(all(c("monte_carlo_lm", "power_analysis", "multilevel_sim", "two_level_mlm") %in% spec_design_types()))
  expect_true(nzchar(spec_schema_path()) && file.exists(spec_schema_path()))

  # print and summary work cleanly without errors
  expect_no_error(suppressMessages(print(s2)))
  sm <- summary(s2)
  expect_s3_class(sm, "summary.spec_ir")
  expect_equal(sm$total_n, 750L)
  expect_equal(sm$n_random_slopes, 1L)
  expect_no_error(suppressMessages(print(sm)))
})

test_that("JSON and YAML serialization round-trips losslessly including matrices and grids", {
  spec <- new_spec_ir(
    design_type = "power_analysis",
    title = "Roundtrip MLM",
    hierarchy = list(levels = 2L, n_clusters = 45L, cluster_size = 20L, cluster_name = "school", unit_name = "student", icc = 0.2),
    predictors = list(
      spec_predictor("trt", level = 1L, distribution = "binary", prob = 0.5, role = "treatment"),
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1.5, centering = "group_mean"),
      spec_predictor("u1", level = 1L, distribution = "uniform", min = -1, max = 2)
    ),
    effects = list(
      intercept = 1.0,
      fixed = list(trt = 0.35, x1 = 0.2, u1 = -0.1),
      focal_predictor = "trt",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "x1", sd = 0.25)),
        corr_matrix = matrix(c(1, -0.3, -0.3, 1), 2, 2)
      )
    ),
    missingness = list(mechanism = "MAR", rate = 0.15, target = "outcome", auxiliary = "x1", strength = 1.2),
    simulation = list(
      n_reps = 100L,
      seed = 99L,
      alpha = 0.05,
      parameter_grid = list(n_clusters = c(30L, 45L, 60L), icc = c(0.1, 0.2))
    )
  )

  # YAML round-trip via text and disk
  yml_file <- tempfile(fileext = ".yaml")
  on.exit(unlink(yml_file), add = TRUE)
  write_spec_ir(spec, yml_file)
  from_yml <- read_spec_ir(yml_file)
  expect_true(is_valid_spec_ir(from_yml))
  expect_false(diff_spec_ir(spec, from_yml)$has_changes)
  expect_true(is.matrix(from_yml$effects$random$corr_matrix))
  expect_equal(from_yml$effects$random$corr_matrix, spec$effects$random$corr_matrix)
  expect_equal(from_yml$simulation$parameter_grid$n_clusters, c(30, 45, 60))

  # JSON round-trip via text and disk
  json_file <- tempfile(fileext = ".json")
  on.exit(unlink(json_file), add = TRUE)
  write_spec_ir(spec, json_file)
  from_json <- read_spec_ir(json_file)
  expect_true(is_valid_spec_ir(from_json))
  expect_false(diff_spec_ir(spec, from_json)$has_changes)
  expect_true(is.matrix(from_json$effects$random$corr_matrix))
  expect_equal(from_json$effects$random$corr_matrix, spec$effects$random$corr_matrix)
  expect_equal(from_json$simulation$parameter_grid$icc, c(0.1, 0.2))

  # Unknown extension errors clearly
  expect_error(write_spec_ir(spec, tempfile(fileext = ".txt")), "Cannot infer format")
})
