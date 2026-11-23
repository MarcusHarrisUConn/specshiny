test_that("spec_to_controls synthesizes ONLY controls relevant to each design", {
  # 1. Minimal 1-level Gaussian complete-data spec
  s1 <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 150L),
    predictors = list(spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "focal"))
  )
  d1 <- spec_to_controls(s1)
  expect_s3_class(d1, "spec_ui_descriptor")
  expect_true("n_obs" %in% d1$control_ids)
  expect_true("residual_sd" %in% d1$control_ids)
  expect_true("pred_sd_x1" %in% d1$control_ids)

  # Strictly absent controls for 1-level Gaussian complete spec:
  absent_1 <- c(
    "n_clusters", "cluster_size", "icc", "outcome_link",
    "pred_prob_x1", "pred_min_x1", "pred_max_x1",
    "focal_predictor", "slope_sd_x1", "corr_1_2",
    "missing_rate", "missing_auxiliary", "missing_strength",
    "estimator_reml"
  )
  expect_false(any(absent_1 %in% d1$control_ids))

  # 2. Complex 2-level MLM with random slope + MAR + binomial
  s2 <- new_spec_ir(
    design_type = "power_analysis",
    hierarchy = list(levels = 2L, n_clusters = 50L, cluster_size = 20L, cluster_name = "school", unit_name = "student", icc = 0.2),
    predictors = list(
      spec_predictor("trt", level = 1L, distribution = "binary", prob = 0.5, role = "treatment"),
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1),
      spec_predictor("u1", level = 1L, distribution = "uniform", min = 0, max = 5)
    ),
    outcome = list(name = "y", family = "binomial", link = "logit"),
    effects = list(
      intercept = 0,
      fixed = list(trt = 0.5, x1 = 0.2, u1 = 0.1),
      focal_predictor = "trt",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "x1", sd = 0.25)),
        corr_matrix = matrix(c(1, 0.2, 0.2, 1), 2, 2)
      )
    ),
    missingness = list(mechanism = "MAR", rate = 0.15, target = "outcome", auxiliary = "x1", strength = 1.0),
    simulation = list(n_reps = 100L, seed = 2026L, parameter_grid = list(n_clusters = c(30L, 50L, 70L)))
  )
  d2 <- spec_to_controls(s2)
  expect_true(all(c(
    "n_clusters", "cluster_size", "icc",
    "outcome_family", "outcome_link",
    "pred_prob_trt", "pred_sd_x1", "pred_min_u1", "pred_max_u1",
    "focal_predictor", "slope_sd_x1", "corr_1_2",
    "missing_mechanism", "missing_rate", "missing_auxiliary", "missing_strength",
    "grid_n_clusters"
  ) %in% d2$control_ids))
  # Strictly omitted for 2-level binomial glmer:
  expect_false("n_obs" %in% d2$control_ids)
  expect_false("residual_sd" %in% d2$control_ids)
  expect_false("estimator_reml" %in% d2$control_ids)

  # Render HTML cards and diff badges cleanly
  html_cards <- render_spec_controls_ui(d2, ns = shiny::NS("ctrl"), diff = diff_spec_ir(s1, s2))
  expect_true(inherits(html_cards, "shiny.tag.list"))
  expect_match(as.character(html_cards), "badge", fixed = TRUE)

  diff_html <- render_diff_badges_ui(diff_spec_ir(s1, s2))
  expect_match(as.character(diff_html), "Latest Sync Diff", fixed = TRUE)
  expect_match(as.character(render_diff_badges_ui(NULL)), "Synced", fixed = TRUE)
})

test_that("patch_spec_from_control validates updates and blocks guardrail violations", {
  s <- nl_to_spec("2-level study with 40 schools, 20 students per school, ICC 0.2, random slope for x1")$spec

  # Valid edit
  ok <- patch_spec_from_control(s, "n_clusters", 75)
  expect_true(ok$changed)
  expect_true(ok$validation$valid)
  expect_equal(ok$spec$hierarchy$n_clusters, 75L)

  # Idempotent no-op edit
  noop <- patch_spec_from_control(s, "n_clusters", 40)
  expect_false(noop$changed)

  # Invalid edit blocked: cluster_size = 2 with 2 random effects (unidentifiable)
  bad_cs <- patch_spec_from_control(s, "cluster_size", 2)
  expect_false(bad_cs$validation$valid)
  expect_true("insufficient_cluster_size_for_random_effects" %in% bad_cs$validation$errors$code)

  # Invalid edit blocked: correlation = 1.0 (non-PD)
  bad_cor <- patch_spec_from_control(s, "corr_1_2", 1.0)
  expect_false(bad_cor$validation$valid)
  expect_true("corr_matrix_not_positive_definite" %in% bad_cor$validation$errors$code)

  # Outcome family switch automatically adapts link & estimator
  sw <- patch_spec_from_control(s, "outcome_family", "binomial")
  expect_true(sw$validation$valid)
  expect_equal(sw$spec$outcome$link, "logit")
  expect_null(sw$spec$outcome$residual_sd)
  expect_equal(sw$spec$estimator$engine, "glmer")
})
