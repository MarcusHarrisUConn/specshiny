make_base_mlm <- function() {
  new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, cluster_name = "school", unit_name = "student", icc = 0.2),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "focal"),
      spec_predictor("z2", level = 2L, distribution = "binary", prob = 0.5, role = "covariate")
    ),
    effects = list(
      intercept = 0,
      fixed = list(x1 = 0.3, z2 = 0.2),
      focal_predictor = "x1",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "x1", sd = 0.2)),
        corr_matrix = matrix(c(1, 0.2, 0.2, 1), 2, 2)
      )
    )
  )
}

test_that("schema validation catches missing fields, bad schema version, and unknown top-level keys", {
  s <- new_spec_ir()
  s_missing <- s
  s_missing$outcome <- NULL
  v1 <- validate_spec_ir(s_missing)
  expect_false(v1$valid)
  expect_true("missing_required_field" %in% v1$errors$code)

  s_extra <- s
  s_extra$rogue_code <- "system('rm -rf /')"
  v2 <- validate_spec_ir(s_extra)
  expect_false(v2$valid)
  expect_true("unknown_top_level_field" %in% v2$errors$code)

  s_ver <- s
  s_ver$schema_version <- "9.9.9"
  expect_false(validate_spec_ir(s_ver)$valid)

  expect_error(validate_spec_ir(s_ver, strict = TRUE), class = "specshiny_validation_error")
})

test_that("identifier guardrails block injection strings, reserved words, and collisions", {
  s <- new_spec_ir()
  s_inj <- s
  s_inj$outcome$name <- "y; system('echo pwned')"
  v <- validate_spec_ir(s_inj)
  expect_false(v$valid)
  expect_true("invalid_identifier" %in% v$errors$code)

  s_res <- s
  s_res$predictors[[1L]]$name <- "function"
  s_res$effects$fixed <- list(`function` = 0.3)
  s_res$effects$focal_predictor <- "function"
  expect_true("invalid_identifier" %in% validate_spec_ir(s_res)$errors$code)

  s_col <- s
  s_col$outcome$name <- "x1"
  expect_true("name_collision" %in% validate_spec_ir(s_col)$errors$code)
})

test_that("statistical guardrails reject non-positive variances, invalid ICC, and bad sample sizes", {
  s1 <- new_spec_ir()
  s1_bad_sd <- s1; s1_bad_sd$outcome$residual_sd <- -0.5
  expect_true("non_positive_residual_sd" %in% validate_spec_ir(s1_bad_sd)$errors$code)

  s1_bad_n <- s1; s1_bad_n$hierarchy$n_obs <- 3L
  expect_true("invalid_sample_size" %in% validate_spec_ir(s1_bad_n)$errors$code)

  s1_nonint <- s1; s1_nonint$hierarchy$n_obs <- 25.5
  expect_true("invalid_sample_size" %in% validate_spec_ir(s1_nonint)$errors$code)

  s2 <- make_base_mlm()
  s2_icc1 <- s2; s2_icc1$hierarchy$icc <- 1.0
  expect_true("invalid_icc" %in% validate_spec_ir(s2_icc1)$errors$code)

  s2_icc_neg <- s2; s2_icc_neg$hierarchy$icc <- -0.1
  expect_true("invalid_icc" %in% validate_spec_ir(s2_icc_neg)$errors$code)

  s2_noint <- s2; s2_noint$effects$random$intercept <- FALSE
  expect_true("icc_requires_random_intercept" %in% validate_spec_ir(s2_noint)$errors$code)

  s2_mismatch <- s2; s2_mismatch$effects$random$intercept_sd <- 5.0
  expect_true("intercept_sd_icc_mismatch" %in% validate_spec_ir(s2_mismatch)$errors$code)
})

test_that("random-effect correlation matrix guardrails catch non-PD, asymmetric, and bad-diagonal matrices", {
  s2 <- make_base_mlm()

  # Non-PD 2x2 (rho = 1 -> singular, min eig = 0)
  s_sing <- s2
  s_sing$effects$random$corr_matrix <- matrix(c(1, 1, 1, 1), 2, 2)
  v_sing <- validate_spec_ir(s_sing)
  expect_false(v_sing$valid)
  expect_true("corr_matrix_not_positive_definite" %in% v_sing$errors$code)

  # Asymmetric matrix
  s_asym <- s2
  s_asym$effects$random$corr_matrix <- matrix(c(1, 0.3, 0.1, 1), 2, 2)
  expect_true("corr_matrix_not_symmetric" %in% validate_spec_ir(s_asym)$errors$code)

  # Non-unit diagonal
  s_diag <- s2
  s_diag$effects$random$corr_matrix <- matrix(c(2, 0.2, 0.2, 1), 2, 2)
  expect_true("corr_matrix_unit_diagonal" %in% validate_spec_ir(s_diag)$errors$code)

  # 3x3 valid pairwise entries in (-1, 1) that violate positive definiteness
  s3 <- make_base_mlm()
  s3$predictors[[3L]] <- spec_predictor("x2", level = 1L, distribution = "normal")
  s3$effects$fixed$x2 <- 0.15
  s3$effects$random$slopes <- list(
    list(predictor = "x1", sd = 0.2),
    list(predictor = "x2", sd = 0.2)
  )
  # Classic non-PD correlation matrix: corr(1,2)=0.9, corr(1,3)=0.9, corr(2,3)=-0.9
  non_pd_3 <- matrix(c(1, 0.9, 0.9, 0.9, 1, -0.9, 0.9, -0.9, 1), 3, 3)
  s3$effects$random$corr_matrix <- non_pd_3
  v3 <- validate_spec_ir(s3)
  expect_false(v3$valid)
  expect_true("corr_matrix_not_positive_definite" %in% v3$errors$code)
})

test_that("identifiability guardrails reject level-2 random slopes, tiny clusters, and bad centering", {
  s2 <- make_base_mlm()

  # Random slope on level-2 predictor z2 is unidentifiable
  s_l2_slope <- s2
  s_l2_slope$effects$random$slopes <- list(list(predictor = "z2", sd = 0.2))
  v_l2 <- validate_spec_ir(s_l2_slope)
  expect_false(v_l2$valid)
  expect_true("random_slope_level2_unidentifiable" %in% v_l2$errors$code)

  # Cluster size <= number of random effects (q = 2 -> cluster_size = 2 is unidentifiable)
  s_tiny <- s2
  s_tiny$hierarchy$cluster_size <- 2L
  v_tiny <- validate_spec_ir(s_tiny)
  expect_false(v_tiny$valid)
  expect_true("insufficient_cluster_size_for_random_effects" %in% v_tiny$errors$code)

  # Group-mean centering on level-2 predictor is invalid
  s_gmc <- s2
  s_gmc$predictors[[2L]]$centering <- "group_mean"
  expect_true("invalid_group_mean_centering" %in% validate_spec_ir(s_gmc)$errors$code)
})

test_that("missingness, estimator compatibility, and parameter grid guardrails work", {
  s2 <- make_base_mlm()

  # Missingness bounds
  s_m1 <- s2; s_m1$missingness$rate <- 0.2 # mechanism is still "none"
  expect_true("nonzero_rate_with_no_mechanism" %in% validate_spec_ir(s_m1)$errors$code)

  s_m2 <- s2; s_m2$missingness$mechanism <- "MCAR"; s_m2$missingness$rate <- 0.96
  expect_true("invalid_missing_rate" %in% validate_spec_ir(s_m2)$errors$code)

  s_m3 <- s2; s_m3$missingness$mechanism <- "MAR"; s_m3$missingness$rate <- 0.2; s_m3$missingness$auxiliary <- "nonexistent"; s_m3$missingness$strength <- 1.0
  expect_true("invalid_mar_auxiliary" %in% validate_spec_ir(s_m3)$errors$code)

  # Estimator mismatch: lm on 2-level with random effects
  s_est <- s2; s_est$estimator$engine <- "lm"
  expect_true("estimator_incompatible" %in% validate_spec_ir(s_est)$errors$code)

  # Parameter grid: inapplicable axis in 1-level design
  s1 <- new_spec_ir()
  s1_g <- s1; s1_g$simulation$parameter_grid <- list(icc = c(0.1, 0.2))
  expect_true("grid_param_inapplicable" %in% validate_spec_ir(s1_g)$errors$code)

  # Parameter grid combinatorial explosion (> 250 cells)
  s2_big <- s2
  s2_big$simulation$parameter_grid <- list(
    n_clusters = seq(20L, 110L, by = 10L), # 10
    cluster_size = seq(5L, 32L, by = 3L),  # 10
    icc = c(0.05, 0.15, 0.25)              # 3 -> 300 cells > 250
  )
  expect_true("grid_combinatorial_explosion" %in% validate_spec_ir(s2_big)$errors$code)
})
