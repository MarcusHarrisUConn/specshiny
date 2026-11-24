test_that("LLM translator backends handle mocked refine_spec and error envelopes cleanly", {
  base_spec <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 150L))
  refined_spec <- refine_spec(base_spec, "set sample size to 300")$spec

  # OpenAI empty API key rejected on both nl_to_spec and refine_spec
  tr_empty <- translator_openai(api_key = "")
  withr::local_envvar(OPENAI_API_KEY = "")
  expect_error(nl_to_spec("sample size 100", translator = tr_empty), "api_key")
  expect_error(refine_spec(base_spec, "set n to 200", translator = tr_empty), "api_key")

  # Constructor validation errors
  expect_error(translator_openai(api_key = "sk-test", base_url = "ftp://bad.example"), "https")
  expect_error(translator_openai(api_key = "sk-test", timeout = -1), "between 1 and 600")
  expect_error(translator_openai(api_key = "sk-test", max_response_bytes = 10L))
  expect_error(translator_ollama(base_url = "ftp://localhost:11434"))

  # Mocked OpenAI refine_spec() returning wrapped spec + clarifications
  payload_wrapped <- paste0(
    '{"spec":', as_spec_json(refined_spec, pretty = FALSE),
    ',"clarifications":[{"id":"c1","path":"simulation.n_reps",',
    '"question":"How many reps?","current_default":200,',
    '"options":[200,500],"rationale":"Defaulted"}]}'
  )
  tr_oai <- translator_openai(api_key = "sk-mock-secret-999")
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(choices = list(list(message = list(content = as.character(payload_wrapped))))),
          auto_unbox = TRUE
        ))
      )
    },
    {
      ref_oai <- refine_spec(base_spec, "set sample size to 300", translator = tr_oai)
      expect_true(is_valid_spec_ir(ref_oai$spec))
      expect_equal(ref_oai$spec$hierarchy$n_obs, 300L)
      expect_equal(length(ref_oai$clarifications), 1L)
    }
  )

  # Mocked Ollama refine_spec() returning bare spec_ir JSON (no wrapper)
  tr_oll <- translator_ollama(model = "llama3.1")
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          list(message = list(content = as_spec_json(refined_spec, pretty = FALSE))),
          auto_unbox = TRUE
        ))
      )
    },
    {
      ref_oll <- refine_spec(base_spec, "set sample size to 300", translator = tr_oll)
      expect_true(is_valid_spec_ir(ref_oll$spec))
      expect_equal(ref_oll$spec$hierarchy$n_obs, 300L)
    }
  )

  # Malformed HTTP JSON envelope (including glue-injection payload!), empty content, non-object JSON, oversized content
  httr2::with_mocked_responses(
    function(req) httr2::response(status_code = 200L, body = charToRaw("{stop('GLUE_INJECTED')}")),
    expect_error(nl_to_spec("sample size 100", translator = tr_oll), class = "specshiny_llm_parse_error")
  )
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        body = charToRaw(jsonlite::toJSON(list(message = list(content = "")), auto_unbox = TRUE))
      )
    },
    expect_error(nl_to_spec("sample size 100", translator = tr_oll), "empty response")
  )
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        body = charToRaw(jsonlite::toJSON(list(message = list(content = '"just_a_string"')), auto_unbox = TRUE))
      )
    },
    expect_error(nl_to_spec("sample size 100", translator = tr_oll), "must be an object")
  )
  expect_error(
    specshiny:::unpack_llm_json_payload(paste(rep("a", 500), collapse = ""), max_bytes = 100L),
    class = "specshiny_security_error"
  )
})

test_that("run_spec_preview_sim and mod_spec_preview_server cover all families, errors, missingness, and caps", {
  skip_if_not_installed("lme4")

  # 1. 2-level lmer with predictor_corr, uniform + binary + normal, group_mean & grand_mean, poisson clusters, t errors, MAR
  s_rich <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 25L, cluster_size = 10L, cluster_size_dist = "poisson", icc = 0.2),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal", centering = "group_mean"),
      spec_predictor("x2", level = 1L, distribution = "uniform", min = 0, max = 1, centering = "grand_mean"),
      spec_predictor("z1", level = 2L, distribution = "binary", prob = 0.5)
    ),
    predictor_corr = matrix(c(1, 0.3, 0, 0.3, 1, 0, 0, 0, 1), nrow = 3L),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, error_dist = "t", error_df = 5),
    effects = list(
      intercept = 0,
      fixed = list(x1 = 0.4, x2 = 0.2, z1 = 0.3),
      interactions = list(spec_interaction("x1", "z1", beta = 0.25)),
      focal_predictor = "x1:z1",
      random = list(intercept = TRUE, slopes = list(list(predictor = "x1", sd = 0.2)), corr_matrix = matrix(c(1, 0.2, 0.2, 1), 2, 2))
    ),
    missingness = list(mechanism = "MAR", rate = 0.15, auxiliary = "x2", strength = 1.0),
    simulation = list(n_reps = 2L, seed = 101L)
  )
  p_rich <- specshiny:::run_spec_preview_sim(s_rich, max_reps = 2L)
  expect_equal(p_rich$summary$n_reps, 2L)

  # 2. Skewed errors + MCAR + 1-level cap downscaling + early time-budget break
  s_skew <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 5000L),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, error_dist = "skewed", error_skew = 1.5),
    missingness = list(mechanism = "MCAR", rate = 0.10),
    simulation = list(n_reps = 50L, seed = 102L, parameter_grid = list(n_obs = c(500, 1000)))
  )
  p_skew <- specshiny:::run_spec_preview_sim(s_skew, max_reps = 20L, max_n = 400L, max_seconds = 1e-9)
  expect_true(any(grepl("scaled down", p_skew$warnings)))
  expect_true(any(grepl("parameter grid", p_skew$warnings)))
  expect_true(any(grepl("stopped early", p_skew$warnings)))

  # 3. 1-level GLM: binomial logit, binomial probit, poisson log
  s_logit <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 120L),
    outcome = list(name = "y", family = "binomial", link = "logit"),
    simulation = list(n_reps = 2L, seed = 103L)
  )
  s_probit <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 120L),
    outcome = list(name = "y", family = "binomial", link = "probit"),
    simulation = list(n_reps = 2L, seed = 104L)
  )
  s_pois <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 120L),
    outcome = list(name = "y", family = "poisson", link = "log"),
    simulation = list(n_reps = 2L, seed = 105L)
  )
  expect_equal(specshiny:::run_spec_preview_sim(s_logit, max_reps = 2L)$summary$n_reps, 2L)
  expect_equal(specshiny:::run_spec_preview_sim(s_probit, max_reps = 2L)$summary$n_reps, 2L)
  expect_equal(specshiny:::run_spec_preview_sim(s_pois, max_reps = 2L)$summary$n_reps, 2L)

  # 4. 2-level glmer binomial + longitudinal MAR monotone dropout preview
  s_glmer <- use_spec_template("logistic_power", n_clusters = 20L, cluster_size = 10L)
  expect_equal(specshiny:::run_spec_preview_sim(s_glmer, max_reps = 1L)$summary$n_reps, 1L)

  s_long_mar <- use_spec_template("longitudinal_growth_ar1", n_clusters = 30L)
  s_long_mar$missingness$auxiliary <- "treatment"
  s_long_mar$missingness$strength <- 1.2
  expect_equal(specshiny:::run_spec_preview_sim(s_long_mar, max_reps = 2L)$summary$n_reps, 2L)

  # 5. mod_spec_preview_server JSON toggle + preview UI rendering
  spec_rv <- shiny::reactiveVal(s_logit)
  shiny::testServer(
    mod_spec_preview_server,
    args = list(spec_rv = spec_rv, max_preview_reps = 2L),
    {
      session$setInputs(ir_format = "json")
      expect_true(grepl('"schema_version"', output$ir_code, fixed = TRUE))
      expect_true(grepl("AST Guardrails", as.character(output$summary_ui$html), fixed = TRUE))
      expect_true(grepl("run_simulation", output$r_code, fixed = TRUE))
      expect_true(grepl("---", output$qmd_code, fixed = TRUE))
      session$setInputs(run_preview = 1L)
      expect_true(grepl("Quick Preview Simulation", as.character(output$preview_run_ui$html), fixed = TRUE))
    }
  )
})

test_that("ui-synthesize and mod_spec_controls_server cover all control patches and grid sweeps", {
  s_full <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 15L, cluster_size_dist = "normal", cluster_size_sd = 4, icc = 0.2),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal", sd = 1.0, centering = "none"),
      spec_predictor("x2", level = 1L, distribution = "uniform", min = -1, max = 1, centering = "none"),
      spec_predictor("trt", level = 2L, distribution = "binary", prob = 0.5, centering = "none")
    ),
    predictor_corr = matrix(c(1, 0.25, 0, 0.25, 1, 0, 0, 0, 1), nrow = 3L),
    outcome = list(name = "y", family = "gaussian", residual_sd = 1.0, error_dist = "t", error_df = 6),
    effects = list(
      intercept = 0,
      fixed = list(x1 = 0.3, x2 = 0.1, trt = 0.4),
      interactions = list(spec_interaction("x1", "trt", beta = 0.2)),
      focal_predictor = "x1:trt",
      random = list(intercept = TRUE, slopes = list(list(predictor = "x1", sd = 0.2)), corr_matrix = matrix(c(1, 0.1, 0.1, 1), 2, 2))
    ),
    missingness = list(mechanism = "MAR", rate = 0.15, auxiliary = "x2", strength = 0.8),
    simulation = list(n_reps = 50L, seed = 10L, alpha = 0.05, parameter_grid = list(n_clusters = c(30, 50)))
  )

  desc <- spec_to_controls(s_full)
  expect_no_error(suppressMessages(print(desc)))
  expect_true(all(c("cluster_size_sd", "error_df", "pred_min_x2", "pred_max_x2", "pcorr_1_2", "inter_beta_x1__trt", "slope_sd_x1", "corr_1_2", "missing_auxiliary", "missing_strength", "grid_n_clusters") %in% desc$control_ids))
  expect_error(render_spec_controls_ui(list()), "spec_ui_descriptor")

  # Patch every control family
  expect_false(patch_spec_from_control(s_full, "n_clusters", NULL)$changed)
  expect_equal(patch_spec_from_control(s_full, "cluster_size", 20L)$spec$hierarchy$cluster_size, 20L)
  expect_equal(patch_spec_from_control(s_full, "cluster_size_dist", "poisson")$spec$hierarchy$cluster_size_dist, "poisson")
  expect_equal(patch_spec_from_control(s_full, "cluster_size_dist", "fixed")$spec$hierarchy$cluster_size_dist, "fixed")
  expect_equal(patch_spec_from_control(s_full, "cluster_size_sd", 3)$spec$hierarchy$cluster_size_sd, 3)
  expect_equal(patch_spec_from_control(s_full, "residual_sd", 1.5)$spec$outcome$residual_sd, 1.5)
  expect_equal(patch_spec_from_control(s_full, "error_dist", "skewed")$spec$outcome$error_dist, "skewed")
  expect_equal(patch_spec_from_control(s_full, "error_dist", "normal")$spec$outcome$error_dist, "normal")
  expect_equal(patch_spec_from_control(s_full, "error_df", 8)$spec$outcome$error_df, 8)
  s_sk <- patch_spec_from_control(s_full, "error_dist", "skewed")$spec
  expect_equal(patch_spec_from_control(s_sk, "error_skew", 2.0)$spec$outcome$error_skew, 2.0)
  expect_equal(patch_spec_from_control(s_full, "effects_intercept", 1.2)$spec$effects$intercept, 1.2)
  expect_equal(patch_spec_from_control(s_full, "focal_predictor", "trt")$spec$effects$focal_predictor, "trt")
  expect_equal(patch_spec_from_control(s_full, "missing_mechanism", "none")$spec$missingness$mechanism, "none")
  s_none <- patch_spec_from_control(s_full, "missing_mechanism", "none")$spec
  expect_equal(patch_spec_from_control(s_none, "missing_mechanism", "MAR")$spec$missingness$mechanism, "MAR")
  expect_equal(patch_spec_from_control(s_none, "missing_mechanism", "MCAR")$spec$missingness$mechanism, "MCAR")
  expect_equal(patch_spec_from_control(s_full, "missing_rate", 0.25)$spec$missingness$rate, 0.25)
  expect_equal(patch_spec_from_control(s_full, "missing_auxiliary", "x1")$spec$missingness$auxiliary, "x1")
  expect_equal(patch_spec_from_control(s_full, "missing_strength", 1.4)$spec$missingness$strength, 1.4)
  expect_equal(patch_spec_from_control(s_full, "n_reps", 100L)$spec$simulation$n_reps, 100L)
  expect_equal(patch_spec_from_control(s_full, "alpha", 0.01)$spec$estimator$conf_level, 0.99)
  expect_equal(patch_spec_from_control(s_full, "seed", 777L)$spec$simulation$seed, 777L)
  expect_false(patch_spec_from_control(s_full, "estimator_reml", FALSE)$spec$estimator$reml)
  expect_equal(patch_spec_from_control(s_full, "pred_sd_x1", 2.0)$spec$predictors[[1L]]$sd, 2.0)
  expect_equal(patch_spec_from_control(s_full, "pred_min_x2", -2)$spec$predictors[[2L]]$min, -2)
  expect_equal(patch_spec_from_control(s_full, "pred_max_x2", 2)$spec$predictors[[2L]]$max, 2)
  expect_equal(patch_spec_from_control(s_full, "pred_prob_trt", 0.4)$spec$predictors[[3L]]$prob, 0.4)
  expect_equal(patch_spec_from_control(s_full, "pred_centering_x1", "group_mean")$spec$predictors[[1L]]$centering, "group_mean")
  expect_equal(patch_spec_from_control(s_full, "inter_beta_x1__trt", 0.5)$spec$effects$interactions[[1L]]$beta, 0.5)
  expect_equal(patch_spec_from_control(s_full, "slope_sd_x1", 0.35)$spec$effects$random$slopes[[1L]]$sd, 0.35)
  expect_equal(patch_spec_from_control(s_full, "corr_1_2", 0.3)$spec$effects$random$corr_matrix[1L, 2L], 0.3)
  expect_equal(patch_spec_from_control(s_full, "pcorr_1_2", 0.45)$spec$predictor_corr[1L, 2L], 0.45)
  expect_null(patch_spec_from_control(s_full, "grid_n_clusters", "")$spec$simulation$parameter_grid$n_clusters)
  expect_false(patch_spec_from_control(s_full, "grid_n_clusters", "abc,def")$validation$valid)

  # outcome_link patch on binomial
  s_bin <- patch_spec_from_control(s_full, "outcome_family", "binomial")$spec
  expect_equal(patch_spec_from_control(s_bin, "outcome_link", "probit")$spec$outcome$link, "probit")

  # default_grid_values_for_axis across all axes
  s_1l <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
  expect_length(specshiny:::default_grid_values_for_axis(s_1l, "n_obs"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "n_clusters"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "cluster_size"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "icc"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "focal_effect"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "residual_sd"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "missing_rate"), 3L)
  expect_length(specshiny:::default_grid_values_for_axis(s_full, "ar1_rho"), 3L)
  expect_equal(specshiny:::default_grid_values_for_axis(s_full, "other"), c(1, 2))
})

test_that("translator_rule, refine_spec, and apply_clarifications cover rich NL syntax branches", {
  # Rich creation prompts
  t1 <- nl_to_spec("2-level study with 30 schools, 15 per school, unbalanced cluster sizes, binary x1, uniform x2, grand-mean centered x1, negative effect size 0.4, random effect correlation 0.3, missingness 10%, ML estimation")
  expect_true(is_valid_spec_ir(t1$spec))
  expect_equal(t1$spec$hierarchy$cluster_size_dist, "normal")
  expect_false(t1$spec$estimator$reml)
  expect_no_error(suppressMessages(print(t1)))

  t2 <- nl_to_spec("single-level count outcome with 3 covariates, treatment allocation 40%, sweep sample sizes 100, 200, 300")
  expect_true(is_valid_spec_ir(t2$spec))
  expect_equal(t2$spec$outcome$family, "poisson")
  expect_equal(t2$spec$simulation$parameter_grid$n_obs, c(100, 200, 300))

  t3 <- nl_to_spec("2-level study with 25 clinics, cluster size 12, poisson cluster sizes, binary outcome probit link, skewed errors, sweep cluster sizes 10, 20 and sweep effect sizes 0.2, 0.5")
  expect_true(is_valid_spec_ir(t3$spec))
  expect_equal(t3$spec$outcome$link, "probit")

  t4 <- nl_to_spec("single-level study with skewed errors 1.8 and 15% MAR missingness, sweep missing rates 0.1, 0.25")
  expect_true(is_valid_spec_ir(t4$spec))
  expect_equal(t4$spec$outcome$error_dist, "skewed")
  expect_equal(t4$spec$simulation$parameter_grid$missing_rate, c(0.1, 0.25))

  # Broad refinement sequence exercising almost every refine_spec_backend branch
  sp <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2),
    predictors = list(spec_predictor("x1", level = 1L), spec_predictor("x2", level = 1L)),
    effects = list(intercept = 0, fixed = list(x1 = 0.3, x2 = 0.1), focal_predictor = "x1", random = list(intercept = TRUE, slopes = list()))
  )
  steps <- c(
    "set cluster size to 25 and cluster size sd 5",
    "use poisson cluster sizes",
    "use fixed cluster sizes",
    "use unbalanced cluster sizes",
    "use fixed cluster sizes",
    "use t-distributed errors df 6",
    "use skewed errors 1.4",
    "use normal errors and residual sd 1.3",
    "switch to binomial outcome with probit link",
    "use logit link",
    "switch to poisson outcome",
    "switch to gaussian outcome",
    "add binary level-2 predictor z2",
    "set predictor correlation 0.35",
    "clear predictor correlation",
    "grand-mean center x1",
    "set effect size to 0.5",
    "add interaction between x1 and z2 of 0.25",
    "remove interaction",
    "add random slope for x1 sd 0.3",
    "remove predictor z2",
    "use ML estimation and alpha 0.01 and seed 999",
    "switch to 1-level"
  )
  cur <- sp
  for (inst in steps) {
    r <- refine_spec(cur, inst)
    expect_true(is_valid_spec_ir(r$spec), info = inst)
    cur <- r$spec
  }
  expect_no_error(suppressMessages(print(r)))
  expect_equal(cur$hierarchy$levels, 1L)
  expect_equal(cur$simulation$alpha, 0.01)
  expect_equal(cur$simulation$seed, 999L)

  # Answer clarifications across all supported paths + error on unknown path
  s2 <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 15L, icc = 0.2),
    predictors = list(spec_predictor("x1", level = 1L, centering = "group_mean"), spec_predictor("x2", level = 1L)),
    effects = list(intercept = 0, fixed = list(x1 = 0.3, x2 = 0.2), focal_predictor = "x1", random = list(intercept = TRUE, slopes = list()))
  )
  s2_c <- answer_clarifications(s2, list(
    "hierarchy.cluster_size" = 20L,
    "effects.fixed.x1" = 0.45,
    "effects.random.slopes.x2.sd" = 0.2,
    "predictors.x1.level" = 2L,
    "missingness.mechanism" = "MAR",
    "missingness.rate" = 0.2,
    "simulation.n_reps" = 150L,
    "simulation.alpha" = 0.01
  ))$spec
  expect_true(is_valid_spec_ir(s2_c))
  expect_equal(s2_c$hierarchy$cluster_size, 20L)
  expect_equal(s2_c$predictors[[1L]]$level, 2L)
  expect_equal(s2_c$predictors[[1L]]$centering, "none")
  expect_equal(s2_c$missingness$mechanism, "MAR")
  expect_equal(s2_c$estimator$conf_level, 0.99)

  s2_mcar <- answer_clarifications(s2_c, list("missingness.mechanism" = "MCAR"))$spec
  expect_null(s2_mcar$missingness$auxiliary)
  s2_none <- answer_clarifications(s2_mcar, list("missingness.mechanism" = "none"))$spec
  expect_equal(s2_none$missingness$rate, 0)

  s1_c <- answer_clarifications(new_spec_ir(), list("hierarchy.n_obs" = 350L))$spec
  expect_equal(s1_c$hierarchy$n_obs, 350L)
  expect_error(answer_clarifications(s1_c, list("unsupported.path" = 1)), "Unsupported clarification path")
  expect_error(answer_clarifications(list(), list(a = 1)), "spec_ir")
  expect_error(answer_clarifications(s1_c, list(1)), "named list")

  # Input guards on nl_to_spec & refine_spec
  expect_error(nl_to_spec(""), "non-empty")
  expect_error(nl_to_spec("ok", translator = list()), "spec_translator")
  expect_error(refine_spec(list(), "x"), "spec_ir")
  expect_error(refine_spec(s1_c, ""), "non-empty")
  expect_error(refine_spec(s1_c, "x", translator = list()), "spec_translator")
})

test_that("validate_spec_ir, spec-ir constructors, security AST, and bundle cover defensive branches", {
  s1 <- new_spec_ir()
  s2 <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 15L, icc = 0.2),
    predictors = list(spec_predictor("x1", level = 1L), spec_predictor("z1", level = 2L)),
    effects = list(intercept = 0, fixed = list(x1 = 0.3, z1 = 0.2), focal_predictor = "x1", random = list(intercept = TRUE, slopes = list()))
  )

  # Validation error branches
  expect_false(validate_spec_ir(list())$valid)
  expect_false(validate_spec_ir("not_a_list")$valid)

  b <- s1; b$metadata <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$metadata$title <- ""; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$metadata$description <- 123; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$metadata$source_prompt <- 123; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$design_type <- "unknown_type"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$hierarchy <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$hierarchy$levels <- 3L; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$hierarchy$n_obs <- 2L; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$hierarchy$unit_name <- "123bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$design_type <- "multilevel_sim"; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$design_type <- "monte_carlo_lm"; expect_false(validate_spec_ir(b)$valid)

  b <- s2; b$hierarchy$n_clusters <- 2L; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$hierarchy$cluster_size <- 1L; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$hierarchy$icc <- 1.0; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$hierarchy$cluster_name <- "bad name"; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$hierarchy$cluster_size_dist <- "weibull"; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$hierarchy$cluster_size_dist <- "normal"; b$hierarchy$cluster_size_sd <- -1; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$hierarchy$cluster_size_dist <- "fixed"; b$hierarchy$cluster_size_sd <- 2; expect_false(validate_spec_ir(b)$valid)

  # Predictors & predictor_corr errors
  b <- s1; b$predictors <- list(); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors <- list("bad"); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors <- list(spec_predictor("x1"), spec_predictor("x1")); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]]$level <- 2L; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]]$distribution <- "gamma"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]]$sd <- -1; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]]$centering <- "invalid"; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$predictors[[2L]]$centering <- "group_mean"; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$predictor_corr <- "not_a_matrix"; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$predictor_corr <- matrix(c(1, 0.4, 0.4, 1), 2, 2); expect_false(validate_spec_ir(b)$valid) # cross-level!
  b <- s2; b$predictor_corr <- matrix(c(1, NA, NA, 1), 2, 2); expect_false(validate_spec_ir(b)$valid)

  # Outcome, effects, missingness, estimator, simulation, defaults_rationale errors
  b <- s1; b$outcome <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$outcome$family <- "gamma"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$outcome$link <- "logit"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$outcome$residual_sd <- -2; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$outcome$error_dist <- "laplace"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$outcome$error_dist <- "t"; b$outcome$error_df <- 1.5; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$outcome$error_dist <- "skewed"; b$outcome$error_skew <- 0; expect_false(validate_spec_ir(b)$valid)

  b <- s1; b$effects <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$intercept <- "x"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$fixed <- list(x1 = "bad"); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$interactions <- list("bad"); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$interactions <- list(list(terms = c("x1"), beta = 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$interactions <- list(list(terms = c("x1", "x1"), beta = 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$interactions <- list(list(terms = c("x1", "ghost"), beta = "bad")); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$focal_predictor <- "ghost"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$random$intercept <- TRUE; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$random$slopes <- list(list(predictor = "x1", sd = 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$effects$random$corr_matrix <- diag(2); expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$effects$random$intercept_sd <- -1; expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$effects$random$slopes <- list(list(predictor = "z1", sd = -0.1)); expect_false(validate_spec_ir(b)$valid)

  b <- s1; b$missingness <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$missingness$mechanism <- "MNAR"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$missingness$mechanism <- "MCAR"; b$missingness$rate <- 0; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$missingness$mechanism <- "MAR"; b$missingness$rate <- 0.2; b$missingness$auxiliary <- "ghost"; b$missingness$strength <- 0; expect_false(validate_spec_ir(b)$valid)

  b <- s1; b$estimator <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$estimator$engine <- "stan"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$estimator$engine <- "lmer"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$estimator$conf_level <- 1.5; expect_false(validate_spec_ir(b)$valid)

  b <- s1; b$simulation <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$n_reps <- 0L; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$seed <- 1.5; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$alpha <- 0.8; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$parameter_grid <- list(unknown_axis = c(1, 2)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$parameter_grid <- list(n_obs = c(Inf, 10)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$parameter_grid <- list(n_clusters = c(20, 40)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$parameter_grid <- list(cluster_size = c(10, 20)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$parameter_grid <- list(icc = c(0.1, 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$simulation$parameter_grid <- list(missing_rate = c(0.1, 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$defaults_rationale <- "bad"; expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$defaults_rationale <- list("bad key!" = 123); expect_false(validate_spec_ir(b)$valid)

  # Print validation report with errors & warnings
  rep_err <- validate_spec_ir(b, strict = FALSE)
  expect_no_error(suppressMessages(print(rep_err)))
  s_warn <- s1; s_warn$schema_version <- "1.0.0"
  rep_warn <- validate_spec_ir(s_warn, strict = FALSE)
  expect_no_error(suppressMessages(print(rep_warn)))

  # spec_predictor & spec_interaction descriptor validation via validate_spec_ir
  expect_error(spec_predictor("x", distribution = "beta"), "should be one of")
  b <- s1; b$predictors[[1L]] <- spec_predictor("x", distribution = "normal", sd = 0); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]] <- spec_predictor("x", distribution = "binary", prob = 1.2); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]] <- spec_predictor("x", distribution = "uniform", min = 5, max = 2); expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$predictors[[2L]] <- spec_predictor("z1", level = 2L, centering = "group_mean"); expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$effects$interactions <- list(spec_interaction("x1", "x1", 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$effects$interactions <- list(spec_interaction("1bad", "z1", 0.2)); expect_false(validate_spec_ir(b)$valid)
  b <- s2; b$effects$interactions <- list(spec_interaction("x1", "z1", NA_real_)); expect_false(validate_spec_ir(b)$valid)
  expect_error(as_spec_ir("bad"), "Cannot coerce")
  expect_true(is.na(spec_total_n(NULL)))
  expect_no_error(suppressMessages(print(summary(s2))))
  # Also test spec_predictor bad identifier/level caught by validate_spec_ir
  b <- s1; b$predictors[[1L]] <- spec_predictor("1bad"); expect_false(validate_spec_ir(b)$valid)
  b <- s1; b$predictors[[1L]] <- spec_predictor("x", level = 3L); expect_false(validate_spec_ir(b)$valid)

  # AST edge branches: ::: callee, dynamic callee, unwhitelisted pkg::fn, bare pkg::fn symbol, dynamic library()
  expect_false(verify_compiled_r_ast("stats:::C_Cdqrls()", strict = FALSE)$safe)
  expect_false(verify_compiled_r_ast("(get('mean'))(1:5)", strict = FALSE)$safe)
  expect_false(verify_compiled_r_ast("utils::read.csv('a.csv')", strict = FALSE)$safe)
  expect_false(verify_compiled_r_ast("f <- utils::read.csv", strict = FALSE)$safe)
  expect_false(verify_compiled_r_ast("library(pkg_var)", strict = FALSE)$safe)

  # Bundle defensive errors
  tmp_b <- tempfile("bundle_cov_")
  on.exit(unlink(tmp_b, recursive = TRUE), add = TRUE)
  expect_error(export_spec_bundle(list(), tmp_b), "spec_ir")
  export_spec_bundle(s1, tmp_b, overwrite = TRUE)
  expect_error(export_spec_bundle(s1, tmp_b, overwrite = FALSE), "Refusing to overwrite")
  expect_false(verify_spec_bundle(tempfile("missing_dir_"))$valid)
  # Tamper with simulation.R to test checksum mismatch
  writeLines("# tampered", file.path(tmp_b, "simulation.R"))
  vb <- verify_spec_bundle(tmp_b)
  expect_false(vb$valid)
  expect_no_error(suppressMessages(print(vb)))
})
