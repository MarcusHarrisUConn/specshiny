test_that("translator_rule accurately parses the flagship multilevel prompt", {
  prompt <- paste(
    "2-level multilevel power simulation with 60 schools, 25 students per school,",
    "ICC 0.2, treatment effect 0.35, random slope for x1, 15% MAR missingness,",
    "500 replications"
  )
  res <- nl_to_spec(prompt, translator = translator_rule())

  expect_s3_class(res, "spec_translation")
  expect_true(res$validation$valid)
  expect_equal(res$backend, "rule")

  sp <- res$spec
  expect_equal(sp$design_type, "power_analysis")
  expect_equal(sp$hierarchy$levels, 2L)
  expect_equal(sp$hierarchy$n_clusters, 60L)
  expect_equal(sp$hierarchy$cluster_size, 25L)
  expect_equal(sp$hierarchy$cluster_name, "school")
  expect_equal(sp$hierarchy$unit_name, "student")
  expect_equal(sp$hierarchy$icc, 0.2)
  expect_equal(sp$effects$focal_predictor, "treatment")
  expect_equal(sp$effects$fixed$treatment, 0.35)
  expect_equal(length(sp$effects$random$slopes), 1L)
  expect_equal(sp$effects$random$slopes[[1L]]$predictor, "x1")
  expect_equal(sp$missingness$mechanism, "MAR")
  expect_equal(sp$missingness$rate, 0.15)
  expect_equal(sp$simulation$n_reps, 500L)
  expect_equal(sp$estimator$engine, "lmer")
  expect_true(length(sp$defaults_rationale) >= 3L)
  expect_true(length(res$clarifications) >= 1L)
})

test_that("translator_rule parses single-level, binomial, poisson, and grid sweep prompts", {
  r1 <- nl_to_spec("Single-level Monte Carlo simulation with 300 participants, binary outcome, x1 effect 0.6, 10% MCAR missingness, 400 reps")
  expect_true(r1$validation$valid)
  expect_equal(r1$spec$hierarchy$levels, 1L)
  expect_equal(r1$spec$hierarchy$n_obs, 300L)
  expect_equal(r1$spec$outcome$family, "binomial")
  expect_equal(r1$spec$outcome$link, "logit")
  expect_equal(r1$spec$estimator$engine, "glm")
  expect_equal(r1$spec$missingness$mechanism, "MCAR")
  expect_equal(r1$spec$missingness$rate, 0.10)

  r2 <- nl_to_spec("2-level count outcome Poisson study with 40 clinics, 15 patients per clinic, ICC 0.1, level-2 treatment effect 0.5, sweep clusters across 30, 50, 70")
  expect_true(r2$validation$valid)
  expect_equal(r2$spec$outcome$family, "poisson")
  expect_equal(r2$spec$estimator$engine, "glmer")
  expect_equal(r2$spec$predictors[[1L]]$level, 2L)
  expect_equal(r2$spec$simulation$parameter_grid$n_clusters, c(30L, 50L, 70L))
})

test_that("underspecified prompts generate structured clarifications and answer_clarifications resolves them", {
  res <- nl_to_spec("2-level power analysis")
  expect_true(res$validation$valid)
  cl_ids <- vapply(res$clarifications, `[[`, character(1), "id")
  expect_true(all(c("n_clusters", "cluster_size", "icc") %in% cl_ids))
  expect_true(length(res$spec$defaults_rationale) >= 4L)

  upd <- answer_clarifications(res, list(n_clusters = 80, icc = 0.25))
  expect_true(upd$validation$valid)
  expect_equal(upd$spec$hierarchy$n_clusters, 80L)
  expect_equal(upd$spec$hierarchy$icc, 0.25)
  expect_true(upd$diff$has_changes)
  rem_ids <- vapply(upd$clarifications, `[[`, character(1), "id")
  expect_false("n_clusters" %in% rem_ids)
  expect_false("icc" %in% rem_ids)
  expect_match(upd$spec$defaults_rationale[["hierarchy.n_clusters"]], "Explicitly confirmed")
})

test_that("refine_spec applies iterative conversational updates and tracks semantic diffs", {
  base <- nl_to_spec("2-level study with 40 schools, 20 students per school, ICC 0.15, treatment effect 0.3, 200 reps")

  r1 <- refine_spec(base, "increase clusters to 70 and set treatment effect to 0.45")
  expect_s3_class(r1, "spec_refinement")
  expect_equal(r1$spec$hierarchy$n_clusters, 70L)
  expect_equal(r1$spec$effects$fixed$treatment, 0.45)
  expect_true(all(c("hierarchy.n_clusters", "effects.fixed.treatment") %in% r1$diff$changed_paths))

  r2 <- refine_spec(r1, "add a random slope for x1 with sd 0.3 and correlation -0.2")
  expect_true(r2$validation$valid)
  expect_equal(r2$spec$effects$random$slopes[[1L]]$predictor, "x1")
  expect_equal(r2$spec$effects$random$slopes[[1L]]$sd, 0.3)
  expect_equal(r2$spec$effects$random$corr_matrix[1, 2], -0.2)

  r3 <- refine_spec(r2, "set 20% MAR missingness and sweep ICC across 0.1, 0.2, 0.3")
  expect_equal(r3$spec$missingness$mechanism, "MAR")
  expect_equal(r3$spec$missingness$rate, 0.20)
  expect_equal(r3$spec$simulation$parameter_grid$icc, c(0.1, 0.2, 0.3))

  r4 <- refine_spec(r3, "remove missingness and remove random slope for x1 and clear grid")
  expect_equal(r4$spec$missingness$mechanism, "none")
  expect_equal(length(r4$spec$effects$random$slopes), 0L)
  expect_equal(length(r4$spec$simulation$parameter_grid), 0L)

  r5 <- refine_spec(r4, "switch to binomial outcome and 500 replications")
  expect_equal(r5$spec$outcome$family, "binomial")
  expect_equal(r5$spec$estimator$engine, "glmer")
  expect_equal(r5$spec$simulation$n_reps, 500L)
})

test_that("translator_openai and translator_ollama parse mocked JSON responses safely without eval", {
  target_spec <- new_spec_ir(
    design_type = "monte_carlo_lm",
    title = "Mocked LLM Spec",
    hierarchy = list(levels = 1L, n_obs = 180L),
    simulation = list(n_reps = 100L, seed = 7L)
  )
  envelope_json <- jsonlite::toJSON(
    list(
      spec = jsonlite::fromJSON(as_spec_json(target_spec), simplifyVector = FALSE),
      clarifications = list(
        list(id = "n_obs", path = "hierarchy.n_obs", question = "Confirm N?", current_default = 180, options = list(100, 180, 300), rationale = "Test")
      )
    ),
    auto_unbox = TRUE, null = "null"
  )

  # Mock OpenAI response (wrapped in markdown fence to test safe fence stripping)
  openai_tr <- translator_openai(model = "gpt-4o-mini", api_key = "test-key")
  res_oai <- httr2::with_mocked_responses(
    function(req) {
      httr2::response_json(
        status_code = 200L,
        body = list(choices = list(list(message = list(content = paste0("```json\n", envelope_json, "\n```")))))
      )
    },
    nl_to_spec("Simple 1-level linear simulation N=180", translator = openai_tr)
  )
  expect_true(res_oai$validation$valid)
  expect_equal(res_oai$backend, "openai")
  expect_equal(res_oai$spec$hierarchy$n_obs, 180L)
  expect_equal(length(res_oai$clarifications), 1L)

  # Mock Ollama response
  ollama_tr <- translator_ollama(model = "llama3.1")
  res_oll <- httr2::with_mocked_responses(
    function(req) {
      httr2::response_json(
        status_code = 200L,
        body = list(message = list(content = as.character(envelope_json)))
      )
    },
    nl_to_spec("Simple 1-level linear simulation N=180", translator = ollama_tr)
  )
  expect_true(res_oll$validation$valid)
  expect_equal(res_oll$backend, "ollama")
  expect_equal(res_oll$spec$hierarchy$n_obs, 180L)

  # Invalid R code returned by malicious LLM is rejected cleanly without execution
  expect_error(
    httr2::with_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = list(message = list(content = "system('echo NEVER_RUN'); stop('boom')"))
        )
      },
      nl_to_spec("Malicious response test", translator = ollama_tr)
    ),
    class = "specshiny_llm_parse_error"
  )
})
