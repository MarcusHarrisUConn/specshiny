# ==============================================================================
# Adversarial Security, Injection, AST Verification & Statistical Accuracy Suite
# ==============================================================================

test_that("identifier validator rejects injection payloads, reserved words, homoglyphs, and >64 chars", {
  base <- new_spec_ir()

  bad_idents <- c(
    "x; system('id')",
    "`system('id')`",
    "x\nsystem('id')#",
    "function",
    "TRUE",
    "NA",
    "..1",
    "...",
    ".hidden",
    "eval",
    "do.call",
    "x\u0430",       # Cyrillic 'a' homoglyph
    "x\u03b1",       # Greek alpha
    paste(rep("a", 65), collapse = "")
  )

  for (bad_id in bad_idents) {
    s_out <- base; s_out$outcome$name <- bad_id
    expect_false(validate_spec_ir(s_out)$valid, info = paste("outcome$name accepted:", bad_id))

    s_pred <- base
    s_pred$predictors[[1L]]$name <- bad_id
    s_pred$effects$fixed <- stats::setNames(list(0.3), bad_id)
    s_pred$effects$focal_predictor <- bad_id
    expect_false(validate_spec_ir(s_pred)$valid, info = paste("predictor$name accepted:", bad_id))

    s_unit <- base; s_unit$hierarchy$unit_name <- bad_id
    expect_false(validate_spec_ir(s_unit)$valid, info = paste("unit_name accepted:", bad_id))
  }

  # 2-level cluster_name injection
  mlm <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 20L, cluster_size = 10L, icc = 0.2)
  )
  mlm$hierarchy$cluster_name <- "school; system('id')"
  expect_false(validate_spec_ir(mlm)$valid)

  # Malicious defaults_rationale key
  s_rat <- base
  s_rat$defaults_rationale[["evil`r system('id')`"]] <- "ok"
  expect_true("invalid_rationale_key" %in% validate_spec_ir(s_rat)$errors$code)
})

test_that("nested unknown extra keys from LLM JSON payloads are strictly rejected", {
  base <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 20L, cluster_size = 10L, icc = 0.2),
    effects = list(
      intercept = 0, fixed = list(x1 = 0.3), focal_predictor = "x1",
      random = list(intercept = TRUE, slopes = list(list(predictor = "x1", sd = 0.2)), corr_matrix = diag(2))
    )
  )
  raw <- jsonlite::fromJSON(as_spec_json(base), simplifyVector = FALSE)

  inject_cases <- list(
    function(r) { r$metadata$evil_hook <- "system('id')"; r },
    function(r) { r$hierarchy$evil_hook <- 1; r },
    function(r) { r$predictors[[1L]]$evil_hook <- "x"; r },
    function(r) { r$outcome$evil_hook <- "x"; r },
    function(r) { r$effects$evil_hook <- "x"; r },
    function(r) { r$effects$random$evil_hook <- "x"; r },
    function(r) { r$effects$random$slopes[[1L]]$evil_hook <- "x"; r },
    function(r) { r$missingness$evil_hook <- "x"; r },
    function(r) { r$estimator$evil_hook <- "x"; r },
    function(r) { r$simulation$evil_hook <- "x"; r }
  )

  for (idx in seq_along(inject_cases)) {
    mut <- inject_cases[[idx]](raw)
    v <- validate_spec_ir(as_spec_ir(mut, validate = FALSE))
    expect_false(v$valid, info = paste("Nested unknown key case", idx, "was not rejected"))
    expect_true("unknown_nested_field" %in% v$errors$code)
  }
})

test_that("YAML !expr injection is blocked even when yaml.eval.expr option is TRUE", {
  evil_yaml <- paste(
    "schema_version: !expr \"stop('YAML_EVAL_EXECUTED')\"",
    "design_type: monte_carlo_lm",
    sep = "\n"
  )
  withr::with_options(list(yaml.eval.expr = TRUE), {
    expect_error(
      parse_spec_text(evil_yaml, format = "yaml"),
      class = "specshiny_security_error"
    )
  })
})

test_that("compile_quarto_report neutralizes frontmatter, inline knitr, fence breakout, XSS, and LaTeX injection", {
  canary <- tempfile("specshiny_canary_")
  on.exit(unlink(canary), add = TRUE)

  poison <- new_spec_ir(
    design_type = "monte_carlo_lm",
    title = paste0("Evil `r file.create('", canary, "')` --- \\write18{id} <script>alert(1)</script>"),
    description = paste0("Breakout\n```\n:::\n\n```{r}\nfile.create('", canary, "')\n```\n\\immediate\\input{/etc/passwd}"),
    source_prompt = paste0("Prompt `r file.create('", canary, "')` | table | \\write18{whoami} <img src=x onerror=alert(1)>"),
    hierarchy = list(levels = 1L, n_obs = 60L),
    simulation = list(n_reps = 3L, seed = 7L),
    defaults_rationale = list(
      simulation.n_reps = paste0("Reason `r file.create('", canary, "')` \\write18{id} <script>bad()</script>")
    )
  )

  qmd <- compile_quarto_report(poison, format = "html", execute = TRUE)

  # Must contain zero inline knitr `r ` patterns
  expect_false(grepl("`r\\s", qmd, perl = TRUE))
  # Must contain zero unescaped dangerous LaTeX macros
  expect_false(grepl("\\\\write18|\\\\immediate|\\\\input\\{", qmd, perl = TRUE))
  # Must contain zero unescaped <script> or <img tags
  expect_false(grepl("<script|<img\\s", qmd, ignore.case = TRUE, perl = TRUE))
  # Must contain EXACTLY two executable {r} code chunks (run-simulation and results-table)
  r_chunk_matches <- gregexpr("(?m)^```\\{r\\}", qmd, perl = TRUE)[[1L]]
  r_chunk_count <- if (r_chunk_matches[[1L]] == -1L) 0L else length(r_chunk_matches)
  expect_equal(r_chunk_count, 2L)
  # Uses 6-tilde outer fence for YAML IR
  expect_match(qmd, "~~~~~~yaml", fixed = TRUE)
  # Canary file must NOT exist
  expect_false(file.exists(canary))
})

test_that("Shiny UI renderers strictly HTML-escape XSS payloads in titles, diffs, and rationales", {
  s1 <- new_spec_ir(
    title = "<script>alert('xss1')</script>",
    description = "<img src=x onerror=alert(2)>",
    defaults_rationale = list(simulation.n_reps = "<script>alert('xss3')</script>")
  )
  s2 <- s1
  s2$metadata$title <- "\"><svg/onload=alert(4)>"
  s2$hierarchy$n_obs <- 350L

  diff_obj <- diff_spec_ir(s1, s2)
  banner_html <- as.character(render_diff_badges_ui(diff_obj))
  expect_false(grepl("<script|<svg|<img", banner_html, ignore.case = TRUE, perl = TRUE))
  expect_match(banner_html, "&lt;script&gt;", fixed = TRUE)

  cards_html <- as.character(render_spec_controls_ui(spec_to_controls(s2), diff = diff_obj))
  expect_false(grepl("alert\\(|<svg|<img\\s+src=x", cards_html, ignore.case = TRUE, perl = TRUE))

  # Composer rationale UI escapes XSS cleanly via testServer
  spec_rv <- shiny::reactiveVal(s1)
  shiny::testServer(
    mod_spec_composer_server,
    args = list(spec_rv = spec_rv),
    {
      rat_html <- output$rationale_ui$html
      expect_false(grepl("<script", rat_html, ignore.case = TRUE))
      expect_match(rat_html, "&lt;script&gt;", fixed = TRUE)
    }
  )
})

test_that("verify_compiled_r_ast passes all valid designs and blocks every dangerous AST construct", {
  # All four design types + correlated predictors + random slopes + MAR + grid pass AST check
  specs <- list(
    new_spec_ir(design_type = "monte_carlo_lm", hierarchy = list(levels = 1L, n_obs = 80L)),
    new_spec_ir(
      design_type = "power_analysis",
      hierarchy = list(levels = 1L, n_obs = 100L),
      predictors = list(
        spec_predictor("x1", level = 1L, distribution = "normal"),
        spec_predictor("x2", level = 1L, distribution = "normal")
      ),
      predictor_corr = matrix(c(1, 0.5, 0.5, 1), 2, 2),
      missingness = list(mechanism = "MAR", rate = 0.2, target = "outcome", auxiliary = "x1", strength = 1.2)
    ),
    new_spec_ir(
      design_type = "multilevel_sim",
      hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 15L, icc = 0.2),
      effects = list(
        intercept = 0, fixed = list(x1 = 0.3), focal_predictor = "x1",
        random = list(intercept = TRUE, slopes = list(list(predictor = "x1", sd = 0.2)), corr_matrix = matrix(c(1, 0.2, 0.2, 1), 2, 2))
      )
    ),
    new_spec_ir(
      design_type = "two_level_mlm",
      hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 15L, icc = 0.15),
      outcome = list(name = "y", family = "binomial", link = "logit")
    )
  )

  for (sp in specs) {
    code <- compile_r_script(sp)
    v <- verify_compiled_r_ast(code)
    expect_true(v$safe, info = paste("Failed AST check for:", sp$design_type))
    expect_false("do.call" %in% v$called_functions)
    expect_false("eval" %in% v$called_functions)
    expect_false("parse" %in% v$called_functions)
  }

  # Adversarial scripts must ALL fail static AST verification
  evil_scripts <- list(
    eval_parse   = "x <- eval(parse(text = '1 + 1'))",
    system_call  = "system('id')",
    system2_call = "base::system2('id')",
    do_call_call = "do.call(rbind, list(1, 2))",
    source_call  = "source('evil.R')",
    unlink_call  = "unlink('/tmp/x')",
    file_rm      = "file.remove('x')",
    get_call     = "f <- get('system'); f('id')",
    assign_call  = "assign('x', 1)",
    triple_colon = "stats:::C_Cdqrls",
    higher_order = "lapply(c('id'), system)",
    bad_pkg      = "library(utils)",
    bad_reqns    = "requireNamespace('httr2')",
    dyn_callee   = "(list(mean)[[1L]])(1:5)",
    syntax_err   = "x <- function( {"
  )

  for (nm in names(evil_scripts)) {
    v <- verify_compiled_r_ast(evil_scripts[[nm]], strict = FALSE)
    expect_false(v$safe, info = paste("AST verifier failed to block:", nm))
    expect_true(nrow(v$violations) >= 1L)
  }

  expect_error(verify_compiled_r_ast("system('id')", strict = TRUE), class = "specshiny_ast_violation")
  expect_no_error(suppressMessages(print(verify_compiled_r_ast("x <- 1 + 1"))))
  expect_no_error(suppressMessages(print(verify_compiled_r_ast("system('id')"))))
})

test_that("LLM backends mask API keys, redact secrets in errors, validate URLs, and enforce response byte caps", {
  secret <- "sk-supersecret-9876543210-ABCD"
  tr <- translator_openai(model = "gpt-4o-mini", api_key = secret, max_response_bytes = 2048L)

  # Secret never appears in print(), str(), or object fields
  printed <- paste(capture.output(print(tr), type = "message"), collapse = "\n")
  str_out <- paste(capture.output(str(tr)), collapse = "\n")
  expect_false(grepl(secret, printed, fixed = TRUE))
  expect_false(grepl(secret, str_out, fixed = TRUE))
  expect_equal(tr$api_key_masked, "sk-...ABCD")

  # URL validation rejects non-HTTPS, file://, embedded credentials, and control chars
  expect_error(translator_openai(api_key = secret, base_url = "http://evil.example.com/v1"), class = "specshiny_security_error")
  expect_error(translator_openai(api_key = secret, base_url = "file:///etc/passwd"), class = "specshiny_security_error")
  expect_error(translator_openai(api_key = secret, base_url = "https://user:pass@api.openai.com/v1"), class = "specshiny_security_error")
  expect_error(translator_ollama(base_url = "http://remote.example.com:11434"), class = "specshiny_security_error")
  expect_error(translator_ollama(base_url = "http://localhost:11434\nX-Injected: 1"), class = "specshiny_security_error")
  expect_error(translator_ollama(timeout = 9999), class = "specshiny_security_error")
  expect_error(translator_ollama(max_response_bytes = 100), class = "specshiny_security_error")

  # Oversized mocked HTTP response rejected BEFORE JSON parsing
  big_payload <- paste0("{\"message\":{\"content\":\"", paste(rep("A", 3000), collapse = ""), "\"}}")
  oll <- translator_ollama(model = "llama3.1", max_response_bytes = 2048L)
  expect_error(
    httr2::with_mocked_responses(
      function(req) httr2::response(status_code = 200L, body = charToRaw(big_payload)),
      nl_to_spec("Simple study", translator = oll)
    ),
    class = "specshiny_security_error"
  )

  # Error containing secret key is redacted
  expect_error(
    httr2::with_mocked_responses(
      function(req) stop(paste("Auth failed for Bearer", secret)),
      nl_to_spec("Simple study", translator = tr)
    ),
    regexp = "\\[REDACTED\\]"
  )
  err_caught <- tryCatch(
    httr2::with_mocked_responses(
      function(req) stop(paste("Auth failed for", secret)),
      nl_to_spec("Simple study", translator = tr)
    ),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl(secret, err_caught, fixed = TRUE))
})

test_that("mod_spec_controls_server survives NA, Inf, NaN, and rapid invalid edits without crashing or stale errors", {
  init <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2)
  )
  spec_rv <- shiny::reactiveVal(init)
  diff_rv <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_spec_controls_server,
    args = list(spec_rv = spec_rv, last_diff_rv = diff_rv),
    {
      # Rapid NA, NaN, Inf, -Inf, out-of-range edits must NEVER crash
      session$setInputs(n_clusters = NA)
      expect_equal(spec_rv()$hierarchy$n_clusters, 40L)
      expect_false(session$returned$guardrail_error()$valid)
      expect_null(diff_rv())

      session$setInputs(icc = Inf)
      expect_equal(spec_rv()$hierarchy$icc, 0.2)
      expect_false(session$returned$guardrail_error()$valid)

      session$setInputs(residual_sd = -5)
      expect_equal(spec_rv()$outcome$residual_sd, 1.0)
      expect_false(session$returned$guardrail_error()$valid)

      session$setInputs(n_clusters = NaN)
      expect_equal(spec_rv()$hierarchy$n_clusters, 40L)

      # Returning to a valid value clears the guardrail error cleanly
      session$setInputs(n_clusters = 55)
      expect_equal(spec_rv()$hierarchy$n_clusters, 55L)
      expect_null(session$returned$guardrail_error())
      expect_true(diff_rv()$has_changes)

      # External IR update also clears any active guardrail error
      session$setInputs(icc = 1.5)
      expect_false(session$returned$guardrail_error()$valid)
      spec_rv(refine_spec(spec_rv(), "set ICC to 0.25")$spec)
      session$flushReact()
      expect_null(session$returned$guardrail_error())
      expect_equal(spec_rv()$hierarchy$icc, 0.25)
    }
  )
})

test_that("compiled R scripts empirically recover predictor correlation, negative betas, MAR dependence, ICC, random slopes, and L'Ecuyer-CMRG reproducibility", {
  # 1. Exact predictor correlation matrix + negative/positive fixed effects + L'Ecuyer-CMRG reproducibility
  R_target <- matrix(c(1, 0.6, -0.4, 0.6, 1, 0.2, -0.4, 0.2, 1), 3, 3)
  s_corr <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 5000L),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "focal"),
      spec_predictor("x2", level = 1L, distribution = "normal", mean = 0, sd = 1),
      spec_predictor("x3", level = 1L, distribution = "normal", mean = 0, sd = 1)
    ),
    predictor_corr = R_target,
    outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1.0),
    effects = list(intercept = 0.5, fixed = list(x1 = -0.5, x2 = 0.4, x3 = 0.3), focal_predictor = "x1"),
    simulation = list(n_reps = 2L, seed = 2026L)
  )

  p_r <- tempfile(fileext = ".R")
  on.exit(unlink(p_r), add = TRUE)
  compile_r_script(s_corr, file = p_r, run_on_source = FALSE)

  env1 <- new.env(parent = globalenv())
  env2 <- new.env(parent = globalenv())
  sys.source(p_r, envir = env1)
  sys.source(p_r, envir = env2)

  # Bit-for-bit reproducible L'Ecuyer-CMRG runs
  res1 <- env1$run_simulation(env1$cfg)
  expect_equal(RNGkind()[1L], "L'Ecuyer-CMRG")
  res2 <- env2$run_simulation(env2$cfg)
  expect_identical(res1$raw, res2$raw)

  # Verify Cholesky predictor correlation matrix recovery
  set.seed(123, kind = "L'Ecuyer-CMRG")
  dat_big <- env1$simulate_one_dataset(env1$cfg)
  emp_R <- stats::cor(dat_big[, c("x1", "x2", "x3")])
  expect_lt(max(abs(emp_R - R_target)), 0.05)

  # Verify OLS fixed-effect recovery including negative focal beta
  fit_ols <- stats::lm(y ~ x1 + x2 + x3, data = dat_big)
  expect_equal(unname(stats::coef(fit_ols)["x1"]), -0.5, tolerance = 0.08)
  expect_equal(unname(stats::coef(fit_ols)["x2"]), 0.4, tolerance = 0.08)
  expect_equal(unname(stats::coef(fit_ols)["x3"]), 0.3, tolerance = 0.08)

  # 2. Genuine MAR missingness recovery (dependent on auxiliary x1, NOT MCAR)
  s_mar <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 4000L),
    predictors = list(spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "focal")),
    missingness = list(mechanism = "MAR", rate = 0.25, target = "outcome", auxiliary = "x1", strength = 1.8),
    simulation = list(n_reps = 1L, seed = 77L)
  )
  p_mar <- tempfile(fileext = ".R")
  on.exit(unlink(p_mar), add = TRUE)
  compile_r_script(s_mar, file = p_mar, run_on_source = FALSE)
  env_mar <- new.env(parent = globalenv())
  sys.source(p_mar, envir = env_mar)
  set.seed(77, kind = "L'Ecuyer-CMRG")
  dat_mar <- env_mar$simulate_one_dataset(env_mar$cfg)

  emp_miss_rate <- mean(is.na(dat_mar$y))
  expect_equal(emp_miss_rate, 0.25, tolerance = 0.03)
  mar_glm <- stats::glm(is.na(y) ~ scale(x1), data = dat_mar, family = stats::binomial())
  mar_slope <- unname(stats::coef(mar_glm)[2L])
  mar_pval <- stats::coef(summary(mar_glm))[2L, 4L]
  expect_equal(mar_slope, 1.8, tolerance = 0.25)
  expect_lt(mar_pval, 1e-10)

  # 3. 2-level ICC, random slope SD, and intercept-slope correlation recovery
  skip_if_not_installed("lme4")
  s_mlm <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 500L, cluster_size = 25L, cluster_name = "school", unit_name = "student", icc = 0.25),
    predictors = list(spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1, role = "focal")),
    outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1.0),
    effects = list(
      intercept = 0,
      fixed = list(x1 = 0.45),
      focal_predictor = "x1",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "x1", sd = 0.40)),
        corr_matrix = matrix(c(1, 0.50, 0.50, 1), 2, 2)
      )
    ),
    simulation = list(n_reps = 1L, seed = 88L)
  )
  p_mlm <- tempfile(fileext = ".R")
  on.exit(unlink(p_mlm), add = TRUE)
  compile_r_script(s_mlm, file = p_mlm, run_on_source = FALSE)
  env_mlm <- new.env(parent = globalenv())
  sys.source(p_mlm, envir = env_mlm)
  set.seed(88, kind = "L'Ecuyer-CMRG")
  dat_mlm <- env_mlm$simulate_one_dataset(env_mlm$cfg)

  fit_mlm <- suppressMessages(lme4::lmer(y ~ x1 + (1 + x1 | school), data = dat_mlm))
  vc <- as.data.frame(lme4::VarCorr(fit_mlm))
  tau0_hat <- vc$sdcor[which(vc$grp == "school" & vc$var1 == "(Intercept)" & is.na(vc$var2))]
  tau1_hat <- vc$sdcor[which(vc$grp == "school" & vc$var1 == "x1" & is.na(vc$var2))]
  rho_hat  <- vc$sdcor[which(vc$grp == "school" & vc$var1 == "(Intercept)" & !is.na(vc$var2) & vc$var2 == "x1")]
  sig_hat  <- vc$sdcor[which(vc$grp == "Residual")]
  icc_hat  <- (tau0_hat^2) / (tau0_hat^2 + sig_hat^2)

  expect_lt(abs(icc_hat - 0.25), 0.04)
  expect_lt(abs(tau1_hat - 0.40), 0.06)
  expect_lt(abs(rho_hat - 0.50), 0.12)
  expect_lt(abs(unname(lme4::fixef(fit_mlm)["x1"]) - 0.45), 0.08)
})

test_that("expanded translator_rule and refine_spec parse negative effects, percentages, correlated predictors, and structural switches", {
  tr <- nl_to_spec(
    "2-level power study with 50 schools, 20 students per school, ICC 20%, negative treatment effect 0.35, predictors x1 and x2 with predictor correlation 0.4, 0.15 MAR missingness, alpha 1%, 300 reps"
  )
  expect_true(tr$validation$valid)
  expect_equal(tr$spec$hierarchy$icc, 0.20)
  expect_equal(tr$spec$effects$fixed$treatment, -0.35)
  expect_equal(tr$spec$missingness$rate, 0.15)
  expect_equal(tr$spec$simulation$alpha, 0.01)
  expect_true(is.matrix(tr$spec$predictor_corr))
  expect_equal(tr$spec$predictor_corr[1, 2], 0.4)

  # Refine: remove predictor x2, switch missingness to MCAR, switch to 1-level, switch to glm
  r1 <- refine_spec(tr$spec, "drop predictor x2 and switch to MCAR")
  expect_true(r1$validation$valid)
  expect_false("x2" %in% vapply(r1$spec$predictors, `[[`, character(1), "name"))
  expect_equal(r1$spec$missingness$mechanism, "MCAR")

  r2 <- refine_spec(r1$spec, "switch to 1-level and use glm")
  expect_true(r2$validation$valid)
  expect_equal(r2$spec$hierarchy$levels, 1L)
  expect_equal(r2$spec$estimator$engine, "glm")

  r3 <- refine_spec(r2$spec, "switch to binomial and use probit link")
  expect_true(r3$validation$valid)
  expect_equal(r3$spec$outcome$family, "binomial")
  expect_equal(r3$spec$outcome$link, "probit")
})
