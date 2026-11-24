# ==============================================================================
# Peer Red-Team Remediation Regression Suite (v0.5.0 — All 17 Findings)
# Covers CRIT-01..03, HIGH-01..04, MED-01..06, LOW-01..04
# ==============================================================================

test_that("CRIT-01: AST verifier walks all extra arguments of library() and requireNamespace()", {
  canary <- tempfile("crit01_")
  on.exit(unlink(canary), add = TRUE)

  attacks <- list(
    reqns_quietly = sprintf('requireNamespace("lme4", quietly = { file.create("%s"); TRUE })', canary),
    lib_libloc    = 'library(lme4, lib.loc = { system("id"); NULL })',
    lib_named_pkg = 'library(quietly = { unlink("/tmp/x"); TRUE }, package = lme4)',
    lib_charonly  = 'library("lme4", character.only = { eval(parse(text = "1")); TRUE })',
    lib_dyn_pkg   = 'library({ system("id"); "lme4" })',
    lib_partial   = 'library(p = { system("id"); "lme4" })'
  )

  for (nm in names(attacks)) {
    v <- verify_compiled_r_ast(attacks[[nm]], strict = FALSE)
    expect_false(v$safe, info = paste("CRIT-01 attack bypassed AST verifier:", nm))
    expect_gt(nrow(v$violations), 0L)
  }
  expect_false(file.exists(canary))

  # Legitimate allowlisted forms remain safe
  expect_true(verify_compiled_r_ast('requireNamespace("lme4", quietly = TRUE)')$safe)
  expect_true(verify_compiled_r_ast('library(lme4)')$safe)
})

test_that("CRIT-02: AST verifier blocks hoisted local_funs, higher-order bypasses, string callables, and RNG primitive abuse", {
  attacks <- list(
    hoist_getfromns = paste(
      'f <- getFromNamespace("system", "base")',
      'lapply("id", f)',
      'getFromNamespace <- function(...) NULL',
      sep = "\n"
    ),
    shadow_sysfn    = 'getFromNamespace <- function(...) NULL; getFromNamespace("system", "base")',
    ho_sys_getenv   = 'lapply("HOME", Sys.getenv)',
    ho_save_image   = 'lapply("/tmp/out.RData", save.image)',
    ho_read_delim   = 'lapply("/etc/hosts", read.delim)',
    ho_list_files   = 'lapply(".", list.files)',
    ho_vapply_env   = 'vapply("HOME", Sys.getenv, character(1))',
    ho_trycatch_env = 'tryCatch(stop("x"), error = Sys.getenv)',
    ho_string_sys   = 'lapply("id", "system")',
    ho_named_x_str  = 'lapply(X = "id", "system")',
    ho_partial_fun  = 'lapply("id", FU = "system")',
    ho_trycatch_str = 'tryCatch(stop("x"), error = "system")',
    ho_alias_var    = 'f <- Sys.getenv; lapply("HOME", f)',
    ho_uniroot_str  = 'stats::uniroot("system", c(0, 1))',
    ho_ave_str      = 'stats::ave(1:4, c(1, 1, 2, 2), FUN = "system")',
    ho_reverse_def  = 'lapply(1:3, helper_fn); helper_fn <- function(z) z + 1',
    bad_exists      = 'exists("x")',
    bad_global_ref  = '.GlobalEnv$evil <- 1',
    bad_superassign = 'x <<- 1',
    bad_onexit_body = 'f <- function() { on.exit(system("id")); 1 }',
    bad_s4_slot     = 'x <- list(); y <- x@evil'
  )

  for (nm in names(attacks)) {
    v <- verify_compiled_r_ast(attacks[[nm]], strict = FALSE)
    expect_false(v$safe, info = paste("CRIT-02 attack bypassed AST verifier:", nm))
    expect_gt(nrow(v$violations), 0L)
  }

  # Legitimate lexical-order local function & safe higher-order calls pass
  good <- paste(
    'my_add <- function(z) z + 1',
    'res <- lapply(1:3, my_add)',
    'm <- stats::ave(c(1, 2, 3, 4), c(1, 1, 2, 2), FUN = mean)',
    sep = "\n"
  )
  expect_true(verify_compiled_r_ast(good)$safe)
})

test_that("CRIT-03: zero cli/glue RCE across all print and abort sinks", {
  canary <- tempfile("crit03_canary_")
  on.exit(unlink(canary), add = TRUE)
  payload <- sprintf('{file.create("%s")}', gsub("\\\\", "/", canary))

  # 1 & 2. abort_spec_validation() and print.spec_validation()
  bad <- new_spec_ir()
  bad$predictors[[1L]]$name <- payload
  expect_error(validate_spec_ir(bad, strict = TRUE), class = "specshiny_validation_error")
  expect_false(file.exists(canary))
  expect_no_error(suppressMessages(capture.output(print(validate_spec_ir(bad, strict = FALSE)))))
  expect_false(file.exists(canary))

  # 3. print.spec_diff()
  s1 <- new_spec_ir(title = "Clean baseline")
  s2 <- new_spec_ir(title = payload)
  d <- diff_spec_ir(s1, s2)
  expect_no_error(suppressMessages(capture.output(print(d))))
  expect_false(file.exists(canary))

  # 4. print.spec_comparison() with payload in both baseline and non-baseline names
  cmp_args <- stats::setNames(list(s1, s2), c(payload, paste0("alt_", payload)))
  cmp <- compare_specs(cmp_args)
  expect_no_error(suppressMessages(capture.output(print(cmp))))
  expect_false(file.exists(canary))

  # 5. print.spec_bundle_verification() with malformed JSON containing glue payload
  bdir <- tempfile("crit03_bundle_")
  on.exit(unlink(bdir, recursive = TRUE), add = TRUE)
  dir.create(bdir)
  writeLines(payload, file.path(bdir, "bundle_manifest.json"))
  bv <- verify_spec_bundle(bdir)
  expect_false(bv$valid)
  expect_no_error(suppressMessages(capture.output(print(bv))))
  expect_false(file.exists(canary))

  # 6. print.spec_translation() with payload in clarifications
  tr <- nl_to_spec("2-level study with 30 schools")
  tr$clarifications <- list(list(
    id = payload,
    path = "hierarchy.icc",
    question = paste("Clarify", payload),
    current_default = payload,
    options = list(),
    rationale = payload
  ))
  expect_no_error(suppressMessages(capture.output(print(tr))))
  expect_false(file.exists(canary))

  # 7. print.spec_ir(), print.summary.spec_ir(), print.spec_ui_descriptor()
  expect_no_error(suppressMessages(capture.output(print(s2))))
  expect_no_error(suppressMessages(capture.output(print(summary(s2)))))
  desc <- spec_to_controls(s2)
  desc$sections[[1L]]$title <- payload
  expect_no_error(suppressMessages(capture.output(print(desc))))
  expect_false(file.exists(canary))
})

test_that("HIGH-01: export_spec_bundle and verify_spec_bundle reject symlinks and escape targets", {
  s <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 80L), simulation = list(n_reps = 3L, seed = 1L))
  victim <- tempfile("high01_victim_")
  writeLines("ORIGINAL SECRET DATA", victim)
  bdir_live <- tempfile("high01_live_")
  bdir_dang <- tempfile("high01_dang_")
  bdir_tamp <- tempfile("high01_tamp_")
  bdir_man  <- tempfile("high01_man_")
  sym_root  <- tempfile("high01_symroot_")
  real_root <- tempfile("high01_realroot_")
  dir.create(bdir_live)
  dir.create(bdir_dang)
  dir.create(bdir_tamp)
  dir.create(bdir_man)
  dir.create(real_root)
  on.exit(
    unlink(c(victim, bdir_live, bdir_dang, bdir_tamp, bdir_man, sym_root, real_root), recursive = TRUE, force = TRUE),
    add = TRUE
  )

  make_symlink <- function(from, to) {
    isTRUE(suppressWarnings(file.symlink(from, to)))
  }

  # 1. Pre-existing live symlink inside bundle dir must fail closed and leave victim intact
  ok_live <- make_symlink(victim, file.path(bdir_live, "spec.yaml"))
  skip_if_not(ok_live, "Symbolic links cannot be created in this environment.")
  expect_error(
    export_spec_bundle(s, bdir_live, overwrite = TRUE),
    class = "specshiny_security_error"
  )
  expect_identical(readLines(victim), "ORIGINAL SECRET DATA")

  # 2. Dangling symlink must fail closed even with overwrite = FALSE
  dangling_target <- file.path(tempdir(), "nonexistent_high01_outside.qmd")
  if (make_symlink(dangling_target, file.path(bdir_dang, "report.qmd"))) {
    expect_error(
      export_spec_bundle(s, bdir_dang, overwrite = FALSE),
      class = "specshiny_security_error"
    )
    expect_false(file.exists(dangling_target))
  }

  # 3. Symlinked bundle root directory is rejected by both export and verify
  if (make_symlink(real_root, sym_root)) {
    expect_error(export_spec_bundle(s, sym_root, overwrite = TRUE), class = "specshiny_security_error")
    expect_error(verify_spec_bundle(sym_root), class = "specshiny_security_error")
  }

  # 4. Valid bundle tampered with a symlinked artifact fails verification
  export_spec_bundle(s, bdir_tamp, overwrite = TRUE)
  expect_true(verify_spec_bundle(bdir_tamp)$valid)
  unlink(file.path(bdir_tamp, "spec.yaml"), force = TRUE)
  if (make_symlink(victim, file.path(bdir_tamp, "spec.yaml"))) {
    v_tampered <- verify_spec_bundle(bdir_tamp)
    expect_false(v_tampered$valid)
    expect_true(any(grepl("symbolic link|outside", v_tampered$issues, ignore.case = TRUE)))
  }

  # 5. Symlinked bundle_manifest.json fails verification
  export_spec_bundle(s, bdir_man, overwrite = TRUE)
  unlink(file.path(bdir_man, "bundle_manifest.json"), force = TRUE)
  if (make_symlink(victim, file.path(bdir_man, "bundle_manifest.json"))) {
    v_man <- verify_spec_bundle(bdir_man)
    expect_false(v_man$valid)
  }

  # 6. Nested non-existing bundle directory & valid overwrite = TRUE succeed cleanly
  nested_dir <- file.path(tempfile("high01_nested_"), "sub", "bundle")
  on.exit(unlink(dirname(dirname(nested_dir)), recursive = TRUE, force = TRUE), add = TRUE)
  export_spec_bundle(s, nested_dir, overwrite = FALSE)
  expect_true(verify_spec_bundle(nested_dir)$valid)
  expect_no_error(export_spec_bundle(s, nested_dir, overwrite = TRUE))
  expect_true(verify_spec_bundle(nested_dir)$valid)
})

test_that("HIGH-02: spec_total_n avoids 32-bit overflow and validate_spec_ir enforces upper sample-size bounds", {
  sp_big <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 100000L, cluster_size = 100000L, icc = 0.2),
    validate = FALSE
  )
  # Validation strictly rejects excessive total N
  v <- validate_spec_ir(sp_big, strict = FALSE)
  expect_false(v$valid)
  expect_true("total_n_exceeds_limit" %in% v$errors$code)

  # spec_total_n computes exact finite double without warning or NA_integer_ overflow
  tot <- expect_no_warning(spec_total_n(sp_big))
  expect_true(is.finite(tot))
  expect_equal(tot, 1e10)

  # Single-level excessive n_obs and grid upper bounds are rejected
  sp_1l <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 20000000L), validate = FALSE)
  expect_true("sample_size_exceeds_limit" %in% validate_spec_ir(sp_1l)$errors$code)

  sp_grid <- new_spec_ir(
    hierarchy = list(levels = 1L, n_obs = 100L),
    simulation = list(n_reps = 5L, seed = 1L, parameter_grid = list(n_obs = c(100, 5e7))),
    validate = FALSE
  )
  expect_true("grid_value_out_of_bounds" %in% validate_spec_ir(sp_grid)$errors$code)
})

test_that("HIGH-03: OpenAI API key is never serialized to RDS; CRLF, bad temperature, metadata IPs, and bad ports are blocked", {
  secret <- "sk-proj-SUPERSECRETKEY1234567890"
  tr <- translator_openai(model = "gpt-4o-mini", api_key = secret)

  # 1. Zero cleartext bytes of secret in saveRDS(..., compress = FALSE)
  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)
  saveRDS(tr, f, compress = FALSE)
  raw_b <- readBin(f, "raw", file.info(f)$size)
  key_b <- charToRaw(secret)
  found <- length(raw_b) >= length(key_b) && any(vapply(
    seq_len(length(raw_b) - length(key_b) + 1L),
    function(i) identical(raw_b[i:(i + length(key_b) - 1L)], key_b),
    logical(1)
  ))
  expect_false(found)
  tr_reloaded <- readRDS(f)
  expect_null(tr_reloaded[[".auth"]])
  expect_false(identical(tr_reloaded[[".auth_id"]], secret))

  # In-session secret lookup still authenticates cleanly
  expect_identical(get_openai_api_key(tr), secret)

  # 2. CRLF/control chars in api_key and model rejected
  expect_error(
    translator_openai(model = "gpt-4o\r\nX-Evil: 1", api_key = secret),
    class = "specshiny_security_error"
  )
  expect_error(
    translator_openai(model = "gpt-4o-mini", api_key = "sk-abc123456789\r\nX-Inj: 1"),
    class = "specshiny_security_error"
  )
  expect_error(
    translator_ollama(model = "llama3\nX-Bad: 1"),
    class = "specshiny_security_error"
  )

  # 3. Invalid temperature strictly rejected with zero base warnings
  expect_error(translator_openai(api_key = secret, temperature = "bad"), class = "specshiny_security_error")
  expect_error(translator_openai(api_key = secret, temperature = -0.1), class = "specshiny_security_error")
  expect_error(translator_openai(api_key = secret, temperature = 2.5), class = "specshiny_security_error")
  expect_error(translator_ollama(temperature = NA_real_), class = "specshiny_security_error")

  # 4. SSRF cloud metadata / link-local hosts and invalid TCP ports rejected
  bad_urls <- c(
    "https://169.254.169.254/latest/meta-data",
    "https://metadata.google.internal/computeMetadata/v1",
    "https://metadata.goog/v1",
    "https://0.0.0.0/v1",
    "https://[fe80::1]/v1",
    "https://[fd00:ec2::254]/latest",
    "https://2852039166/latest",
    "https://api.openai.com:99999/v1",
    "https://api.openai.com:0/v1"
  )
  for (u in bad_urls) {
    expect_error(
      translator_openai(api_key = secret, base_url = u),
      class = "specshiny_security_error",
      info = paste("URL not rejected:", u)
    )
  }
  expect_error(translator_ollama(base_url = "http://localhost:99999"), class = "specshiny_security_error")
  expect_error(translator_ollama(base_url = "http://localhost:0"), class = "specshiny_security_error")
})

test_that("HIGH-04: parameter_grid ICC sweeps dynamically recompute tau0 even when intercept_sd is explicitly set", {
  sp <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 80L, cluster_size = 15L, icc = 0.2),
    effects = list(
      intercept = 0,
      fixed = list(x1 = 0.3),
      focal_predictor = "x1",
      random = list(intercept = TRUE, intercept_sd = 0.5, slopes = list())
    ),
    simulation = list(n_reps = 2L, seed = 42L, parameter_grid = list(icc = c(0.01, 0.20, 0.80)))
  )
  expect_true(is_valid_spec_ir(sp))
  code <- compile_r_script(sp, run_on_source = FALSE)
  e <- new.env(parent = globalenv())
  eval(parse(text = code), envir = e)

  set.seed(101L)
  p1 <- e$cfg; p1$icc <- 0.01
  d1 <- e$simulate_one_dataset(p1)
  set.seed(101L)
  p2 <- e$cfg; p2$icc <- 0.80
  d2 <- e$simulate_one_dataset(p2)

  expect_false(isTRUE(all.equal(d1$y, d2$y)))
  # Empirical total variance must be substantially larger at ICC = 0.80 than ICC = 0.01
  expect_gt(stats::var(d2$y), 2.5 * stats::var(d1$y))
})

test_that("MED-01: compiled lmer/glmer scripts and preview runner flag singular fits as non-converged", {
  skip_if_not_installed("lme4")
  sp <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 15L, cluster_size = 10L, icc = 0.2),
    simulation = list(n_reps = 2L, seed = 7L)
  )
  code <- compile_r_script(sp, run_on_source = FALSE)
  expect_match(code, "lme4::isSingular(fit)", fixed = TRUE)
  expect_match(code, "fit@optinfo$conv", fixed = TRUE)
  expect_true(verify_compiled_r_ast(code)$safe)

  e <- new.env(parent = globalenv())
  eval(parse(text = code), envir = e)

  # Construct deterministic zero-between-cluster-variance data that guarantees singular lmer fit
  set.seed(123L)
  dat_sing <- data.frame(
    cluster = factor(rep(seq_len(6L), each = 8L)),
    x1 = stats::rnorm(48L),
    y = stats::rnorm(48L)
  )
  # Remove cluster means from y so between-cluster variance is identically 0 -> singular!
  dat_sing$y <- dat_sing$y - stats::ave(dat_sing$y, dat_sing$cluster, FUN = mean)
  fit_res <- e$fit_and_extract(dat_sing, e$cfg)
  expect_false(fit_res$converged)
})

test_that("MED-02: preview runner and compiled script calibrate auxiliary monotone_dropout identically", {
  skip_if_not_installed("lme4")
  sp <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(levels = 2L, n_clusters = 120L, time_points = 5L, cluster_size = 5L, icc = 0.3),
    predictors = list(
      spec_predictor("time", level = 1L, distribution = "time", role = "focal"),
      spec_predictor("treatment", level = 2L, distribution = "binary", prob = 0.5, role = "treatment")
    ),
    effects = list(
      intercept = 0,
      fixed = list(time = 0.3, treatment = 0.2),
      focal_predictor = "time",
      random = list(intercept = TRUE, slopes = list(list(predictor = "time", sd = 0.2)), corr_matrix = diag(2))
    ),
    missingness = list(mechanism = "monotone_dropout", rate = 0.35, target = "outcome", auxiliary = "treatment", strength = 2.0),
    simulation = list(n_reps = 3L, seed = 42L)
  )
  code <- compile_r_script(sp, run_on_source = FALSE)
  e <- new.env(parent = globalenv())
  eval(parse(text = code), envir = e)
  set.seed(42L)
  dat_c <- e$simulate_one_dataset(e$cfg)
  w5_miss_compiled <- mean(is.na(dat_c$y[seq(5L, nrow(dat_c), by = 5L)]))
  expect_equal(w5_miss_compiled, 0.35, tolerance = 0.10)

  # Same seed and unscaled N: compiled run_simulation() and run_spec_preview_sim() must agree identically!
  comp_out <- e$run_simulation(e$cfg)
  prev_out <- run_spec_preview_sim(sp, max_reps = 3L, max_n = 2000L, max_seconds = 15)
  expect_equal(prev_out$summary$n_converged, comp_out$summary$n_converged)
  expect_equal(prev_out$summary$mean_est, comp_out$summary$mean_est)
  expect_equal(prev_out$summary$mean_se, comp_out$summary$mean_se)
  expect_equal(prev_out$summary$power, comp_out$summary$power)
})

test_that("MED-03: 2-level specs with zero random effects reject lmer/glmer engines", {
  sp_gauss <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 20L, cluster_size = 10L, icc = 0),
    effects = list(
      intercept = 0, fixed = list(x1 = 0.3), focal_predictor = "x1",
      random = list(intercept = FALSE, slopes = list())
    ),
    estimator = list(engine = "lmer", reml = TRUE, conf_level = 0.95, missing_handling = "complete_case"),
    validate = FALSE
  )
  v_g <- validate_spec_ir(sp_gauss)
  expect_false(v_g$valid)
  expect_true("mixed_engine_requires_random_effects" %in% v_g$errors$code)

  sp_bin <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 20L, cluster_size = 10L, icc = 0),
    outcome = list(name = "y", family = "binomial", link = "logit"),
    effects = list(
      intercept = 0, fixed = list(x1 = 0.3), focal_predictor = "x1",
      random = list(intercept = FALSE, slopes = list())
    ),
    estimator = list(engine = "glmer", reml = FALSE, conf_level = 0.95, missing_handling = "complete_case"),
    validate = FALSE
  )
  v_b <- validate_spec_ir(sp_bin)
  expect_false(v_b$valid)
  expect_true("mixed_engine_requires_random_effects" %in% v_b$errors$code)

  # Switching to lm / glm makes them valid and compilable
  sp_gauss$estimator$engine <- "lm"
  expect_true(is_valid_spec_ir(sp_gauss))
  expect_true(verify_compiled_r_ast(compile_r_script(sp_gauss))$safe)
})

test_that("MED-04: non-zero predictor_corr involving deterministic time predictor is rejected", {
  sp <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(levels = 2L, n_clusters = 40L, time_points = 5L, cluster_size = 5L, icc = 0.3),
    predictors = list(
      spec_predictor("time", level = 1L, distribution = "time", role = "focal"),
      spec_predictor("x1", level = 1L, distribution = "normal", mean = 0, sd = 1)
    ),
    predictor_corr = matrix(c(1, 0.7, 0.7, 1), 2, 2),
    effects = list(
      intercept = 0, fixed = list(time = 0.3, x1 = 0.2), focal_predictor = "time",
      random = list(intercept = TRUE, slopes = list(list(predictor = "time", sd = 0.2)), corr_matrix = diag(2))
    ),
    validate = FALSE
  )
  v <- validate_spec_ir(sp)
  expect_false(v$valid)
  expect_true("time_predictor_correlation_forbidden" %in% v$errors$code)
})

test_that("MED-05: normalize_clarifications handles malformed LLM clarification JSON cleanly", {
  oll <- translator_ollama(model = "llama3.1")
  valid_spec_json <- as_spec_json(new_spec_ir())

  malformed_clarifs <- c(
    '[1, 2, 3]',
    '"oops"',
    '{"a": 1}',
    '[null, 42, {"id": "c1", "path": "hierarchy.n_obs", "question": "How many?"}]'
  )

  for (cl_json in malformed_clarifs) {
    inner <- sprintf('{"spec": %s, "clarifications": %s}', valid_spec_json, cl_json)
    envelope <- as.character(jsonlite::toJSON(list(message = list(content = inner)), auto_unbox = TRUE))
    res <- httr2::with_mocked_responses(
      function(req) httr2::response(status_code = 200L, body = charToRaw(envelope)),
      nl_to_spec("Simple study", translator = oll)
    )
    expect_s3_class(res, "spec_translation")
    expect_true(res$validation$valid)
    expect_true(is.list(res$clarifications))
  }
})

test_that("MED-06: refine_spec clears ar1_rho when switching longitudinal models to binomial/poisson", {
  sp <- use_spec_template("longitudinal_growth_ar1")
  sp$simulation$parameter_grid <- list(ar1_rho = c(0.1, 0.4))

  r_bin <- refine_spec(sp, "switch to binomial")
  expect_true(r_bin$validation$valid)
  expect_equal(r_bin$spec$outcome$family, "binomial")
  expect_equal(r_bin$spec$estimator$engine, "glmer")
  expect_null(r_bin$spec$outcome$ar1_rho)
  expect_null(r_bin$spec$simulation$parameter_grid$ar1_rho)

  r_pois <- refine_spec(sp, "switch to poisson")
  expect_true(r_pois$validation$valid)
  expect_equal(r_pois$spec$outcome$family, "poisson")
  expect_equal(r_pois$spec$estimator$engine, "glmer")
  expect_null(r_pois$spec$outcome$ar1_rho)

  r_back <- refine_spec(r_bin$spec, "switch to gaussian")
  expect_true(r_back$validation$valid)
  expect_equal(r_back$spec$outcome$family, "gaussian")
  expect_equal(r_back$spec$estimator$engine, "lmer")
  expect_equal(r_back$spec$outcome$ar1_rho, 0.30)
})

test_that("LOW-01: compiled run_simulation() restores caller RNGkind() and .Random.seed in both global and base parent envs", {
  sp <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 50L), simulation = list(n_reps = 2L, seed = 42L))
  code <- compile_r_script(sp, run_on_source = FALSE)

  for (parent_env in list(globalenv(), baseenv())) {
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(2026L)
    kind_before <- RNGkind()
    seed_before <- .GlobalEnv$.Random.seed

    e <- new.env(parent = parent_env)
    eval(parse(text = code), envir = e)
    res <- e$run_simulation(e$cfg)

    expect_equal(nrow(res$summary), 1L)
    expect_identical(RNGkind(), kind_before)
    expect_identical(.GlobalEnv$.Random.seed, seed_before)
  }
})

test_that("LOW-02: compare_specs accurately accounts for left-censored normal cluster sizes and validator warns", {
  s_bal <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 5L, icc = 0.2)
  )
  s_unb <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(
      levels = 2L, n_clusters = 40L, cluster_size = 5L,
      cluster_size_dist = "normal", cluster_size_sd = 20, icc = 0.2
    )
  )
  v_unb <- validate_spec_ir(s_unb)
  expect_true(v_unb$valid)
  expect_true("cluster_size_left_censoring" %in% v_unb$warnings$code)

  cmp <- compare_specs(bal = s_bal, unb = s_unb)
  e <- new.env(parent = globalenv())
  eval(parse(text = compile_r_script(s_unb, run_on_source = FALSE)), envir = e)
  set.seed(1L)
  sim_mean_n <- mean(replicate(150L, nrow(e$simulate_one_dataset(e$cfg))))
  # Analytical left-censored total_n (~463) matches simulated mean N (~462) within 5%
  expect_lt(abs(cmp$summary$total_n[[2L]] - sim_mean_n) / sim_mean_n, 0.05)
})

test_that("LOW-03: run_spec_preview_sim never scales longitudinal n_clusters UP above spec value", {
  sp <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(levels = 2L, n_clusters = 3L, time_points = 100L, cluster_size = 100L, icc = 0.3),
    simulation = list(n_reps = 1L, seed = 1L)
  )
  expect_true(is_valid_spec_ir(sp))
  expect_equal(spec_total_n(sp), 300L)

  # With max_n = 200 (< 300), n_clusters = 3 must NOT be scaled UP to q_min = 4 (400 obs)
  out <- run_spec_preview_sim(sp, max_reps = 1L, max_n = 200L, max_seconds = 10)
  expect_true(any(grepl("retained minimum identifiable", out$warnings, ignore.case = TRUE)))
  expect_false(any(grepl("scaled down", out$warnings, ignore.case = TRUE)))
})

test_that("LOW-04: validate_spec_ir warns when negative ar1_rho is paired with uneven time_spacing", {
  sp_uneven_neg <- new_spec_ir(
    design_type = "longitudinal_growth",
    hierarchy = list(
      levels = 2L, n_clusters = 30L, time_points = 4L, cluster_size = 4L,
      time_spacing = c(0, 1, 3, 10), icc = 0.3
    ),
    outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1, ar1_rho = -0.7)
  )
  v1 <- validate_spec_ir(sp_uneven_neg)
  expect_true(v1$valid)
  expect_true("negative_ar1_uneven_spacing" %in% v1$warnings$code)

  # Positive ar1_rho with uneven spacing does NOT trigger this warning
  sp_uneven_pos <- sp_uneven_neg
  sp_uneven_pos$outcome$ar1_rho <- 0.7
  v2 <- validate_spec_ir(sp_uneven_pos)
  expect_true(v2$valid)
  expect_false("negative_ar1_uneven_spacing" %in% v2$warnings$code)

  # Negative ar1_rho with uniform spacing does NOT trigger this warning
  sp_even_neg <- sp_uneven_neg
  sp_even_neg$hierarchy$time_spacing <- c(0, 1, 2, 3)
  v3 <- validate_spec_ir(sp_even_neg)
  expect_true(v3$valid)
  expect_false("negative_ar1_uneven_spacing" %in% v3$warnings$code)
})
