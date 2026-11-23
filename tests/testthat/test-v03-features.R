# ==============================================================================
# v0.3.0 Capabilities: Interactions, Unbalanced Clusters, Non-Normal Errors,
# IR Migration, Undo/Redo History, Reproducible Bundles & Preview Caps
# ==============================================================================

test_that("interaction terms validate, synthesize controls, parse from NL, and recover true betas", {
  # Guardrails: self-interaction, unknown predictor, duplicate pair
  base2 <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 15L, icc = 0.2),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal"),
      spec_predictor("z1", level = 2L, distribution = "normal")
    ),
    effects = list(intercept = 0, fixed = list(x1 = 0.3, z1 = 0.2), focal_predictor = "x1")
  )

  s_self <- base2
  s_self$effects$interactions <- list(spec_interaction("x1", "x1", 0.2))
  expect_true("self_interaction_forbidden" %in% validate_spec_ir(s_self)$errors$code)

  s_unk <- base2
  s_unk$effects$interactions <- list(spec_interaction("x1", "nope", 0.2))
  expect_true("unknown_interaction_predictor" %in% validate_spec_ir(s_unk)$errors$code)

  s_dup <- base2
  s_dup$effects$interactions <- list(spec_interaction("x1", "z1", 0.2), spec_interaction("z1", "x1", 0.3))
  expect_true("duplicate_interaction" %in% validate_spec_ir(s_dup)$errors$code)

  # NL parsing of cross-level interaction + focal target
  tr <- nl_to_spec(
    "2-level power analysis for the cross-level interaction between x1 and z1 of 0.35 with 60 schools, 20 students per school, ICC 0.2, random slope for x1, 200 reps"
  )
  expect_true(tr$validation$valid)
  expect_equal(length(tr$spec$effects$interactions), 1L)
  expect_equal(tr$spec$effects$interactions[[1L]]$beta, 0.35)
  expect_equal(tr$spec$effects$focal_predictor, "x1:z1")
  expect_match(spec_to_math(tr$spec)$formula, "x1:z1", fixed = TRUE)

  # Dynamic UI exposes interaction control & patch updates it cleanly
  desc <- spec_to_controls(tr$spec)
  expect_true("inter_beta_x1__z1" %in% desc$control_ids)
  patched <- patch_spec_from_control(tr$spec, "inter_beta_x1__z1", 0.50)
  expect_true(patched$validation$valid)
  expect_equal(patched$spec$effects$interactions[[1L]]$beta, 0.50)

  # Refine: remove interaction
  r_rm <- refine_spec(tr$spec, "remove interaction x1:z1")
  expect_true(r_rm$validation$valid)
  expect_equal(length(r_rm$spec$effects$interactions), 0L)

  # Empirical recovery: 1-level interaction x1:x2 via compiled AST-verified script
  s_int1 <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 4000L),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal"),
      spec_predictor("x2", level = 1L, distribution = "normal")
    ),
    effects = list(
      intercept = 0.2,
      fixed = list(x1 = 0.3, x2 = -0.2),
      interactions = list(spec_interaction("x1", "x2", beta = 0.45)),
      focal_predictor = "x1:x2"
    ),
    simulation = list(n_reps = 2L, seed = 101L)
  )
  p1 <- tempfile(fileext = ".R")
  on.exit(unlink(p1), add = TRUE)
  code1 <- compile_r_script(s_int1, file = p1, run_on_source = TRUE)
  expect_true(verify_compiled_r_ast(code1)$safe)
  e1 <- new.env(parent = globalenv())
  capture.output(sys.source(p1, envir = e1))
  expect_equal(e1$results$summary$n_converged, 2L)
  expect_lt(abs(e1$results$summary$mean_est - 0.45), 0.08)

  # Empirical recovery: 2-level cross-level interaction x1:z1 via lmer
  skip_if_not_installed("lme4")
  s_int2 <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 120L, cluster_size = 20L, cluster_name = "school", unit_name = "student", icc = 0.2),
    predictors = list(
      spec_predictor("x1", level = 1L, distribution = "normal"),
      spec_predictor("z1", level = 2L, distribution = "normal")
    ),
    effects = list(
      intercept = 0,
      fixed = list(x1 = 0.3, z1 = 0.2),
      interactions = list(spec_interaction("x1", "z1", beta = 0.40)),
      focal_predictor = "x1:z1",
      random = list(
        intercept = TRUE,
        slopes = list(list(predictor = "x1", sd = 0.2)),
        corr_matrix = diag(2)
      )
    ),
    simulation = list(n_reps = 2L, seed = 202L)
  )
  p2 <- tempfile(fileext = ".R")
  on.exit(unlink(p2), add = TRUE)
  code2 <- compile_r_script(s_int2, file = p2, run_on_source = TRUE)
  expect_true(verify_compiled_r_ast(code2)$safe)
  e2 <- new.env(parent = globalenv())
  capture.output(sys.source(p2, envir = e2))
  expect_equal(e2$results$summary$n_converged, 2L)
  expect_lt(abs(e2$results$summary$mean_est - 0.40), 0.12)
})

test_that("unbalanced cluster sizes validate, parse without family collision, and generate varying cluster sizes", {
  # Guardrails
  s_bad1 <- new_spec_ir(validate = FALSE)
  s_bad1$hierarchy$cluster_size_dist <- "poisson"
  expect_true("unbalanced_in_single_level" %in% validate_spec_ir(s_bad1)$errors$code)

  s_bad2 <- new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 20L, cluster_size = 10L, cluster_size_dist = "normal", cluster_size_sd = -2, icc = 0.2),
    validate = FALSE
  )
  expect_true("invalid_cluster_size_sd" %in% validate_spec_ir(s_bad2)$errors$code)

  # NL parsing: "poisson cluster sizes" MUST keep Gaussian outcome family!
  tr_pois <- nl_to_spec("2-level study with 50 schools, 20 students per school, poisson cluster sizes, ICC 0.2")
  expect_true(tr_pois$validation$valid)
  expect_equal(tr_pois$spec$outcome$family, "gaussian")
  expect_equal(tr_pois$spec$hierarchy$cluster_size_dist, "poisson")
  expect_false("cluster_size_sd" %in% spec_to_controls(tr_pois$spec)$control_ids)

  tr_norm <- nl_to_spec("2-level study with 80 schools, 20 students per school, cluster size sd 6, ICC 0.2")
  expect_true(tr_norm$validation$valid)
  expect_equal(tr_norm$spec$hierarchy$cluster_size_dist, "normal")
  expect_equal(tr_norm$spec$hierarchy$cluster_size_sd, 6)
  expect_true("cluster_size_sd" %in% spec_to_controls(tr_norm$spec)$control_ids)

  # Execute compiled DGP for unbalanced normal and Poisson cluster sizes
  for (sp in list(tr_norm$spec, tr_pois$spec)) {
    fp <- tempfile(fileext = ".R")
    on.exit(unlink(fp), add = TRUE)
    code <- compile_r_script(sp, file = fp, run_on_source = FALSE)
    expect_true(verify_compiled_r_ast(code)$safe)
    env <- new.env(parent = globalenv())
    sys.source(fp, envir = env)
    set.seed(55, kind = "L'Ecuyer-CMRG")
    dat <- env$simulate_one_dataset(env$cfg)
    counts <- as.numeric(table(dat$school))
    expect_equal(length(counts), sp$hierarchy$n_clusters)
    expect_gt(stats::sd(counts), 2)
    expect_gte(min(counts), 2)
    expect_lt(abs(mean(counts) - 20), 3)
  }
})

test_that("heavy-tailed t and skewed errors validate, synthesize conditional controls, and recover exact moments", {
  # Guardrails
  s_bad_df <- new_spec_ir(outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1, error_dist = "t", error_df = 2), validate = FALSE)
  expect_true("invalid_error_df" %in% validate_spec_ir(s_bad_df)$errors$code)

  s_bad_sk <- new_spec_ir(outcome = list(name = "y", family = "gaussian", link = "identity", residual_sd = 1, error_dist = "skewed", error_skew = 0), validate = FALSE)
  expect_true("invalid_error_skew" %in% validate_spec_ir(s_bad_sk)$errors$code)

  # NL parsing + conditional controls
  tr_t <- nl_to_spec("Single-level Monte Carlo study N = 8000, x1 effect 0.3, residual sd 1.5, t-distributed errors df 5, 1 rep")
  expect_true(tr_t$validation$valid)
  expect_equal(tr_t$spec$outcome$error_dist, "t")
  expect_equal(tr_t$spec$outcome$error_df, 5)
  expect_true("error_df" %in% spec_to_controls(tr_t$spec)$control_ids)
  expect_false("error_skew" %in% spec_to_controls(tr_t$spec)$control_ids)

  # Empirical recovery: Student-t(df=5) standardized to SD = 1.5 has heavy tails (kurtosis > 1.5)
  pt <- tempfile(fileext = ".R")
  on.exit(unlink(pt), add = TRUE)
  code_t <- compile_r_script(tr_t$spec, file = pt, run_on_source = FALSE)
  expect_true(verify_compiled_r_ast(code_t)$safe)
  et <- new.env(parent = globalenv())
  sys.source(pt, envir = et)
  set.seed(1234, kind = "L'Ecuyer-CMRG")
  dat_t <- et$simulate_one_dataset(et$cfg)
  resid_t <- stats::residuals(stats::lm(y ~ x1, data = dat_t))
  expect_lt(abs(stats::sd(resid_t) - 1.5), 0.12)
  z_t <- (resid_t - mean(resid_t)) / stats::sd(resid_t)
  excess_kurt <- mean(z_t^4) - 3
  expect_gt(excess_kurt, 1.5)

  # Empirical recovery: Skewed standardized gamma (skew = 2.0, SD = 1.2)
  tr_sk <- nl_to_spec("Single-level study N = 8000, x1 effect 0.2, residual sd 1.2, skewed errors skewness 2.0, 1 rep")
  expect_true(tr_sk$validation$valid)
  expect_equal(tr_sk$spec$outcome$error_dist, "skewed")
  expect_equal(tr_sk$spec$outcome$error_skew, 2.0)
  expect_true("error_skew" %in% spec_to_controls(tr_sk$spec)$control_ids)
  expect_false("error_df" %in% spec_to_controls(tr_sk$spec)$control_ids)

  ps <- tempfile(fileext = ".R")
  on.exit(unlink(ps), add = TRUE)
  code_s <- compile_r_script(tr_sk$spec, file = ps, run_on_source = FALSE)
  expect_true(verify_compiled_r_ast(code_s)$safe)
  es <- new.env(parent = globalenv())
  sys.source(ps, envir = es)
  set.seed(4321, kind = "L'Ecuyer-CMRG")
  dat_s <- es$simulate_one_dataset(es$cfg)
  resid_s <- stats::residuals(stats::lm(y ~ x1, data = dat_s))
  expect_lt(abs(stats::sd(resid_s) - 1.2), 0.10)
  z_s <- (resid_s - mean(resid_s)) / stats::sd(resid_s)
  emp_skew <- mean(z_s^3)
  expect_lt(abs(emp_skew - 2.0), 0.25)
})

test_that("migrate_spec_ir upgrades v1.0.0 specs cleanly and rejects incompatible versions", {
  legacy <- unclass(new_spec_ir(
    design_type = "multilevel_sim",
    hierarchy = list(levels = 2L, n_clusters = 30L, cluster_size = 15L, icc = 0.2)
  ))
  legacy$schema_version <- "1.0.0"
  legacy$hierarchy$cluster_size_dist <- NULL
  legacy$hierarchy$cluster_size_sd <- NULL
  legacy$outcome$error_dist <- NULL
  legacy$outcome$error_df <- NULL
  legacy$outcome$error_skew <- NULL
  legacy$effects$interactions <- NULL
  legacy$predictor_corr <- NULL

  v_old <- validate_spec_ir(as_spec_ir(legacy, validate = FALSE))
  expect_true(v_old$valid)
  expect_true("schema_outdated" %in% v_old$warnings$code)

  migrated <- migrate_spec_ir(legacy)
  expect_equal(migrated$schema_version, "1.1.0")
  expect_equal(migrated$hierarchy$cluster_size_dist, "fixed")
  expect_equal(migrated$outcome$error_dist, "normal")
  expect_equal(migrated$effects$interactions, list())
  expect_true(length(attr(migrated, "migration_log")) >= 3L)
  expect_equal(nrow(validate_spec_ir(migrated)$warnings), 0L)

  # Round-trip auto_migrate via YAML file
  f_leg <- tempfile(fileext = ".yaml")
  on.exit(unlink(f_leg), add = TRUE)
  writeLines(yaml::as.yaml(legacy), f_leg)
  auto_up <- read_spec_ir(f_leg, auto_migrate = TRUE)
  expect_equal(auto_up$schema_version, "1.1.0")

  # Idempotent when already current
  again <- migrate_spec_ir(migrated)
  expect_equal(length(attr(again, "migration_log")), 0L)

  # Crisp version mismatch errors for future or unknown versions
  future_spec <- legacy; future_spec$schema_version <- "2.0.0"
  expect_error(migrate_spec_ir(future_spec), class = "specshiny_version_mismatch")
  ancient_spec <- legacy; ancient_spec$schema_version <- "0.5.0"
  expect_error(migrate_spec_ir(ancient_spec), class = "specshiny_version_mismatch")
})

test_that("spec_history supports undo, redo, branch truncation, and Shiny controls integration", {
  s1 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
  s2 <- refine_spec(s1, "set sample size to 200")$spec
  s3 <- refine_spec(s2, "set sample size to 350")$spec

  h <- spec_history(s1, max_entries = 10L)
  expect_false(history_can_undo(h))
  expect_false(history_can_redo(h))

  # No-op push ignored
  history_push(h, s1)
  expect_equal(nrow(history_summary(h)), 1L)

  history_push(h, s2, label = "N = 200")
  history_push(h, s3, label = "N = 350")
  expect_true(history_can_undo(h))
  expect_equal(history_current(h)$hierarchy$n_obs, 350L)

  u1 <- history_undo(h)
  expect_equal(u1$spec$hierarchy$n_obs, 200L)
  expect_true(history_can_redo(h))

  r1 <- history_redo(h)
  expect_equal(r1$spec$hierarchy$n_obs, 350L)
  expect_false(history_can_redo(h))

  # Undo then push a new branch truncates redo tail
  history_undo(h)
  s4 <- refine_spec(s2, "set sample size to 500")$spec
  history_push(h, s4, label = "N = 500")
  expect_false(history_can_redo(h))
  expect_equal(nrow(history_summary(h)), 3L)
  expect_no_error(suppressMessages(print(h)))

  # Wired into mod_spec_controls_server via testServer
  spec_rv <- shiny::reactiveVal(s1)
  diff_rv <- shiny::reactiveVal(NULL)
  shiny::testServer(
    mod_spec_controls_server,
    args = list(spec_rv = spec_rv, last_diff_rv = diff_rv),
    {
      expect_false(session$returned$can_undo())
      session$setInputs(n_obs = 240)
      expect_equal(spec_rv()$hierarchy$n_obs, 240L)
      expect_true(session$returned$can_undo())

      # Conversational edit pushed automatically via spec_rv observer
      spec_rv(refine_spec(spec_rv(), "set sample size to 420")$spec)
      session$flushReact()
      expect_equal(spec_rv()$hierarchy$n_obs, 420L)

      # Undo reverts conversational edit -> 240, then UI edit -> 100
      session$setInputs(undo_btn = 1)
      expect_equal(spec_rv()$hierarchy$n_obs, 240L)
      expect_true(session$returned$can_redo())

      session$setInputs(undo_btn = 2)
      expect_equal(spec_rv()$hierarchy$n_obs, 100L)

      # Redo steps forward -> 240
      session$setInputs(redo_btn = 1)
      expect_equal(spec_rv()$hierarchy$n_obs, 240L)
    }
  )
})

test_that("export_spec_bundle and verify_spec_bundle enforce path safety, SHA-256 checksums, and AST attestation", {
  spec <- new_spec_ir(
    design_type = "power_analysis",
    hierarchy = list(levels = 1L, n_obs = 120L),
    simulation = list(n_reps = 10L, seed = 42L)
  )

  # Reject .. path traversal
  expect_error(export_spec_bundle(spec, file.path(tempdir(), "..", "escape_bundle")), class = "specshiny_security_error")

  bdir <- file.path(tempdir(), paste0("specshiny_bundle_", Sys.getpid()))
  on.exit(unlink(bdir, recursive = TRUE), add = TRUE)

  out <- export_spec_bundle(spec, bdir, overwrite = TRUE)
  expect_true(all(file.exists(out$files)))
  expect_true(out$manifest$ast_verification$safe)

  # Refuses overwrite when overwrite = FALSE
  expect_error(export_spec_bundle(spec, bdir, overwrite = FALSE), "Refusing to overwrite")

  # Clean verification passes
  ver <- verify_spec_bundle(bdir)
  expect_s3_class(ver, "spec_bundle_verification")
  expect_true(ver$valid)
  expect_no_error(suppressMessages(print(ver)))

  # Tampering with simulation.R is caught by BOTH SHA-256 and static AST verifier
  writeLines(c(readLines(out$files[["simulation.R"]]), "system('id')"), out$files[["simulation.R"]])
  ver_tampered <- verify_spec_bundle(bdir)
  expect_false(ver_tampered$valid)
  expect_true(any(grepl("SHA-256 mismatch", ver_tampered$issues)))
  expect_true(any(grepl("AST verification", ver_tampered$issues)))
})

test_that("mod_spec_preview_server enforces hard preview caps and preserves global .Random.seed", {
  huge_spec <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 10000L),
    simulation = list(n_reps = 50000L, seed = 2026L, parameter_grid = list(n_obs = c(5000L, 10000L)))
  )
  spec_rv <- shiny::reactiveVal(huge_spec)

  set.seed(9999)
  seed_before <- get(".Random.seed", envir = globalenv(), inherits = FALSE)

  shiny::testServer(
    mod_spec_preview_server,
    args = list(spec_rv = spec_rv, max_preview_reps = 5L, max_preview_n = 400L, max_preview_seconds = 5),
    {
      session$setInputs(run_preview = 1)
      df <- session$returned$preview_results()
      warns <- session$returned$preview_warnings()
      expect_true(is.data.frame(df))
      expect_equal(df$n_reps, 5L)
      expect_equal(df$n_converged, 5L)
      expect_true(any(grepl("50000 to 5", warns)))
      expect_true(any(grepl("scaled down", warns)))
      expect_true(any(grepl("baseline design cell", warns)))
      expect_match(output$preview_run_ui$html, "Preview Budget Guardrails Applied", fixed = TRUE)
    }
  )

  # Global .Random.seed is preserved untouched after preview execution
  seed_after <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  expect_identical(seed_before, seed_after)
})
