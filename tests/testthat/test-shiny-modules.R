test_that("mod_spec_composer_server compiles NL prompts, refines specs, and applies clarifications", {
  spec_rv <- shiny::reactiveVal(NULL)
  diff_rv <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_spec_composer_server,
    args = list(spec_rv = spec_rv, translator = translator_rule(), last_diff_rv = diff_rv),
    {
      # 1. Compile an underspecified prompt
      session$setInputs(prompt = "2-level power analysis with random slope for x1", compile = 1)
      expect_true(is_spec_ir(spec_rv()))
      expect_true(is_valid_spec_ir(spec_rv()))
      expect_equal(spec_rv()$hierarchy$levels, 2L)
      expect_true(length(session$returned$clarifications()) >= 2L)

      # 2. Apply structured clarification answers
      session$setInputs(clarif_n_clusters = "60", clarif_icc = "0.25", apply_clarifications = 1)
      expect_equal(spec_rv()$hierarchy$n_clusters, 60L)
      expect_equal(spec_rv()$hierarchy$icc, 0.25)
      expect_true(diff_rv()$has_changes)
      expect_true("hierarchy.n_clusters" %in% diff_rv()$changed_paths)

      # 3. Apply conversational refinement
      session$setInputs(refine_input = "increase clusters to 90 and set 20% MAR missingness", refine = 1)
      expect_equal(spec_rv()$hierarchy$n_clusters, 90L)
      expect_equal(spec_rv()$missingness$mechanism, "MAR")
      expect_equal(spec_rv()$missingness$rate, 0.20)
      expect_true("missingness.mechanism" %in% diff_rv()$changed_paths)

      # 4. Load built-in preset
      session$setInputs(preset_choice = "single_level_lm", load_preset = 1)
      expect_equal(spec_rv()$hierarchy$levels, 1L)
      expect_equal(spec_rv()$hierarchy$n_obs, 250L)
    }
  )
})

test_that("mod_spec_controls_server maintains bidirectional sync, visual diffs, and guardrails", {
  init <- nl_to_spec(
    "2-level multilevel power simulation with 60 schools, 25 students per school, ICC 0.2, treatment effect 0.35, random slope for x1, 15% MAR missingness, 500 replications"
  )$spec

  spec_rv <- shiny::reactiveVal(init)
  diff_rv <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_spec_controls_server,
    args = list(spec_rv = spec_rv, last_diff_rv = diff_rv),
    {
      # Initial flush synthesizes 2-level controls and omits n_obs
      desc0 <- session$returned$descriptor()
      expect_true(all(c("n_clusters", "cluster_size", "icc", "slope_sd_x1", "corr_1_2", "missing_rate", "missing_auxiliary") %in% desc0$control_ids))
      expect_false("n_obs" %in% desc0$control_ids)

      # UI -> IR edit: tweak n_clusters from 60 -> 85
      session$setInputs(n_clusters = 85)
      expect_equal(spec_rv()$hierarchy$n_clusters, 85L)
      expect_true(diff_rv()$has_changes)
      expect_true("hierarchy.n_clusters" %in% diff_rv()$changed_paths)
      expect_null(session$returned$guardrail_error())

      # Diff banner HTML displays the live change badge
      banner_html <- output$diff_banner$html
      expect_match(banner_html, "60 -&gt; 85|60 -> 85", perl = TRUE)

      # Conversational / external IR -> UI update
      refined <- refine_spec(spec_rv(), "set ICC to 0.3 and set treatment effect to 0.5")
      spec_rv(refined$spec)
      diff_rv(refined$diff)
      session$flushReact()
      expect_equal(spec_rv()$hierarchy$icc, 0.3)
      expect_equal(spec_rv()$effects$fixed$treatment, 0.5)

      # Structural UI edit: turning off missingness removes missingness sliders dynamically
      session$setInputs(missing_mechanism = "none")
      expect_equal(spec_rv()$missingness$mechanism, "none")
      desc_after <- session$returned$descriptor()
      expect_false("missing_rate" %in% desc_after$control_ids)
      expect_false("missing_auxiliary" %in% desc_after$control_ids)

      # Statistical guardrail check via UI: cluster_size = 2 with 2 random effects is blocked!
      prev_cs <- spec_rv()$hierarchy$cluster_size
      session$setInputs(cluster_size = 2)
      expect_equal(spec_rv()$hierarchy$cluster_size, prev_cs)
      expect_false(is.null(session$returned$guardrail_error()))
      expect_false(session$returned$guardrail_error()$valid)
      expect_match(output$guardrail_alert$html, "Statistical Guardrail Blocked", fixed = TRUE)

      # Adding a parameter grid sweep axis dynamically creates grid_icc control
      session$setInputs(add_grid_axis = "icc", add_grid_btn = 1)
      expect_true("icc" %in% names(spec_rv()$simulation$parameter_grid))
      expect_true("grid_icc" %in% session$returned$descriptor()$control_ids)
    }
  )
})

test_that("mod_spec_preview_server updates live math, IR, R script, Quarto, and quick preview", {
  s1 <- new_spec_ir(
    design_type = "monte_carlo_lm",
    hierarchy = list(levels = 1L, n_obs = 100L),
    simulation = list(n_reps = 20L, seed = 42L)
  )
  spec_rv <- shiny::reactiveVal(s1)

  shiny::testServer(
    mod_spec_preview_server,
    args = list(spec_rv = spec_rv),
    {
      expect_match(session$returned$r_script(), "n_obs = 100L", fixed = TRUE)
      expect_match(session$returned$quarto_report(), "## 2. Mathematical Model Specification", fixed = TRUE)
      expect_match(session$returned$yaml(), "n_obs: 100", fixed = TRUE)
      expect_match(session$returned$json(), "\"n_obs\": 100", fixed = TRUE)

      # Updating spec_rv reactively updates compiled R script and Quarto report
      s2 <- refine_spec(s1, "set sample size to 250")$spec
      spec_rv(s2)
      session$flushReact()
      expect_match(session$returned$r_script(), "n_obs = 250L", fixed = TRUE)

      # Running quick preview executes cleanly and returns bounded 10-rep summary
      session$setInputs(run_preview = 1)
      prev_df <- session$returned$preview_results()
      expect_true(is.data.frame(prev_df))
      expect_equal(prev_df$n_reps, 10L)
      expect_equal(prev_df$n_converged, 10L)
    }
  )
})

test_that("full 3-module app server synchronizes NL composer, dynamic controls, and deterministic code preview", {
  init_spec <- nl_to_spec("Single-level Monte Carlo simulation with 150 participants, x1 effect 0.3, 50 reps")$spec

  app_server <- function(input, output, session) {
    spec_rv <- shiny::reactiveVal(init_spec)
    last_diff_rv <- shiny::reactiveVal(NULL)
    comp <- mod_spec_composer_server("comp", spec_rv = spec_rv, translator = translator_rule(), last_diff_rv = last_diff_rv)
    ctrl <- mod_spec_controls_server("ctrl", spec_rv = spec_rv, last_diff_rv = last_diff_rv)
    prev <- mod_spec_preview_server("prev", spec_rv = spec_rv, last_diff_rv = last_diff_rv)
  }

  shiny::testServer(app_server, {
    # Initial 1-level spec has n_obs and no multilevel controls
    expect_true("n_obs" %in% ctrl$descriptor()$control_ids)
    expect_false("n_clusters" %in% ctrl$descriptor()$control_ids)
    expect_match(prev$r_script(), "n_obs = 150L", fixed = TRUE)

    # 1. UI control edit updates shared spec_rv and compiled R script
    session$setInputs(`ctrl-n_obs` = 300)
    expect_equal(spec_rv()$hierarchy$n_obs, 300L)
    expect_match(prev$r_script(), "n_obs = 300L", fixed = TRUE)
    expect_true("hierarchy.n_obs" %in% last_diff_rv()$changed_paths)

    # 2. Conversational NL compile switches to 2-level MLM; dynamic controls & preview adapt
    session$setInputs(
      `comp-prompt` = "2-level power study with 50 schools, 20 students per school, ICC 0.2, treatment effect 0.4, 100 reps",
      `comp-compile` = 1
    )
    expect_equal(spec_rv()$hierarchy$levels, 2L)
    expect_true("n_clusters" %in% ctrl$descriptor()$control_ids)
    expect_false("n_obs" %in% ctrl$descriptor()$control_ids)
    expect_match(prev$r_script(), "lme4::lmer", fixed = TRUE)
    expect_match(prev$r_script(), "n_clusters = 50L", fixed = TRUE)

    # 3. Conversational refinement updates both controls and compiled R script
    session$setInputs(`comp-refine_input` = "increase clusters to 90", `comp-refine` = 1)
    expect_equal(spec_rv()$hierarchy$n_clusters, 90L)
    expect_match(prev$r_script(), "n_clusters = 90L", fixed = TRUE)
    expect_equal(last_diff_rv()$changes$new_value[last_diff_rv()$changes$path == "hierarchy.n_clusters"], "90")
  })
})
