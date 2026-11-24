# specshiny 0.4.0

## New Longitudinal, Comparison & Template Capabilities (Zero New Hard Dependencies)

* Added longitudinal growth-curve modeling (`design_type = "longitudinal_growth"`, IR schema v1.2.0 with chained forward migration `1.0.0 -> 1.1.0 -> 1.2.0` via `migrate_spec_ir()`):
  - Supports repeated-measures wave count (`hierarchy$time_points`), custom strictly increasing wave schedules (`hierarchy$time_spacing`), `distribution = "time"` predictors, random intercepts and random time slopes, stationary within-subject AR(1) residual autocorrelation (`outcome$ar1_rho` strictly in `(-0.95, 0.95)` plus `ar1_rho` parameter grid sweeps), and strictly monotone post-baseline attrition (`missingness$mechanism = "monotone_dropout"`, both unconditional and auxiliary-driven MAR).
  - Wired end-to-end through `validate_spec_ir()`, `spec_to_math()`, `spec_to_controls()`, `patch_spec_from_control()`, `translator_rule()`, `refine_spec()`, `mod_spec_preview_server()`, `compile_r_script()` (passing strict `verify_compiled_r_ast()`), and `compile_quarto_report()`.
* Added multi-scenario design comparison via `compare_specs()` (with `print()` and `as.data.frame()` S3 methods), reporting total $N$, Kish / Eldridge-Ashby-Kerry cluster design effect ($\text{DEFF} = 1 + (\bar{n}_j(1 + \text{CV}^2) - 1)\,\text{ICC}$), effective sample size $N_{\text{eff}} = N / \text{DEFF}$, level-1/level-2 variance decomposition, AR(1) autocorrelation, synthesized UI control counts, pairwise semantic IR diffs vs. baseline, and optional bounded comparative power estimates (`run_power = TRUE`).
* Added an extensible domain template registry (`list_spec_templates()`, `use_spec_template()`, `register_spec_template()`, `unregister_spec_template()`) shipping 5 immutable built-in templates (`cluster_rct`, `cross_level_mlm`, `longitudinal_growth_ar1`, `logistic_power`, `factorial_ols`), ergonomic parameter overrides and NL refinement hooks, and strict `assert_valid_spec_ir()` + `verify_compiled_r_ast()` registration gating for custom templates.

## Coverage Audit & Latent Bug Fixes

* Audited line coverage with `covr::package_coverage()` (lifting overall package coverage from 82.84% to 93.73%) and fixed 4 latent bugs:
  1. **Remote LLM Error Glue-Interpolation Hardening (`R/translator-backends.R`, `R/security.R`)**: Escaped untrusted error payloads in `cli::cli_abort()` / `rlang::abort()` so malformed remote JSON or AST violation messages containing `{...}` braces cannot crash `glue` or inject expressions.
  2. **Correlated Predictor Matrix Resize on Implicit Refinement Additions (`R/translator-nl.R`)**: Automatically resized `predictor_corr` when `refine_spec()` implicitly introduces a new predictor via an interaction or random slope.
  3. **Preview Simulator Proportional Cluster-SD & Longitudinal Scaling (`R/mod-preview.R`)**: Scaled `cluster_size_sd` proportionally when shrinking large unbalanced normal-cluster previews, and preserved wave count (`time_points`) while scaling subject count (`n_clusters`) for large longitudinal previews.
  4. **Vector-Safe Parameter Grid Patching (`R/ui-synthesize.R`)**: Collapsed multi-element character/numeric inputs safely before splitting in `patch_spec_from_control()` for `grid_*` controls.

## Documentation & Vignettes

* Added a fifth executable vignette (`vignettes/longitudinal-and-templates.Rmd`) demonstrating longitudinal AR(1) growth curves with monotone dropout, `compare_specs()` efficiency & power trade-offs, and custom template registration.
* Updated `README.md`, `inst/guides/specshiny-guide.Rmd`, `_pkgdown.yml`, `cran-comments.md`, and bundled v1.2.0 YAML examples.

# specshiny 0.3.0

## New Modeling & Simulation Capabilities (All AST-Verified)

* Added two-way and cross-level interaction terms (`spec_interaction()`, `effects$interactions`) across `spec_ir` v1.1.0, `validate_spec_ir()`, `spec_to_math()`, `spec_to_controls()`, `translator_rule()`, `refine_spec()`, `compile_r_script()`, and `mod_spec_preview_server()`, including targeting interactions (e.g., `x1:z1`) directly as `effects$focal_predictor`.
* Added unbalanced 2-level cluster sizes (`hierarchy$cluster_size_dist` supporting `"fixed"`, `"normal"`, and `"poisson"` plus `hierarchy$cluster_size_sd`) with automatic identifiability floor guards.
* Added heavy-tailed Student-t (`outcome$error_dist = "t"`, `outcome$error_df > 2`) and skewed standardized gamma (`outcome$error_dist = "skewed"`, `outcome$error_skew` in `[-4, 4] \ {0}`) level-1 Gaussian error distributions preserving exact residual variance and ICC.

## IR Migration, Undo/Redo History, Bundles & Preview Caps

* Added `migrate_spec_ir()` for forward-compatible schema upgrades (`1.0.0` -> `1.1.0`) with structured `migration_log` audit trails and crisp `specshiny_version_mismatch` errors, plus optional `auto_migrate = TRUE` in `read_spec_ir()` and `parse_spec_text()`.
* Added `spec_history()` (`history_push()`, `history_undo()`, `history_redo()`, `history_can_undo()`, `history_can_redo()`, `history_current()`, `history_summary()`) wired directly into `mod_spec_controls_ui()` / `mod_spec_controls_server()` and `specshiny_demo_app()` so users can undo/redo both visual widget edits and conversational refinements.
* Added `export_spec_bundle()` and `verify_spec_bundle()` writing path-safe, self-contained study archives (`spec.yaml`, `simulation.R`, `report.qmd`, `bundle_manifest.json` with SHA-256 digests and static AST safety attestation).
* Added configurable preview-runner budgets (`max_preview_reps`, `max_preview_n`, `max_preview_seconds`) in `mod_spec_preview_server()` so large specs (e.g., `n_reps = 50000`) display clear guardrail warnings instead of freezing Shiny, while preserving global `.Random.seed`.

## CRAN Readiness, Vignettes & Documentation

* Expanded to 4 executable `knitr` vignettes (`getting-started`, `building-custom-apps`, `ir-schema-and-safety`, `multilevel-power-walkthrough`), runnable examples across every exported function, `inst/CITATION`, `inst/examples/`, `inst/guides/specshiny-guide.Rmd`, `inst/WORDLIST`, `cran-comments.md`, GitHub Actions CI/pkgdown workflows, and `_pkgdown.yml`.

# specshiny 0.2.0

## Security Hardening

* Added centralized security, sanitization, escaping, and static AST verification module (`R/security.R`) and documented the red-team threat model in `SECURITY.md`.
* Exported `verify_compiled_r_ast()`, a static positive-allowlist AST safety verifier that rejects `eval`, `parse`, `system`, `system2`, `do.call`, `source`, `unlink`, `file.remove`, `get`, `assign`, `:::`, dynamic callees, unwhitelisted packages, and higher-order references to banned symbols. `compile_r_script()` now runs `verify_compiled_r_ast(..., strict = TRUE)` as a mandatory post-compilation gate and emits zero `do.call()` calls.
* Hardened `compile_quarto_report()` against YAML frontmatter breakouts, inline knitr code execution (`` `r ...` ``), markdown code-fence escapes (using 6-tilde fences and interior line sanitization), HTML/XSS tags, markdown table injection, and dangerous LaTeX macros (`\write18`, `\input`, `\immediate`, etc.).
* Hardened `parse_spec_text()` to explicitly enforce `yaml::yaml.load(..., eval.expr = FALSE)`, reject `!expr` / `!r` YAML tags, and cap maximum payload size.
* Hardened `validate_spec_ir()` with strict pure-ASCII identifier allowlists (blocking Unicode homoglyphs, leading dots, >64-byte identifiers, and expanded reserved words), closed nested key allowlists across every IR subsection (blocking unknown extra properties from LLM JSON payloads), and strict dotted-path validation on `defaults_rationale` keys.
* Hardened `translator_openai()` and `translator_ollama()` with strict URL validation (`https://` default; loopback `http://localhost` for Ollama), API key storage in a private environment with masked printing and error-trace redaction, bounded timeouts, and `max_response_bytes` enforcement before JSON parsing.

## Statistical Accuracy, Parser & Shiny Improvements

* Upgraded `compile_r_script()` and live preview execution to use reproducible `L'Ecuyer-CMRG` RNG (`kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection"`).
* Added optional `predictor_corr` positive-definite correlation matrix support to `spec_ir`, `validate_spec_ir()` (with cross-level correlation guardrails), `diff_spec_ir()`, `spec_to_math()`, `spec_to_controls()`, `compile_r_script()` (exact Cholesky draws and Gaussian copulas), and `translator_rule()`.
* Expanded `translator_rule()` and `refine_spec()` to parse negative effects, percentage vs. proportion inputs (ICC, missingness, alpha, prevalence), correlated multi-predictor designs, 2-level to 1-level switches, predictor removal, link/estimator switches, and MAR auxiliary/strength edits.
* Fixed a crash in `diff_spec_ir()` and `patch_spec_from_control()` on `NA`/`NaN`/`Inf` UI inputs and hardened `mod_spec_controls_server()` against rapid out-of-range edits and stale guardrail alerts.
* Added comprehensive adversarial security, XSS, Quarto/LaTeX/YAML injection, AST verification, and empirical statistical parameter/ICC/MAR/correlation recovery tests in `tests/testthat/test-adversarial-security.R`.

# specshiny 0.1.0

* Initial release of `specshiny`: versioned `spec_ir` grammar with JSON/YAML serialization and pure-R statistical sanity guardrails, pluggable NL-to-spec translators (`translator_rule()`, `translator_openai()`, `translator_ollama()`), dynamic `bslib`/Shiny UI synthesizer and modular suite with bidirectional sync and visual diff badges, deterministic R script and Quarto report compilers, bundled demo app (`run_specshiny_demo()`), and vignettes.
