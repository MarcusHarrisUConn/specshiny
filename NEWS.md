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
