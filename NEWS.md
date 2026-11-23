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
