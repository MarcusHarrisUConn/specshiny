## R CMD check results

Checked locally on macOS (`aarch64-apple-darwin23`, R 4.6.1):

* `rcmdcheck::rcmdcheck()` (including PDF reference manual and all 5 vignettes): `0 errors | 0 warnings | 0 notes`
* `rcmdcheck::rcmdcheck(args = c("--as-cran"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_LIMIT_CORES_" = "true"))`: `0 errors | 0 warnings | 0 package notes` (1 local macOS environment-only note regarding legacy `/usr/bin/tidy` HTML Tidy and optional `V8`)
* `spelling::spell_check_package()`: `0 misspellings`
* `covr::percent_coverage(covr::package_coverage())`: `93.92%`
* `devtools::test()`: 1,146 assertions across 14 test files pass with `0 failures | 0 warnings | 0 skips`
* Source tarball: `specshiny_0.5.0.tar.gz` (256.7 KB / 0.251 MB)

## Release summary (0.5.1)

* Fixed Windows cross-platform path canonicalization (8.3 short-path expansion and case-insensitive drive/path comparison) and portable symlink/junction detection in `R/bundle.R` (`validate_bundle_dir()`, `export_spec_bundle()`, `verify_spec_bundle()`), plus isolated cross-platform symlink tests in `tests/testthat/test-peer-redteam.R`.
* Zero new dependencies.

## Release summary (0.5.0)

* Resolved all 17 independent peer red-team findings (3 critical, 4 high, 6 medium, 4 low) with permanent regression tests in `tests/testthat/test-peer-redteam.R`:
  - Hardened `verify_compiled_r_ast()` to walk all arguments of `library()` / `requireNamespace()`, enforce sequential lexical scoping without Pass-1 hoisting, prohibit system-function shadowing, and enforce a positive allowlist on unbound namespace functions and higher-order callable arguments (`lapply`, `vapply`, `tryCatch`, `stats::ave`, `stats::uniroot`).
  - Eliminated all `cli`/`glue` template injection risks across `abort_spec_validation()` (now `rlang::abort()`) and all S3 `print()` methods.
  - Added symlink and canonical directory containment checks to `validate_bundle_dir()`, `export_spec_bundle()`, and `verify_spec_bundle()`.
  - Prevented 32-bit integer overflow in `spec_total_n()`, added explicit sample-size upper bounds in `validate_spec_ir()`, and hardened `run_spec_preview_sim()` budget guards.
  - Moved OpenAI API keys into a session-scoped non-serializing secret vault so `saveRDS()` never persists cleartext credentials, and enforced CRLF/control-character, temperature, TCP port, and cloud-metadata/link-local SSRF validation across LLM backends.
  - Added `lme4::isSingular()` and `@optinfo$conv` convergence checks and `on.exit()` caller `RNGkind()`/`.Random.seed` restoration in compiled scripts, dynamically recomputed `tau0` during `parameter_grid` ICC sweeps, reconciled auxiliary `monotone_dropout` calibration between compiled scripts and preview runs, added exact left-censored cluster-size moments to `compare_specs()`, and closed validation/refinement edge cases.
* Zero new hard dependencies added to `Imports`. All offline tests, examples, and 5 vignettes run without network access, respect `<= 2` CPU cores, write strictly to `tempdir()` with full cleanup, and preserve global `.Random.seed`, `RNGkind()`, options, and session template registry state.
