## R CMD check results

Checked locally on macOS (`aarch64-apple-darwin23`, R 4.6.1):

* `rcmdcheck::rcmdcheck()` (including PDF reference manual): `0 errors | 0 warnings | 0 notes`
* `rcmdcheck::rcmdcheck(args = c("--as-cran"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_LIMIT_CORES_" = "true"))`: `0 errors | 0 warnings | 0 notes`
* `spelling::spell_check_package()`: `0 misspellings`
* `devtools::test()`: all unit, adversarial security, AST verification, empirical recovery, and `shiny::testServer()` tests pass with 0 failures, 0 warnings, 0 skips.

## Release summary (0.3.0)

* First CRAN-ready release candidate of `specshiny` (neuro-symbolic specification engine for dynamic `'Shiny'` UIs and deterministic AST-verified R/'Quarto' code generation).
* All offline tests, examples, and vignettes run without network access, respect `<= 2` CPU cores, write strictly to `tempdir()` with full cleanup, and preserve global `.Random.seed` / options state.
