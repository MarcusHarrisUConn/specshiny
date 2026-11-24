## R CMD check results

Checked locally on macOS (`aarch64-apple-darwin23`, R 4.6.1):

* `rcmdcheck::rcmdcheck()` (including PDF reference manual and all 5 vignettes): `0 errors | 0 warnings | 0 notes`
* `rcmdcheck::rcmdcheck(args = c("--as-cran"), env = c("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_LIMIT_CORES_" = "true"))`: `0 errors | 0 warnings | 0 package notes` (1 local macOS environment-only note regarding legacy `/usr/bin/tidy` HTML Tidy)
* `spelling::spell_check_package()`: `0 misspellings`
* `covr::percent_coverage(covr::package_coverage())`: `93.73%`
* `devtools::test()`: 963 assertions across 13 test files pass with `0 failures | 0 warnings | 0 skips`.

## Release summary (0.4.0)

* Added IR schema v1.2.0 longitudinal growth-curve modeling (`design_type = "longitudinal_growth"`) with wave schedules, stationary within-subject AR(1) autocorrelation (`outcome$ar1_rho`), and strictly monotone post-baseline attrition (`missingness$mechanism = "monotone_dropout"`), with chained forward migration (`1.0.0 -> 1.1.0 -> 1.2.0`).
* Added multi-scenario design comparison (`compare_specs()`) computing cluster design effects (DEFF), effective sample sizes (N_eff), variance decompositions, semantic IR diffs, and optional comparative preview power.
* Added an extensible domain template registry (`list_spec_templates()`, `use_spec_template()`, `register_spec_template()`, `unregister_spec_template()`) with 5 AST-verified built-in templates and strict schema + static AST gating for custom templates.
* Hardened error reporting against `cli`/`glue` interpolation on malformed remote LLM payloads and fixed 3 additional latent bugs uncovered during `covr` branch auditing.
* Zero new hard dependencies added to `Imports`. All offline tests, examples, and 5 vignettes run without network access, respect `<= 2` CPU cores, write strictly to `tempdir()` with full cleanup, and preserve global `.Random.seed`, options, and session template registry state.
