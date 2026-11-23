# specshiny

> **Break the flexibility-vs.-accessibility trade-off in statistical Shiny apps:** compile natural language into schema-validated specifications that dynamically synthesize clean `bslib` Shiny UIs, undo/redo history, and deterministic AST-verified R code.

## Overview

Complex statistical Shiny applications—such as generalized Monte Carlo simulation engines, power calculators, or multilevel study designers—typically suffer from either an intimidating "300-button cockpit" or rigid, oversimplified forms. Meanwhile, naive "LLM-writes-R-code" chatbots are fragile, non-reproducible, and unsafe to execute.

`specshiny` implements a **neuro-symbolic UI architecture** for R and Shiny that separates *user intent*, *interface presentation*, and *statistical execution*:

```text
Natural Language Prompt / Refinements
   │
   ▼  (Pluggable: Rule-Based Offline | OpenAI-Compatible API | Local Ollama)
Typed Specification IR (`spec_ir` v1.1.0 — JSON / YAML, forward-compatible migration)
   │
   ▼  (Strict Schema + Statistical Sanity Guardrails: PD matrices, identifiability, bounds)
   ├──▶ Dynamic `bslib`/Shiny UI Synthesizer (renders ONLY active controls + Undo/Redo + diffs)
   │        ▲
   │        └── Bidirectional Sync (UI tweaks ⇄ Conversational edits, loop-free)
   │
   └──▶ Deterministic Code, Report & Bundle Compiler (Standalone R + Quarto + SHA-256 Bundle)
            (Mandatory static AST safety gate `verify_compiled_r_ast()`; zero `eval(parse())`)
```

See [ARCHITECTURE.md](ARCHITECTURE.md) and [SECURITY.md](SECURITY.md) for the complete formal IR grammar, statistical guardrail specification, UI synthesis rules, threat model, and AST verification guarantees.

## Key Features

- **Typed IR Grammar, Migration & Statistical Guardrails (`new_spec_ir()`, `spec_predictor()`, `spec_interaction()`, `validate_spec_ir()`, `migrate_spec_ir()`, `diff_spec_ir()`):**
  - Models 1-level and 2-level hierarchical designs (`monte_carlo_lm`, `power_analysis`, `multilevel_sim`, `two_level_mlm`), predictor distributions & centering, correlated predictors (`predictor_corr`), two-way & cross-level interactions (`x1:z1`), unbalanced cluster sizes (`fixed`/`normal`/`poisson`), Gaussian/Binomial/Poisson outcomes, heavy-tailed Student-t and skewed level-1 errors, random intercepts & slopes with correlation matrices, MCAR/MAR missingness, estimators (`lm`, `glm`, `lmer`, `glmer`), parameter grid sweeps, and transparent `defaults_rationale`.
  - Pure-R schema and statistical sanity checks catch unknown nested properties, non-ASCII/reserved identifiers, non-positive variances, invalid ICCs, unidentifiable level-2 random slopes, insufficient cluster sizes, non-positive-definite correlation matrices (via eigenvalues), invalid missingness bounds, and combinatorial grid explosions.
- **Pluggable Natural-Language Translator (`nl_to_spec()`, `refine_spec()`, `answer_clarifications()`):**
  - Ships a deterministic offline parser (`translator_rule()`), an OpenAI-compatible HTTP adapter (`translator_openai()`), and a local Ollama adapter (`translator_ollama()`) with API key masking, URL validation, and response byte caps.
  - Surfaces structured clarifying questions for underspecified parameters and records plain-language rationales for every default.
- **Dynamic `bslib`/Shiny UI Synthesizer & Undo/Redo (`spec_to_controls()`, `spec_history()`, `mod_spec_composer_*`, `mod_spec_controls_*`, `mod_spec_preview_*`):**
  - Renders **only** the controls relevant to the active specification, maintains loop-free bidirectional sync with full undo/redo history across visual and conversational edits, blocks invalid UI edits with inline statistical guardrail alerts, highlights changes with live visual diff badges, and enforces hard interactive preview budgets so large specs never freeze Shiny.
- **Deterministic R, Quarto & Study Bundle Compilers (`compile_r_script()`, `compile_quarto_report()`, `verify_compiled_r_ast()`, `export_spec_bundle()`, `verify_spec_bundle()`):**
  - Converts any validated `spec_ir` into byte-for-byte deterministic, readable, standalone R simulation scripts (`L'Ecuyer-CMRG` RNG, Cholesky correlated predictors & random effects, calibrated MAR, unbalanced clusters, heavy-tailed/skewed errors), sanitized Quarto `.qmd` reports with LaTeX model equations, and SHA-256-verified reproducible study bundles—verified by a mandatory static positive-allowlist AST safety gate (`verify_compiled_r_ast()`) with **zero `eval(parse())`**.
- **Bundled Reference Demo App (`run_specshiny_demo()`):**
  - Ships a ready-to-run `bslib` Shiny application demonstrating the complete neuro-symbolic pipeline out of the box.

## Installation

```r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("MarcusHarrisUConn/specshiny")
```

## Quickstart

```r
library(specshiny)

# 1. Translate plain English into a schema-validated IR (works 100% offline)
res <- nl_to_spec(
  "2-level power analysis for the cross-level interaction between x1 and z1 of 0.30
   with 50 schools, 20 students per school, cluster size sd 5, ICC 0.2,
   random slope for x1, t-distributed errors df 5, 300 replications"
)
res$spec

# 2. Iteratively refine conversationally with undo/redo history
hist <- spec_history(res$spec)
ref <- refine_spec(res$spec, "increase clusters to 70 and sweep ICC across 0.1, 0.2, 0.3")
history_push(hist, ref$spec, label = "70 schools + ICC sweep")
ref$diff

# 3. Inspect the minimal dynamic UI descriptor synthesized for this spec
spec_to_controls(ref$spec)

# 4. Deterministically compile, statically AST-verify, and export a study bundle
r_script <- compile_r_script(ref$spec)
verify_compiled_r_ast(r_script)

bundle_dir <- file.path(tempdir(), "my_study_bundle")
export_spec_bundle(ref$spec, bundle_dir, overwrite = TRUE)
verify_spec_bundle(bundle_dir)

# 5. Launch the interactive flagship bslib/Shiny demo app
run_specshiny_demo()
```

## Documentation & Vignettes

- Online documentation site: <https://marcusharrisphd.com/specshiny/>
- `vignette("getting-started", package = "specshiny")` — End-to-end neuro-symbolic workflow from English prompts to executed R scripts and Quarto reports.
- `vignette("multilevel-power-walkthrough", package = "specshiny")` — Full 2-level cross-level interaction, unbalanced cluster size, and heavy-tailed error power study walkthrough.
- `vignette("building-custom-apps", package = "specshiny")` — Composing dynamic `bslib`/Shiny apps with undo/redo history, preview budget caps, `testServer()`, and OpenAI/Ollama translators.
- `vignette("ir-schema-and-safety", package = "specshiny")` — Closed IR allowlists, schema migration, static AST verification, Quarto/LaTeX/XSS defenses, and SHA-256 study bundles.
- [ARCHITECTURE.md](ARCHITECTURE.md) — Formal IR specification, validation guardrails, and design invariants.
- [SECURITY.md](SECURITY.md) — Threat model, adversarial red-team audit findings, and static AST verification guarantees.

## License

MIT © Marcus Harris
