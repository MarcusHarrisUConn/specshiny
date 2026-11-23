# specshiny

> **Break the flexibility-vs.-accessibility trade-off in statistical Shiny apps:** compile natural language into schema-validated specifications that dynamically synthesize clean `bslib` Shiny UIs and deterministic, reproducible R code.

## Overview

Complex statistical Shiny applications—such as generalized Monte Carlo simulation engines, power calculators, or multilevel study designers—typically suffer from either an intimidating "300-button cockpit" or rigid, oversimplified forms. Meanwhile, naive "LLM-writes-R-code" chatbots are fragile, non-reproducible, and unsafe to execute.

`specshiny` implements a **neuro-symbolic UI architecture** for R and Shiny that separates *user intent*, *interface presentation*, and *statistical execution*:

```text
Natural Language Prompt / Refinements
   │
   ▼  (Pluggable: Rule-Based Offline | OpenAI-Compatible API | Local Ollama)
Typed Specification IR (`spec_ir` — JSON / YAML, Schema v1.0.0)
   │
   ▼  (Strict Schema + Statistical Sanity Guardrails: PD matrices, identifiability, bounds)
   ├──▶ Dynamic `bslib`/Shiny UI Synthesizer (renders ONLY active controls + visual diffs)
   │        ▲
   │        └── Bidirectional Sync (UI tweaks ⇄ Conversational edits, loop-free)
   │
   └──▶ Deterministic Code & Report Compiler (Standalone R Script + Quarto `.qmd`)
            (Zero `eval(parse())` of untrusted LLM strings)
```

See [ARCHITECTURE.md](ARCHITECTURE.md) for the complete formal IR grammar, statistical guardrail specification, UI synthesis rules, bidirectional sync protocol, and codegen safety guarantees.

## Key Features

- **Typed IR Grammar & Statistical Guardrails (`new_spec_ir()`, `validate_spec_ir()`, `read_spec_ir()`, `write_spec_ir()`, `diff_spec_ir()`):**
  - Models 1-level and 2-level hierarchical designs (`monte_carlo_lm`, `power_analysis`, `multilevel_sim`, `two_level_mlm`), predictor distributions & centering, Gaussian/Binomial/Poisson outcomes, random intercepts & slopes with correlation matrices, MCAR/MAR missingness, estimators (`lm`, `glm`, `lmer`, `glmer`), parameter grid sweeps, and transparent `defaults_rationale`.
  - Pure-R schema and statistical sanity checks catch non-positive variances, invalid ICCs, unidentifiable level-2 random slopes, insufficient cluster sizes, non-positive-definite random-effect correlation matrices (via eigenvalues), invalid missingness bounds, and combinatorial grid explosions.
- **Pluggable Natural-Language Translator (`nl_to_spec()`, `refine_spec()`, `answer_clarifications()`):**
  - Ships a deterministic offline parser (`translator_rule()`), an OpenAI-compatible HTTP adapter (`translator_openai()`), and a local Ollama adapter (`translator_ollama()`).
  - Surfaces structured clarifying questions for underspecified parameters and records plain-language rationales for every default.
- **Dynamic `bslib`/Shiny UI Synthesizer (`spec_to_controls()`, `mod_spec_composer_*`, `mod_spec_controls_*`, `mod_spec_preview_*`):**
  - Renders **only** the controls relevant to the active specification, maintains loop-free bidirectional sync between UI inputs and conversational edits, blocks invalid UI edits with inline statistical guardrail alerts, and highlights changes with live visual diff badges.
- **Deterministic R & Quarto Compilers (`compile_r_script()`, `compile_quarto_report()`):**
  - Converts any validated `spec_ir` into byte-for-byte deterministic, readable, standalone R simulation scripts and Quarto `.qmd` reports with LaTeX model equations—with **zero `eval(parse())`** of untrusted text.
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
  "2-level multilevel power simulation with 60 schools, 25 students per school,
   ICC 0.2, treatment effect 0.35, random slope for x1, 15% MAR missingness,
   500 replications"
)
res$spec

# 2. Iteratively refine conversationally and inspect the semantic diff
ref <- refine_spec(res, "increase clusters to 80 and sweep ICC across 0.1, 0.2, 0.3")
ref$diff

# 3. Inspect the minimal dynamic UI descriptor synthesized for this spec
spec_to_controls(ref$spec)

# 4. Deterministically compile standalone R code and a Quarto report
r_script <- compile_r_script(ref$spec)
qmd_report <- compile_quarto_report(ref$spec)

# 5. Launch the interactive flagship bslib/Shiny demo app
run_specshiny_demo()
```

## Learn More

- `vignette("getting-started", package = "specshiny")` — End-to-end neuro-symbolic workflow from English prompts to executed R scripts and Quarto reports.
- `vignette("building-custom-apps", package = "specshiny")` — Building custom dynamic `bslib`/Shiny apps with `specshiny` modules, `testServer()` verification, and OpenAI/Ollama translators.
- [ARCHITECTURE.md](ARCHITECTURE.md) — Formal IR specification, validation guardrails, and safety invariants.

## License

MIT © Marcus Harris
