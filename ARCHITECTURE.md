# `specshiny` Architecture

## 1. Motivation: Breaking the Flexibility vs. Accessibility Trade-Off

Graphical statistical software built in Shiny faces a fundamental design paradox when supporting open-ended methodological workflows such as Monte Carlo simulations, power analyses, and multilevel modeling:

- **Static comprehensive UIs** collapse into overwhelming "300-button cockpits" where users must hunt through dozens of irrelevant inputs (e.g., cluster counts, random slope covariance matrices, and MAR auxiliary predictors when designing a simple single-level linear regression).
- **Simplified static UIs** hit rigid walls as soon as a researcher needs a random slope, a non-Gaussian outcome, MAR attrition, or a parameter sweep.
- **Naive LLM-to-code chatbots** execute arbitrary generated R code directly, creating severe reproducibility, statistical correctness, and security hazards (silent hallucinations, non-positive-definite covariance matrices, unidentifiable models, and arbitrary code execution).

`specshiny` resolves this paradox with a **neuro-symbolic UI and code-generation architecture** that strictly decouples four concerns:

```text
+--------------------+      +-------------------------+      +---------------------------+
|  Natural Language  | ---> |  Pluggable Translator   | ---> |   Typed Specification IR  |
|  Prompt / Refines  |      | (Rule / OpenAI / Ollama)|      |   (JSON / YAML + Schema)  |
+--------------------+      +-------------------------+      +-------------+-------------+
                                                                           |
                                                                           v
                                                             +---------------------------+
                                                             |  Schema + Statistical     |
                                                             |  Sanity Validator Gate    |
                                                             +-------------+-------------+
                                                                           |
                                    +--------------------------------------+--------------------------------------+
                                    |                                                                             |
                                    v                                                                             v
                     +-----------------------------+                                               +-----------------------------+
                     |  Dynamic UI Synthesizer     | <====== Bidirectional Sync + Diffs ========> |  Deterministic Compiler     |
                     |  (bslib / Shiny Modules)    |                                               |  (Standalone R & Quarto)    |
                     +-----------------------------+                                               +-----------------------------+
```

**Core invariant:** Untrusted text from users or language models is *never* parsed or evaluated as R code. All translation produces a declarative data structure (`spec_ir`), which must pass strict structural validation and statistical sanity guardrails before driving either UI synthesis or deterministic template-based code compilation.

---

## 2. Formal Intermediate Representation (IR) Specification (`v1.0.0`)

The canonical IR is an S3 object (`class = c("spec_ir", "list")`) serialized losslessly to JSON or YAML and formally described in [`inst/schema/spec_ir_v1.json`](inst/schema/spec_ir_v1.json).

### 2.1 Top-Level Structure

| Field | Type | Required | Description |
| :--- | :--- | :---: | :--- |
| `schema_version` | string (`"1.0.0"`) | Yes | Semantic version of the IR grammar. |
| `metadata` | object | Yes | `title`, `description`, optional `source_prompt`, and `tags`. |
| `design_type` | enum | Yes | `"monte_carlo_lm"`, `"power_analysis"`, `"multilevel_sim"`, or `"two_level_mlm"`. |
| `hierarchy` | object | Yes | Data nesting structure (`levels` in `{1, 2}`, sample/cluster sizes, unit/cluster identifiers, `icc`). |
| `predictors` | array[object] | Yes | Non-empty list of predictor specs (`name`, `level`, `distribution`, distribution params, `centering`, `role`). |
| `outcome` | object | Yes | Response variable (`name`, `family`, `link`, `residual_sd`). |
| `effects` | object | Yes | `intercept`, `fixed` map, `focal_predictor`, and `random` (`intercept`, `slopes`, `corr_matrix`). |
| `missingness` | object | Yes | `mechanism` (`"none"`, `"MCAR"`, `"MAR"`), `rate`, `target`, `auxiliary`, `strength`. |
| `estimator` | object | Yes | `engine` (`"lm"`, `"glm"`, `"lmer"`, `"glmer"`), `reml`, `conf_level`, `missing_handling`. |
| `simulation` | object | Yes | `n_reps`, `seed`, `alpha`, and optional `parameter_grid` sweep map. |
| `defaults_rationale` | map[string, string] | Yes | Plain-language explanation for every inferred default keyed by dotted IR path. |

### 2.2 Parameterization Details

- **Identifiers:** `outcome$name`, `hierarchy$cluster_name`, `hierarchy$unit_name`, and all `predictors[[i]]$name` must match `^[A-Za-z][A-Za-z0-9_]*$`, must not be reserved R keywords (`if`, `for`, `function`, `NA`, `TRUE`, etc.) or internal reserved column names (`id`, `cluster_id`), and must be pairwise distinct.
- **Two-Level Random Intercept Variance:** For `levels == 2`, `hierarchy$icc` is the canonical intraclass correlation parameter. The implied random intercept variance $\tau_{00}$ is derived deterministically from `icc` and the level-1 variance $\sigma_e^2$:
  $$\tau_{00} = \sigma_e^2 \cdot \frac{\text{ICC}}{1 - \text{ICC}}$$
  where $\sigma_e^2 = \text{residual\_sd}^2$ for `gaussian`, $\pi^2 / 3$ for `binomial(logit)`, $1$ for `binomial(probit)`, and $1$ (latent scale) for `poisson(log)`. If `effects$random$intercept_sd` is also explicitly supplied, the validator checks positivity and consistency with `icc`.
- **Random Effects Covariance:** Given $q = \mathbb{I}(\text{random intercept}) + |\text{random slopes}|$ random effects with standard deviation vector $d = (\tau_0, \tau_1, \dots, \tau_{q-1})^\top$ and correlation matrix $R \in \mathbb{R}^{q \times q}$, the cluster random effects are drawn as $u_j \sim \mathcal{N}(0, \Sigma)$ where $\Sigma = \text{diag}(d)\, R\, \text{diag}(d)$ using a Cholesky factor in base R.

### 2.3 Validation & Statistical Sanity Guardrails (`validate_spec_ir()`)

Validation executes in two explicit layers, recording structured issues (`path`, `code`, `category`, `message`) without aborting unless `strict = TRUE` (or via `assert_valid_spec_ir()`):

1. **Schema & Type Layer (`category = "schema"`):**
   - Supported `schema_version`, required sections, exact scalar/vector types, finite numeric values, closed enum checks, identifier syntax, and non-collision across names.
2. **Statistical Sanity & Identifiability Layer (`category = "statistical"`):**
   - **Sample sizes & variances:** `n_obs >= 5` (1-level), `n_clusters >= 3`, `cluster_size >= 2` (2-level), `residual_sd > 0`, normal predictor `sd > 0`, binary `prob` in `(0, 1)`, uniform `min < max`, random slope `sd > 0`.
   - **ICC & Design Consistency:** `icc` in `[0, 1)` for 2-level designs; `levels == 2` enforced for `"multilevel_sim"` and `"two_level_mlm"`; `levels == 1` enforced for `"monte_carlo_lm"`; `power_analysis` supports both 1- and 2-level designs.
   - **Centering Validity:** `"group_mean"` centering is allowed only for level-1 predictors in a 2-level design.
   - **Random Slope Identifiability:**
     - Random slopes require `levels == 2`.
     - Every random slope must target an existing **level-1** predictor (a level-2 cluster-constant predictor cannot vary within clusters).
     - `cluster_size` must strictly exceed the number of random effects per cluster ($n_j > q$) and `n_clusters` must exceed $q$ so the random-effects covariance structure is identifiable.
   - **Positive-Definite Correlation Matrix:** `effects$random$corr_matrix` must be $q \times q$, finite, symmetric ($\max|R - R^\top| < 10^{-8}$), have unit diagonal ($\max|\text{diag}(R) - 1| < 10^{-8}$), and be strictly positive definite ($\lambda_{\min}(R) > 10^{-8}$ via `eigen(R, symmetric = TRUE, only.values = TRUE)`).
   - **Missingness Bounds:** `mechanism == "none"` requires `rate == 0`; `"MCAR"` and `"MAR"` require `rate` in `(0, 0.95)`; `"MAR"` requires a valid `auxiliary` predictor and finite non-zero `strength`.
   - **Estimator / Outcome / Hierarchy Compatibility:**
     - 1-level `gaussian` -> `lm` or `glm`; 1-level non-Gaussian -> `glm`.
     - 2-level `gaussian` with random effects -> `lmer`; 2-level non-Gaussian with random effects -> `glmer`.
   - **Parameter Grid Safety:** Allowed sweep axes must match design dimensionality (e.g., no `icc` or `n_clusters` sweep in a 1-level design), all sweep values must satisfy domain guardrails, and total grid cardinality $\prod_k |G_k| \le 250$ to prevent accidental combinatorial explosion.

---

## 3. Natural-Language-to-Spec Translation & Clarification Flow

The translator subsystem exposes a uniform S3 backend protocol (`translator_rule()`, `translator_openai()`, `translator_ollama()`):

1. **Initial Compilation (`nl_to_spec()`):**
   - Extracts explicit design parameters from natural language.
   - Populates sensible statistical defaults for underspecified slots and records transparent explanations in `defaults_rationale`.
   - Emits structured `clarifications` (each with `id`, `path`, `question`, `current_default`, `options`, `rationale`) for key underspecified choices rather than silently guessing.
   - Runs `validate_spec_ir()` on the resulting IR.
2. **Iterative Refinement (`refine_spec()`):**
   - Takes an existing validated `spec_ir` and a natural-language modification (e.g., *"increase clusters to 80"*, *"add a random slope for x1 with sd 0.25"*, *"set 20% MAR missingness"*).
   - Applies surgical updates while preserving untouched fields, re-validates, and computes a semantic diff via `diff_spec_ir()`.
3. **Structured Clarification Application (`answer_clarifications()`):**
   - Maps structured UI or programmatic user responses directly onto IR paths, updates `defaults_rationale` to reflect user confirmation, and returns the updated spec and diff.
4. **External LLM Backends:**
   - `translator_openai()` and `translator_ollama()` use `httr2` with strict JSON-only response instructions embedded with the v1 schema contract.
   - Returned text is parsed exclusively via `jsonlite::fromJSON()` (never `eval()` or `parse()`), normalized into `spec_ir`, and validated through `validate_spec_ir()`.

---

## 4. Dynamic UI Synthesis Rules (`spec_to_controls()`)

`spec_to_controls(spec)` is a pure function mapping any validated `spec_ir` into a minimal `spec_ui_descriptor`. Controls are included *if and only if* they are active in the current specification:

| Condition in `spec_ir` | Synthesized Controls | Omitted Controls |
| :--- | :--- | :--- |
| `hierarchy$levels == 1` | `n_obs` | `n_clusters`, `cluster_size`, `icc`, all random-effect controls |
| `hierarchy$levels == 2` | `n_clusters`, `cluster_size`, `icc`, random intercept/slope controls | `n_obs` |
| `predictor$distribution == "normal"` | `mean`, `sd`, `beta`, `centering` | `prob`, `min`, `max` |
| `predictor$distribution == "binary"` | `prob`, `beta`, `centering` | `mean`, `sd`, `min`, `max` |
| `predictor$distribution == "uniform"` | `min`, `max`, `beta`, `centering` | `mean`, `sd`, `prob` |
| `outcome$family == "gaussian"` | `residual_sd`, `link = "identity"` | non-Gaussian links |
| `outcome$family != "gaussian"` | valid family links only | `residual_sd` |
| `n_random_effects >= 2` | pairwise random-effect correlation sliders | omitted when `< 2` random effects |
| `missingness$mechanism == "none"` | `mechanism` selector only | `rate`, `auxiliary`, `strength` |
| `missingness$mechanism == "MCAR"` | `mechanism`, `rate` | `auxiliary`, `strength` |
| `missingness$mechanism == "MAR"` | `mechanism`, `rate`, `auxiliary`, `strength` | — |
| `estimator$engine == "lmer"` | `reml` toggle | omitted for `lm`, `glm`, `glmer` |
| `length(parameter_grid) > 0` | editors for active sweep axes + valid axis adder | invalid axes for current design |

---

## 5. Bidirectional Sync & Semantic Diff Model

Three reusable `bslib`/Shiny modules communicate through a shared `shiny::reactiveVal` pair: `spec_rv` (the single source of truth, always holding a validated `spec_ir`) and `last_diff_rv` (holding the latest `spec_diff` object):

- **`mod_spec_composer_*`**: Mutates `spec_rv` from NL compilation, conversational refinement, or clarification answers; writes the resulting `spec_diff` to `last_diff_rv`.
- **`mod_spec_controls_*`**:
  - **IR -> UI direction:** Tracks a structural signature `spec_structure_key(spec)`. Structural changes (e.g., adding a random slope, switching 1-level to 2-level, toggling MAR missingness) re-synthesize `bslib` cards from `spec_to_controls(spec)`. Value-only changes update existing widgets in place without destroying user focus.
  - **UI -> IR direction:** Each widget change calls pure `patch_spec_from_control(current_spec, control_id, value)` and checks `diff_spec_ir(current_spec, candidate)`.
  - **Loop-free idempotency:** If `!diff$has_changes`, the update is a no-op, guaranteeing that programmatic `update*Input()` calls never bounce back into infinite reactive cycles.
  - **Guardrail protection:** Candidate specs failing `validate_spec_ir()` are rejected with an inline diagnostic alert; `spec_rv` remains at its last valid state.
  - **Visual Diff Badges:** `last_diff_rv` drives inline Bootstrap/bslib pill badges on modified sections and a live change-summary strip showing exact `old -> new` transitions.
- **`mod_spec_preview_*`**: Read-only reactive observer of `spec_rv` displaying live mathematical equations (`spec_to_math()`), validated YAML/JSON IR, deterministic R script, Quarto `.qmd` report, and download handlers.

---

## 6. Deterministic Codegen Safety Guarantees

Both `compile_r_script()` and `compile_quarto_report()` uphold four strict invariants:

1. **Pre-Compilation Validation Gate:** `assert_valid_spec_ir(spec)` is mandatory; invalid IR objects cannot compile.
2. **Zero `eval(parse())` of Untrusted Strings:** No user prompt, clarification text, or LLM output is ever passed to `eval()`, `parse()`, `do.call()` with dynamic symbols, or unescaped formula string assembly.
3. **Closed Whitelists & Identifier Sanitization:**
   - Model engines, families, links, distributions, centering modes, and missingness mechanisms dispatch strictly through closed `switch()` statements.
   - Variable names pass strict regex validation (`^[A-Za-z][A-Za-z0-9_]*$`) and reserved-word checks before entering generated formulas or column assignments.
   - Free-text metadata (`title`, `description`) is sanitized (newlines removed) and emitted exclusively inside R comments or safely quoted string literals via `deparse()`.
4. **Byte-for-Byte Determinism:** For any given `spec_ir`, `compile_r_script(spec)` and `compile_quarto_report(spec)` produce identical output across repeated invocations, and generated R scripts set explicit seeds (`set.seed(cfg$seed)`) for reproducible simulation results.
