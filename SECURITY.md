# Security Model & Adversarial Hardening Audit (`specshiny`)

`specshiny` is built around a strict **neuro-symbolic trust boundary**: natural-language user prompts, external LLM responses, and serialized YAML/JSON specifications are treated as **untrusted data** and are never passed to `eval()` or dynamically executed. Every transition from untrusted input to UI rendering or R/Quarto code generation must pass through centralized validation, sanitization, and static AST verification in [`R/security.R`](R/security.R).

---

## 1. Threat Model & Trust Boundaries

```text
[Untrusted NL / LLM JSON / YAML / Shiny Inputs]
        │
        ▼  1. Transport & Parser Guards (HTTPS/local URL check, byte cap, YAML !expr block)
        ▼  2. Strict Schema & Pure-ASCII Identifier Validator (closed nested key allowlists)
        ▼  3. Statistical Sanity & Identifiability Guardrails (PD matrices, bounds, NA/Inf safe)
        │
  ┌─────┴───────────────────────────────────────────────┐
  ▼                                                     ▼
[Shiny / bslib UI]                         [Deterministic R & Quarto Compilers]
- Zero raw HTML() interpolation            - Audited closed-switch templates
- htmltools automatic XSS escaping         - Quarto prose/frontmatter/YAML-fence sanitizers
- Guardrails reject invalid UI edits       - Post-codegen verify_compiled_r_ast() AST gate
```

---

## 2. v0.2.0 Red-Team Findings & Concrete Mitigations

During the v0.2.0 adversarial security audit, the following real vulnerabilities and edge-case failure modes were identified in v0.1.0 and resolved:

| # | Severity | Surface | Vulnerability Found in v0.1.0 | Mitigation Implemented in v0.2.0 |
| :- | :--- | :--- | :--- | :--- |
| 1 | **Critical** | `compile_quarto_report()` | Inline knitr expressions (`` `r system("id")` ``) inside `source_prompt`, `defaults_rationale`, or `title` survived single-line stripping and could execute during `quarto render`. | Added `sanitize_quarto_prose()` neutralizing all backticks, `~~~`, `:::`, `---`, HTML `<`/`>`, table pipes, and dangerous LaTeX macros across all prose, table cells, and frontmatter scalars. |
| 2 | **Critical** | `compile_quarto_report()` | Embedded YAML IR inside triple-backtick fences (```` ```yaml ````) could be escaped via CommonMark-indented ```` ``` ```` sequences inside multiline metadata fields, allowing injected ```` ```{r} ```` chunks. | Wrapped embedded YAML IR in a 6-tilde outer fence (`~~~~~~yaml`) and passed all interior YAML lines through `sanitize_yaml_block_for_quarto()`. |
| 3 | **High** | `compile_quarto_report()` / PDF LaTeX | Raw LaTeX directives (`\write18{...}`, `\immediate`, `\input{...}`) in metadata or rationale strings were not neutralized before emission. | Added `SPEC_DANGEROUS_LATEX_RE` filtering and backslash neutralization in `sanitize_quarto_prose()`, `sanitize_yaml_frontmatter_scalar()`, and `sanitize_yaml_block_for_quarto()`; enforced ASCII identifier verification inside `escape_latex_ident()`. |
| 4 | **High** | `parse_spec_text()` (YAML) | `yaml::yaml.load(text)` inherited global `getOption("yaml.eval.expr")`, risking `!expr` execution if enabled in a host session. | Explicitly passed `eval.expr = FALSE` and added `assert_safe_yaml_input()` rejecting `!expr` / `!r` tags and payloads > 1 MB before parsing. |
| 5 | **High** | `validate_spec_ir()` | Only top-level extra keys were rejected; unknown nested keys inside LLM JSON (e.g., `hierarchy$evil_hook` or `predictors[[1]]$extra`) passed validation or were silently dropped during normalization. | Preserved unknown keys during normalization and enforced closed key allowlists (`SPEC_ALLOWED_KEYS`) on every nested IR section, plus pure-ASCII dotted-path validation (`is_valid_rationale_key()`) on `defaults_rationale` keys and length bounds on all strings. |
| 6 | **High** | `compile_r_script()` | No post-compilation static AST verifier existed, and compiled scripts emitted `do.call(rbind, ...)`. | Replaced `do.call(rbind, ...)` with pure base loops, introduced exported `verify_compiled_r_ast()` enforcing a strict positive function allowlist (zero `eval`, `parse`, `system`, `system2`, `do.call`, `source`, `unlink`, `file.remove`, `get`, `assign`, `:::`, dynamic callees, or higher-order banned references like `lapply(x, system)`), and wired it as a mandatory post-compile gate. |
| 7 | **Medium** | `is_valid_r_ident()` | Locale-dependent `[A-Za-z]` regex without pure-ASCII byte check risked accepting Unicode homoglyphs; reserved word list omitted dangerous base symbols. | Added `is_pure_ascii()` byte check (`<= 0x7f`), `perl = TRUE`, 64-byte cap, and expanded `SPEC_RESERVED_WORDS`. |
| 8 | **Medium** | `translator_openai()` / `translator_ollama()` | Raw API keys were stored directly in the S3 list (`x$api_key`), `base_url` had no scheme/SSRF validation, and HTTP responses had no maximum byte limit. | Stored OpenAI keys in a private environment (`.auth`) with masked display (`api_key_masked`), added `sanitize_secret_error()` redaction on HTTP errors, added `validate_backend_url()` (HTTPS-only by default; loopback HTTP allowed for Ollama), and enforced `max_response_bytes` (default 512 KB) via `resp_body_raw()` prior to JSON parsing. |
| 9 | **Medium** | `diff_spec_ir()` / `mod_spec_controls_server()` | Clearing a Shiny `numericInput` (sending `NA`) or entering `Inf`/`NaN` caused `if (values_semantically_equal(...))` to crash with `missing value where TRUE/FALSE needed`, and stale guardrail errors persisted after external updates. | Hardened `values_semantically_equal()` against non-finite values, ensured invalid edits never corrupt `spec_rv` or `last_diff_rv`, and cleared `guardrail_error_rv` on valid recovery or external `spec_rv` updates. |

---

## 3. Static AST Verification Guarantee (`verify_compiled_r_ast()`)

Every script produced by `compile_r_script()` is parsed statically (AST-only, zero evaluation) and verified by `verify_compiled_r_ast(script, strict = TRUE)`:

1. **Positive Allowlist Only:** Every call head must be in `SPEC_AST_ALLOWED_BARE_CALLS`, `SPEC_AST_ALLOWED_NS_CALLS` (`base`, `stats`, `lme4`), or a local function defined inside the script (`simulate_one_dataset`, `fit_and_extract`, `run_simulation`).
2. **Zero `:::` and Zero Dynamic Callees:** Internal triple-colon access and computed callees (e.g., `(get("system"))()`, `funs[[1]]()`) are unconditionally rejected.
3. **Higher-Order Banned Symbol Blocking:** Banned symbols (`eval`, `parse`, `system`, `system2`, `do.call`, `source`, `unlink`, `file.remove`, `get`, `assign`, etc.) are rejected even when passed as arguments to higher-order functions such as `lapply("id", system)`.
4. **Package Load Allowlist:** `library()` and `requireNamespace()` are restricted strictly to `"lme4"`.

---

## 4. Reporting Security Issues

If you discover a security issue or a bypass of the IR validation, Quarto sanitization, or AST verification layers, please report it via <https://github.com/MarcusHarrisUConn/specshiny/issues> or directly to the maintainer listed in `DESCRIPTION`.
