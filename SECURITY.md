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

## 3. v0.5.0 Independent Peer Red-Team Audit & Hardening

Following an independent peer red-team audit of v0.4.0, every reported security and boundary finding was reproduced and fixed with permanent regression coverage in `tests/testthat/test-peer-redteam.R`:

| # | Severity | Surface | Vulnerability Found in v0.4.0 | Mitigation Implemented in v0.5.0 |
| :- | :--- | :--- | :--- | :--- |
| 1 | **Critical** | `verify_compiled_r_ast()` | Early return in `library()`/`requireNamespace()` skipped AST walking of extra arguments (`quietly =`, `lib.loc =`, `character.only =`). | Validate package argument (including named/partial `package =`) and recursively walk every other argument expression. |
| 2 | **Critical** | `verify_compiled_r_ast()` | Upfront Pass-1 `local_funs` hoisting permitted call-before-definition shadowing (`getFromNamespace <- function(...) NULL`), and higher-order arguments used a denylist rather than a positive allowlist. | Replaced Pass-1 hoisting with sequential lexical scoping, prohibited shadowing system functions, enforced positive symbol allowlisting for unbound namespace functions, and added positive callable-slot checks on `lapply`/`vapply`/`tryCatch`/`stats::ave`/`stats::uniroot` (blocking both unwhitelisted functions and string callables like `"system"`). |
| 3 | **Critical** | `cli` printers & `abort_spec_validation()` | Pre-formatted dynamic strings containing `{...}` passed directly to `cli::cli_abort()` / `cli::cli_bullets()` could trigger `glue` expression evaluation. | Switched `abort_spec_validation()` to `rlang::abort()` and passed all dynamic strings via single-pass `"{var}"` interpolation across all S3 `print()` methods. |
| 4 | **High** | `export_spec_bundle()` / `verify_spec_bundle()` | `validate_bundle_dir()` checked only lexical `..` segments and did not reject symlinked directories or symlinked bundle files (live or dangling). | Added `Sys.readlink()` symlink rejection and canonical directory containment checks on both the bundle root and all four bundle files (`spec.yaml`, `simulation.R`, `report.qmd`, `bundle_manifest.json`). |
| 5 | **High** | `spec_total_n()` / `validate_spec_ir()` / `run_spec_preview_sim()` | 32-bit integer overflow in `n_clusters * cluster_size` produced `NA_integer_`, bypassing the `is.finite(total_n_orig)` preview DoS cap. | Switched `spec_total_n()` to double arithmetic, enforced explicit sample-size upper bounds (`<= 1e7` total observations) in `validate_spec_ir()`, and hardened `run_spec_preview_sim()` to fail closed on non-finite totals. |
| 6 | **High** | `translator_openai()` / `translator_ollama()` / `validate_backend_url()` | Private `.auth` environment serialized API keys in cleartext via `saveRDS()`; `model`/`api_key`/`temperature` lacked CRLF and range validation; `validate_backend_url()` accepted link-local cloud metadata IPs and ports `> 65535`. | Moved OpenAI secrets to a session-scoped non-serializing package vault (`.auth_id` SHA-256 handle only), enforced strict ASCII/no-CRLF checks on `model` and `api_key`, bounded `temperature` in `[0, 2]`, and blocked metadata/link-local hosts (`169.254.0.0/16`, `metadata.google.internal`, `0.0.0.0`, `fe80::`, `fd00:ec2::`, numeric/hex IP obfuscations) and invalid TCP ports. |

---

## 4. Static AST Verification Guarantee (`verify_compiled_r_ast()`)

Every script produced by `compile_r_script()` is parsed statically (AST-only, zero evaluation) and verified by `verify_compiled_r_ast(script, strict = TRUE)`:

1. **Sequential Positive Allowlist Only:** Every call head must be in `SPEC_AST_ALLOWED_BARE_CALLS`, `SPEC_AST_ALLOWED_NS_CALLS` (`base`, `stats`, `lme4`), or a non-shadowing local function already defined earlier in lexical order (`simulate_one_dataset`, `fit_and_extract`, `run_simulation`).
2. **Zero `:::` and Zero Dynamic Callees:** Internal triple-colon access, super-assignment `<<-`, arbitrary S4 slots (only `@optinfo` is permitted), and computed callees (e.g., `(get("system"))()`, `funs[[1]]()`) are unconditionally rejected.
3. **Positive Higher-Order Callable & Symbol Allowlist:** Higher-order callable slots (`lapply`, `vapply`, `tryCatch`, `stats::ave`, `stats::uniroot`) accept only inline `function(...)` expressions, allowlisted `pkg::fn` references, or previously defined local/allowlisted function symbols (blocking `Sys.getenv`, `save.image`, `read.delim`, `list.files`, string callables like `"system"`, and aliases).
4. **Full-Argument Package Load Allowlist:** `library()` and `requireNamespace()` are restricted strictly to `"lme4"` and all remaining argument expressions are recursively walked.
5. **Narrow RNG Restoration Primitives Only:** `.GlobalEnv` is banned everywhere except in the exact fixed forms `exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)` and `.GlobalEnv$.Random.seed` used to preserve caller RNG state across `run_simulation()`.

---

## 5. Reporting Security Issues

If you discover a security issue or a bypass of the IR validation, Quarto sanitization, or AST verification layers, please report it via <https://github.com/MarcusHarrisUConn/specshiny/issues> or directly to the maintainer listed in `DESCRIPTION`.
