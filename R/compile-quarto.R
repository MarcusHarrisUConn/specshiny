#' Deterministically compile a validated `spec_ir` into a Quarto report (`.qmd`)
#'
#' Generates a clean, self-contained Quarto markdown document combining a
#' structured study overview, LaTeX mathematical model specification, an
#' auditable table of inferred defaults and rationales, the validated YAML IR,
#' and the deterministic executable simulation script produced by
#' [compile_r_script()].
#'
#' @param spec A validated `spec_ir` object.
#' @param file Optional file path; when provided, writes the `.qmd` report to
#'   `file` and returns the document text invisibly.
#' @param format Quarto output format (e.g., `"html"`, `"pdf"`, `"docx"`).
#' @param execute Logical; whether Quarto code chunks should execute on render
#'   (`eval: true` vs `eval: false`). Defaults to `TRUE`.
#'
#' @return A character scalar containing the Quarto `.qmd` document (invisibly
#'   when `file` is provided).
#' @export
#' @examples
#' spec <- new_spec_ir(
#'   design_type = "power_analysis",
#'   hierarchy = list(levels = 2L, n_clusters = 40L, cluster_size = 20L, icc = 0.2),
#'   simulation = list(n_reps = 50L, seed = 2026L)
#' )
#' qmd <- compile_quarto_report(spec)
#' cat(substr(qmd, 1, 500))
compile_quarto_report <- function(spec,
                                  file = NULL,
                                  format = "html",
                                  execute = TRUE) {
  assert_valid_spec_ir(spec)

  if (!is_chr_scalar(format) || !grepl("^[a-zA-Z0-9_-]+$", format)) {
    cli::cli_abort("{.arg format} must be a simple Quarto format name such as {.val html} or {.val pdf}.")
  }

  math_info <- spec_to_math(spec)
  r_code <- compile_r_script(spec, include_header = FALSE, run_on_source = TRUE)
  yaml_ir <- as_spec_yaml(spec)

  title_clean <- sanitize_comment(spec$metadata$title)
  eval_flag <- if (isTRUE(execute)) "true" else "false"
  levels_val <- as.integer(spec$hierarchy$levels)

  # Build YAML front matter safely
  front_matter <- paste(
    "---",
    sprintf("title: %s", yaml_quote_scalar(title_clean)),
    sprintf("subtitle: \"Reproducible Statistical Simulation Report (specshiny IR v%s)\"", spec$schema_version),
    sprintf("format: %s", format),
    "execute:",
    "  echo: true",
    "  warning: false",
    "  message: false",
    sprintf("  eval: %s", eval_flag),
    "---",
    sep = "\n"
  )

  # Design overview table
  hier_desc <- if (levels_val == 2L) {
    sprintf("2-level (%d %ss x %d %ss; total N = %d; ICC = %s)",
            spec$hierarchy$n_clusters, spec$hierarchy$cluster_name,
            spec$hierarchy$cluster_size, spec$hierarchy$unit_name,
            spec_total_n(spec), format(spec$hierarchy$icc))
  } else {
    sprintf("1-level (N = %d %ss)", spec$hierarchy$n_obs, spec$hierarchy$unit_name)
  }

  miss_desc <- if (identical(spec$missingness$mechanism, "none")) {
    "None (complete data)"
  } else if (identical(spec$missingness$mechanism, "MCAR")) {
    sprintf("MCAR (%.1f%% on `%s`)", 100 * spec$missingness$rate, spec$outcome$name)
  } else {
    sprintf("MAR (%.1f%% on `%s`, driven by `%s`, slope = %s)",
            100 * spec$missingness$rate, spec$outcome$name,
            spec$missingness$auxiliary, format(spec$missingness$strength))
  }

  pg <- spec$simulation$parameter_grid %||% list()
  grid_desc <- if (length(pg) > 0L) {
    paste(sprintf("`%s`: [%s]", names(pg), vapply(pg, function(v) paste(v, collapse = ", "), character(1))), collapse = "; ")
  } else {
    "Single design cell"
  }

  overview_tbl <- paste(
    "## 1. Study Design Overview",
    "",
    if (!is.null(spec$metadata$source_prompt) && nzchar(spec$metadata$source_prompt)) {
      paste0("> **Originating Prompt:** ", sanitize_comment(spec$metadata$source_prompt), "\n")
    } else {
      ""
    },
    "| Component | Specification |",
    "| :--- | :--- |",
    sprintf("| **Design Type** | `%s` |", spec$design_type),
    sprintf("| **Data Hierarchy** | %s |", hier_desc),
    sprintf("| **Outcome** | `%s` (`%s`, `%s` link) |", spec$outcome$name, spec$outcome$family, spec$outcome$link),
    sprintf("| **Focal Predictor** | `%s` (true effect = %s) |",
            spec$effects$focal_predictor %||% spec$predictors[[1L]]$name,
            format(spec$effects$fixed[[spec$effects$focal_predictor %||% spec$predictors[[1L]]$name]])),
    sprintf("| **Estimator** | `%s` (conf. level = %s) |", spec$estimator$engine, format(spec$estimator$conf_level)),
    sprintf("| **Missingness** | %s |", miss_desc),
    sprintf("| **Monte Carlo Setup** | %d replications, alpha = %s, seed = %d |",
            as.integer(spec$simulation$n_reps), format(spec$simulation$alpha), as.integer(spec$simulation$seed)),
    sprintf("| **Parameter Grid** | %s |", grid_desc),
    sep = "\n"
  )

  # Mathematical specification section
  math_sec <- paste(
    "## 2. Mathematical Model Specification",
    "",
    sprintf("**Model Formula:** `%s`", math_info$formula),
    "",
    "$$",
    math_info$latex,
    "$$",
    sep = "\n"
  )

  # Defaults rationale section
  rat <- spec$defaults_rationale %||% list()
  if (length(rat) > 0L) {
    rat_rows <- vapply(names(rat), function(k) {
      sprintf("| `%s` | %s |", sanitize_comment(k), sanitize_comment(rat[[k]]))
    }, character(1))
    rationale_sec <- paste(
      c(
        "## 3. Inferred Defaults & Audit Rationale",
        "",
        "Every default assumption inferred during specification synthesis is documented below for transparency:",
        "",
        "| IR Path | Rationale |",
        "| :--- | :--- |",
        rat_rows
      ),
      collapse = "\n"
    )
  } else {
    rationale_sec <- paste(
      "## 3. Inferred Defaults & Audit Rationale",
      "",
      "All parameters were explicitly specified; no inferred defaults were required.",
      sep = "\n"
    )
  }

  # Validated IR YAML section
  ir_sec <- paste(
    "## 4. Validated Specification IR (YAML)",
    "",
    "::: {.callout-note collapse=\"true\"}",
    "### View Complete Validated `spec_ir` YAML",
    "",
    "```yaml",
    trimws(yaml_ir),
    "```",
    ":::",
    sep = "\n"
  )

  # Executable simulation code chunk
  code_sec <- paste(
    "## 5. Deterministic Simulation Script & Results",
    "",
    "The simulation script below was deterministically compiled from the validated IR above with zero evaluation of untrusted language-model strings.",
    "",
    "```{r}",
    "#| label: run-simulation",
    trimws(r_code),
    "```",
    sep = "\n"
  )

  # Optional summary table chunk
  results_sec <- paste(
    "```{r}",
    "#| label: results-table",
    "knitr::kable(results$summary, digits = 4, caption = \"Monte Carlo Simulation Summary\")",
    "```",
    sep = "\n"
  )

  doc <- paste(
    front_matter,
    overview_tbl,
    math_sec,
    rationale_sec,
    ir_sec,
    code_sec,
    results_sec,
    sep = "\n\n"
  )
  doc <- paste0(doc, "\n")

  if (!is.null(file)) {
    writeLines(doc, con = file, useBytes = TRUE)
    return(invisible(doc))
  }
  doc
}

yaml_quote_scalar <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  paste0("\"", x, "\"")
}
