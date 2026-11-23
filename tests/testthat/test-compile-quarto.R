test_that("compile_quarto_report produces deterministic, well-structured Quarto documents", {
  res <- nl_to_spec(
    "2-level power simulation with 30 schools, 15 students per school, ICC 0.2, treatment effect 0.35, 10% MAR missingness, 10 replications"
  )
  spec <- res$spec

  q1 <- compile_quarto_report(spec)
  q2 <- compile_quarto_report(spec)
  expect_identical(q1, q2)

  # Check all required sections exist
  expect_match(q1, "^---\\n", perl = TRUE)
  expect_match(q1, "format: html", fixed = TRUE)
  expect_match(q1, "## 1. Study Design Overview", fixed = TRUE)
  expect_match(q1, "## 2. Mathematical Model Specification", fixed = TRUE)
  expect_match(q1, "\\begin{aligned}", fixed = TRUE)
  expect_match(q1, "## 3. Inferred Defaults & Audit Rationale", fixed = TRUE)
  expect_match(q1, "## 4. Validated Specification IR (YAML)", fixed = TRUE)
  expect_match(q1, "## 5. Deterministic Simulation Script & Results", fixed = TRUE)
  expect_match(q1, "knitr::kable", fixed = TRUE)

  # Writes cleanly to file
  qmd_file <- tempfile(fileext = ".qmd")
  on.exit(unlink(qmd_file), add = TRUE)
  compile_quarto_report(spec, file = qmd_file, format = "html", execute = FALSE)
  expect_true(file.exists(qmd_file))
  txt <- paste(readLines(qmd_file, warn = FALSE), collapse = "\n")
  expect_match(txt, "eval: false", fixed = TRUE)

  # Rejects unsafe format string
  expect_error(compile_quarto_report(spec, format = "html\nmalicious: true"))
})
