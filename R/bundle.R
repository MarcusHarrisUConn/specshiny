#' Export a self-contained reproducible study bundle from a validated `spec_ir`
#'
#' Writes a path-safe reproducible study directory containing:
#' - `spec.yaml`: the validated declarative `spec_ir` specification;
#' - `simulation.R`: the deterministic, AST-verified standalone R script;
#' - `report.qmd`: the sanitized reproducible Quarto report; and
#' - `bundle_manifest.json`: SHA-256 checksums for every exported file plus the
#'   static AST safety attestation from [verify_compiled_r_ast()].
#'
#' @param spec A validated `spec_ir` object.
#' @param dir Character path to the target bundle directory (created if needed).
#'   Must not contain `..` path-traversal segments.
#' @param overwrite Logical; if `FALSE` (default), refuses to overwrite existing
#'   bundle files in `dir`.
#' @param format Quarto report output format (defaults to `"html"`).
#' @param execute_report Logical; whether Quarto report chunks default to
#'   `eval: true` (default) or `eval: false`.
#'
#' @return Invisibly returns a named list with `dir`, `files`, `manifest`, and
#'   `ast_verification`.
#' @export
#' @examples
#' spec <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 120L),
#'                     simulation = list(n_reps = 10L, seed = 42L))
#' out_dir <- file.path(tempdir(), "specshiny_bundle_example")
#' res <- export_spec_bundle(spec, out_dir, overwrite = TRUE)
#' verify_spec_bundle(out_dir)$valid
#' unlink(out_dir, recursive = TRUE)
export_spec_bundle <- function(spec,
                               dir,
                               overwrite = FALSE,
                               format = "html",
                               execute_report = TRUE) {
  assert_valid_spec_ir(spec)
  clean_dir <- validate_bundle_dir(dir)

  if (!dir.exists(clean_dir)) {
    dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(clean_dir)) {
    cli::cli_abort("Could not create bundle directory: {.path {clean_dir}}.")
  }
  if (is_symlink_path(clean_dir)) {
    cli::cli_abort("Bundle directory {.path {clean_dir}} must not be a symbolic link.", class = "specshiny_security_error")
  }
  canon_dir <- normalizePath(clean_dir, winslash = "/", mustWork = TRUE)

  target_names <- c("spec.yaml", "simulation.R", "report.qmd", "bundle_manifest.json")
  target_paths <- stats::setNames(file.path(canon_dir, target_names), target_names)

  # Fail closed on any pre-existing symlink (live or dangling) or out-of-root target
  for (fn in target_names) {
    assert_safe_bundle_file_target(target_paths[[fn]], canon_dir, fn)
  }

  if (!isTRUE(overwrite)) {
    existing <- target_names[file.exists(target_paths)]
    if (length(existing) > 0L) {
      cli::cli_abort(
        "Refusing to overwrite existing bundle file(s) in {.path {canon_dir}}: {.file {existing}}. Pass {.code overwrite = TRUE} to replace."
      )
    }
  }

  # Generate all deterministic artifacts first (fail closed before writing)
  r_script <- compile_r_script(spec, include_header = TRUE, run_on_source = TRUE)
  ast_res <- verify_compiled_r_ast(r_script, strict = TRUE)
  qmd_doc <- compile_quarto_report(spec, format = format, execute = execute_report)

  write_spec_ir(spec, target_paths[["spec.yaml"]], format = "yaml", validate = TRUE)
  writeLines(r_script, con = target_paths[["simulation.R"]], useBytes = TRUE)
  writeLines(qmd_doc, con = target_paths[["report.qmd"]], useBytes = TRUE)

  artifact_names <- c("spec.yaml", "simulation.R", "report.qmd")
  file_entries <- lapply(artifact_names, function(fn) {
    fp <- target_paths[[fn]]
    list(
      bytes = as.integer(file.info(fp)$size),
      sha256 = digest::digest(file = fp, algo = "sha256")
    )
  })
  names(file_entries) <- artifact_names

  pkg_ver <- tryCatch(
    as.character(utils::packageVersion("specshiny")),
    error = function(e) "0.5.2"
  )

  manifest <- list(
    bundle_format_version = "1.0.0",
    specshiny_version = pkg_ver,
    schema_version = spec$schema_version,
    design_type = spec$design_type,
    title = sanitize_single_line(spec$metadata$title, 300L),
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    ast_verification = list(
      safe = isTRUE(ast_res$safe),
      n_expressions = as.integer(ast_res$n_expressions),
      n_unique_calls = length(ast_res$called_functions),
      violations = nrow(ast_res$violations)
    ),
    files = file_entries
  )

  manifest_json <- as.character(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE))
  writeLines(manifest_json, con = target_paths[["bundle_manifest.json"]], useBytes = TRUE)

  invisible(list(
    dir = clean_dir,
    files = target_paths,
    manifest = manifest,
    ast_verification = ast_res
  ))
}

#' Verify integrity, schema validity, and AST safety of an exported study bundle
#'
#' Reads `bundle_manifest.json` from `dir`, verifies SHA-256 digests for every
#' artifact (`spec.yaml`, `simulation.R`, `report.qmd`), validates `spec.yaml`
#' with [validate_spec_ir()], and statically verifies `simulation.R` with
#' [verify_compiled_r_ast()].
#'
#' @param dir Character path to an exported bundle directory.
#' @return An S3 object of class `spec_bundle_verification` with elements
#'   `valid`, `issues`, `manifest`, `spec_validation`, and `ast_verification`.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' out_dir <- file.path(tempdir(), "specshiny_verify_example")
#' export_spec_bundle(spec, out_dir, overwrite = TRUE)
#' verify_spec_bundle(out_dir)
#' unlink(out_dir, recursive = TRUE)
verify_spec_bundle <- function(dir) {
  clean_dir <- validate_bundle_dir(dir)
  issues <- character()

  if (!dir.exists(clean_dir)) {
    return(structure(
      list(valid = FALSE, issues = "Bundle directory does not exist.", manifest = NULL,
           spec_validation = NULL, ast_verification = NULL),
      class = "spec_bundle_verification"
    ))
  }
  canon_dir <- normalizePath(clean_dir, winslash = "/", mustWork = TRUE)

  manifest_path <- file.path(canon_dir, "bundle_manifest.json")
  if (is_symlink_path(manifest_path) || (file.exists(manifest_path) && !is_path_within_dir(manifest_path, canon_dir))) {
    return(structure(
      list(valid = FALSE, issues = "bundle_manifest.json is a symbolic link or escapes the bundle directory (forbidden).",
           manifest = NULL, spec_validation = NULL, ast_verification = NULL),
      class = "spec_bundle_verification"
    ))
  }
  if (!file.exists(manifest_path)) {
    return(structure(
      list(valid = FALSE, issues = "Missing bundle_manifest.json.", manifest = NULL,
           spec_validation = NULL, ast_verification = NULL),
      class = "spec_bundle_verification"
    ))
  }

  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(e) {
      issues <<- c(issues, paste("Malformed bundle_manifest.json:", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(manifest)) {
    return(structure(
      list(valid = FALSE, issues = issues, manifest = NULL,
           spec_validation = NULL, ast_verification = NULL),
      class = "spec_bundle_verification"
    ))
  }

  # Verify symlink safety, canonical containment, and SHA-256 checksums for every artifact
  safe_files <- stats::setNames(rep(FALSE, 3L), c("spec.yaml", "simulation.R", "report.qmd"))
  for (fn in c("spec.yaml", "simulation.R", "report.qmd")) {
    fp <- file.path(canon_dir, fn)
    if (is_symlink_path(fp)) {
      issues <- c(issues, sprintf("Bundle file '%s' is a symbolic link (forbidden).", fn))
      next
    }
    if (!file.exists(fp)) {
      issues <- c(issues, sprintf("Missing required bundle file '%s'.", fn))
      next
    }
    if (!is_path_within_dir(fp, canon_dir)) {
      issues <- c(issues, sprintf("Bundle file '%s' resolves outside the bundle directory (forbidden).", fn))
      next
    }
    safe_files[[fn]] <- TRUE
    expected_sha <- manifest$files[[fn]]$sha256 %||% ""
    actual_sha <- digest::digest(file = fp, algo = "sha256")
    if (!identical(expected_sha, actual_sha)) {
      issues <- c(issues, sprintf("SHA-256 mismatch for '%s' (tampered or modified).", fn))
    }
  }

  # Validate spec.yaml
  spec_val <- NULL
  spec_path <- file.path(canon_dir, "spec.yaml")
  if (isTRUE(safe_files[["spec.yaml"]])) {
    spec_obj <- tryCatch(read_spec_ir(spec_path, validate = FALSE), error = function(e) e)
    if (inherits(spec_obj, "error")) {
      issues <- c(issues, paste("Failed to parse spec.yaml:", conditionMessage(spec_obj)))
    } else {
      spec_val <- validate_spec_ir(spec_obj, strict = FALSE)
      if (!spec_val$valid) {
        issues <- c(issues, sprintf("spec.yaml failed validation (%d errors).", nrow(spec_val$errors)))
      }
    }
  }

  # Statically verify simulation.R AST
  ast_val <- NULL
  sim_path <- file.path(canon_dir, "simulation.R")
  if (isTRUE(safe_files[["simulation.R"]])) {
    code_lines <- readLines(sim_path, warn = FALSE, encoding = "UTF-8")
    ast_val <- verify_compiled_r_ast(code_lines, strict = FALSE)
    if (!ast_val$safe) {
      issues <- c(issues, sprintf("simulation.R failed static AST verification (%d violations).", nrow(ast_val$violations)))
    }
  }

  structure(
    list(
      valid = length(issues) == 0L,
      issues = issues,
      manifest = manifest,
      spec_validation = spec_val,
      ast_verification = ast_val
    ),
    class = "spec_bundle_verification"
  )
}

#' @export
print.spec_bundle_verification <- function(x, ...) {
  if (x$valid) {
    cli::cli_alert_success("Bundle verification PASSED (SHA-256 checksums, spec_ir validation, and static AST check all clean).")
  } else {
    cli::cli_alert_danger("Bundle verification FAILED ({length(x$issues)} issue{?s}):")
    for (iss in x$issues) {
      cli::cli_bullets(c("x" = "{iss}"))
    }
  }
  invisible(x)
}

same_canon_path <- function(a, b) {
  a_clean <- sub("/+$", "", gsub("\\\\", "/", a))
  b_clean <- sub("/+$", "", gsub("\\\\", "/", b))
  if (identical(.Platform$OS.type, "windows")) {
    identical(tolower(a_clean), tolower(b_clean))
  } else {
    identical(a_clean, b_clean)
  }
}

is_root_path <- function(norm_path) {
  identical(norm_path, "/") || grepl("^[A-Za-z]:/?$", norm_path)
}

trim_trailing_slash <- function(path) {
  norm_slashes <- gsub("\\\\", "/", path)
  if (identical(norm_slashes, "/")) {
    "/"
  } else if (grepl("^[A-Za-z]:/?$", norm_slashes)) {
    paste0(substr(norm_slashes, 1L, 2L), "/")
  } else {
    sub("/+$", "", norm_slashes)
  }
}

is_symlink_path <- function(path) {
  if (!is_chr_scalar(path) || !nzchar(path)) return(FALSE)
  trimmed <- trim_trailing_slash(path)
  if (is_root_path(trimmed)) return(FALSE)

  # 1. POSIX readlink (catches both live and dangling symlinks on macOS/Linux)
  lnk <- tryCatch(Sys.readlink(trimmed), error = function(e) "")
  if (is.character(lnk) && length(lnk) == 1L && !is.na(lnk) && nzchar(lnk)) {
    return(TRUE)
  }

  # 2. Cross-platform canonical resolution check (catches Windows NTFS symlinks & junctions)
  if (file.exists(trimmed) || dir.exists(trimmed)) {
    parent_raw <- dirname(trimmed)
    leaf_nm <- basename(trimmed)
    if (!identical(parent_raw, trimmed) && nzchar(leaf_nm) && !(leaf_nm %in% c(".", ".."))) {
      canon_parent <- tryCatch(normalizePath(parent_raw, winslash = "/", mustWork = TRUE), error = function(e) "")
      canon_actual <- tryCatch(normalizePath(trimmed, winslash = "/", mustWork = TRUE), error = function(e) "")
      if (nzchar(canon_parent) && nzchar(canon_actual)) {
        expected_path <- if (identical(canon_parent, "/")) {
          paste0("/", leaf_nm)
        } else {
          paste0(sub("/+$", "", canon_parent), "/", leaf_nm)
        }
        if (!same_canon_path(canon_actual, expected_path)) {
          return(TRUE)
        }
      }
    }
  }

  # 3. Windows dangling symlink check:
  # On Windows, a broken symlink either (a) appears in list.files() while file.exists() is FALSE,
  # OR (b) makes file.exists() return TRUE while normalizePath() falls back unchanged and
  # opening the target for reading fails because the target does not exist.
  if (identical(.Platform$OS.type, "windows") && !dir.exists(trimmed)) {
    parent_raw <- dirname(trimmed)
    leaf_nm <- basename(trimmed)
    in_dir_listing <- !identical(parent_raw, trimmed) && dir.exists(parent_raw) && nzchar(leaf_nm) &&
      (tolower(leaf_nm) %in% tolower(tryCatch(list.files(parent_raw, all.files = TRUE, no.. = TRUE), error = function(e) character())))
    if (in_dir_listing) {
      if (!file.exists(trimmed)) {
        return(TRUE)
      }
      can_open <- suppressWarnings(tryCatch(
        {
          con <- file(trimmed, open = "rb")
          close(con)
          TRUE
        },
        error = function(e) FALSE
      ))
      if (!isTRUE(can_open)) {
        return(TRUE)
      }
    }
  }

  FALSE
}

canonicalize_bundle_path <- function(trimmed) {
  if (dir.exists(trimmed) || file.exists(trimmed)) {
    return(normalizePath(trimmed, winslash = "/", mustWork = TRUE))
  }
  cur <- trimmed
  suffix <- character()
  repeat {
    parent <- dirname(cur)
    if (identical(parent, cur) || !nzchar(parent)) {
      return(normalizePath(trimmed, winslash = "/", mustWork = FALSE))
    }
    suffix <- c(basename(cur), suffix)
    cur <- parent
    if (dir.exists(cur) || file.exists(cur)) {
      canon_anc <- normalizePath(cur, winslash = "/", mustWork = TRUE)
      anc_clean <- if (identical(canon_anc, "/")) "" else sub("/+$", "", canon_anc)
      return(paste(c(anc_clean, suffix), collapse = "/"))
    }
  }
}

is_path_within_dir <- function(file_path, canon_dir) {
  canon_fp <- tryCatch(normalizePath(file_path, winslash = "/", mustWork = TRUE), error = function(e) "")
  canon_root <- tryCatch(normalizePath(canon_dir, winslash = "/", mustWork = TRUE), error = function(e) "")
  if (!nzchar(canon_fp) || !nzchar(canon_root)) return(FALSE)
  same_canon_path(dirname(canon_fp), canon_root)
}

assert_safe_bundle_file_target <- function(file_path, canon_dir, filename) {
  if (is_symlink_path(file_path)) {
    cli::cli_abort(
      "Refusing to write through symbolic link {.file {filename}} inside bundle directory.",
      class = "specshiny_security_error"
    )
  }
  if (file.exists(file_path) && !is_path_within_dir(file_path, canon_dir)) {
    cli::cli_abort(
      "Refusing to write bundle file {.file {filename}} resolving outside bundle directory.",
      class = "specshiny_security_error"
    )
  }
  invisible(TRUE)
}

validate_bundle_dir <- function(dir) {
  if (!is_chr_scalar(dir) || !nzchar(trimws(dir))) {
    cli::cli_abort("{.arg dir} must be a non-empty directory path.", class = "specshiny_security_error")
  }
  if (grepl("[[:cntrl:]]", dir)) {
    cli::cli_abort("{.arg dir} must not contain control characters.", class = "specshiny_security_error")
  }
  norm_slashes <- gsub("\\\\", "/", dir)
  parts <- strsplit(norm_slashes, "/", fixed = TRUE)[[1L]]
  if (any(parts == "..")) {
    cli::cli_abort("{.arg dir} must not contain '..' path-traversal segments.", class = "specshiny_security_error")
  }
  trimmed <- trim_trailing_slash(norm_slashes)
  if (is_symlink_path(trimmed)) {
    cli::cli_abort("Bundle directory {.path {dir}} must not be a symbolic link.", class = "specshiny_security_error")
  }
  canonicalize_bundle_path(trimmed)
}
