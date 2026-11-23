#' Migrate a `spec_ir` object to the current IR schema version
#'
#' Performs forward-compatible schema migration from earlier IR versions (e.g.,
#' `"1.0.0"`) to the current canonical schema version (`"1.1.0"`), populating
#' newly introduced fields with explicit backwards-compatible defaults and
#' attaching an audit trail in `attr(result, "migration_log")`. Provides crisp
#' version-mismatch diagnostics when given an unrecognized or newer future
#' schema version.
#'
#' @param spec A `spec_ir` object or named list deserialized from YAML/JSON.
#' @param target_version Character scalar target schema version (defaults to
#'   `"1.1.0"`).
#' @param validate Logical; if `TRUE` (default), validates the migrated spec
#'   with [assert_valid_spec_ir()].
#'
#' @return A validated `spec_ir` object at `target_version`, with a character
#'   vector `attr(result, "migration_log")` recording applied migration steps.
#' @export
#' @examples
#' legacy <- new_spec_ir(validate = FALSE)
#' legacy$schema_version <- "1.0.0"
#' legacy$hierarchy$cluster_size_dist <- NULL
#' legacy$outcome$error_dist <- NULL
#' legacy$effects$interactions <- NULL
#' upgraded <- migrate_spec_ir(legacy)
#' upgraded$schema_version
#' attr(upgraded, "migration_log")
migrate_spec_ir <- function(spec, target_version = SPEC_SCHEMA_VERSION, validate = TRUE) {
  if (!is.list(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls spec_ir} or named list.", class = "specshiny_version_mismatch")
  }
  if (!is_chr_scalar(target_version) || !(target_version %in% SPEC_SUPPORTED_SCHEMA_VERSIONS)) {
    cli::cli_abort(
      "Unsupported {.arg target_version} {.val {target_version}}. Supported versions: {.val {SPEC_SUPPORTED_SCHEMA_VERSIONS}}.",
      class = "specshiny_version_mismatch"
    )
  }

  from_ver <- spec$schema_version
  if (!is_chr_scalar(from_ver) || !grepl("^\\d+\\.\\d+\\.\\d+$", from_ver, perl = TRUE)) {
    cli::cli_abort(
      "Cannot migrate specification: missing or malformed {.field schema_version} ({.val {as.character(from_ver %||% 'NULL')}}).",
      class = "specshiny_version_mismatch"
    )
  }

  cmp <- utils::compareVersion(from_ver, target_version)
  if (cmp > 0L) {
    cli::cli_abort(
      c(
        sprintf("Cannot downgrade specification from schema_version '%s' to '%s'.", from_ver, target_version),
        "i" = "This specification was created by a newer version of specshiny; please upgrade specshiny."
      ),
      class = "specshiny_version_mismatch"
    )
  }

  if (!(from_ver %in% SPEC_SUPPORTED_SCHEMA_VERSIONS)) {
    cli::cli_abort(
      c(
        sprintf("Unsupported legacy schema_version '%s' (supported: %s).",
                from_ver, paste(SPEC_SUPPORTED_SCHEMA_VERSIONS, collapse = ", ")),
        "i" = "Recreate the specification with new_spec_ir() or nl_to_spec()."
      ),
      class = "specshiny_version_mismatch"
    )
  }

  if (cmp == 0L) {
    out <- as_spec_ir(spec, validate = validate)
    attr(out, "migration_log") <- character()
    return(out)
  }

  # Migrate 1.0.0 -> 1.1.0 cleanly
  s <- unclass(spec)
  log_entries <- character()

  if (identical(from_ver, "1.0.0") && identical(target_version, "1.1.0")) {
    s$schema_version <- "1.1.0"
    log_entries <- c(log_entries, "Updated schema_version from '1.0.0' to '1.1.0'.")

    if (is.list(s$hierarchy)) {
      lv <- as.integer(s$hierarchy$levels %||% 1L)
      if (!("cluster_size_dist" %in% names(s$hierarchy))) {
        s$hierarchy$cluster_size_dist <- if (lv == 2L) "fixed" else NULL
        log_entries <- c(log_entries, "Initialized hierarchy$cluster_size_dist for balanced clusters.")
      }
      if (!("cluster_size_sd" %in% names(s$hierarchy))) {
        s$hierarchy$cluster_size_sd <- NULL
      }
    }

    if (!("predictor_corr" %in% names(s))) {
      s$predictor_corr <- NULL
      log_entries <- c(log_entries, "Initialized optional predictor_corr = NULL (independent predictors).")
    }

    if (is.list(s$outcome)) {
      fam <- s$outcome$family %||% "gaussian"
      if (!("error_dist" %in% names(s$outcome))) {
        s$outcome$error_dist <- if (identical(fam, "gaussian")) "normal" else NULL
        log_entries <- c(log_entries, "Initialized outcome$error_dist to default Gaussian normal errors.")
      }
      if (!("error_df" %in% names(s$outcome))) s$outcome$error_df <- NULL
      if (!("error_skew" %in% names(s$outcome))) s$outcome$error_skew <- NULL
    }

    if (is.list(s$effects) && !("interactions" %in% names(s$effects))) {
      s$effects$interactions <- list()
      log_entries <- c(log_entries, "Initialized effects$interactions = list().")
    }
  }

  out <- as_spec_ir(s, validate = validate)
  attr(out, "migration_log") <- log_entries
  out
}
