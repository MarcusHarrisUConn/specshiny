#' Serialize a `spec_ir` object to YAML
#'
#' @param spec A `spec_ir` object.
#' @return A character scalar containing YAML text.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' cat(as_spec_yaml(spec))
as_spec_yaml <- function(spec) {
  if (!is.list(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls spec_ir} or list.")
  }
  payload <- spec_to_serializable(spec, target = "yaml")
  yaml::as.yaml(payload, indent.mapping.sequence = TRUE)
}

#' Serialize a `spec_ir` object to JSON
#'
#' @param spec A `spec_ir` object.
#' @param pretty Logical; whether to pretty-print the JSON output.
#' @return A character scalar containing JSON text.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' cat(as_spec_json(spec))
as_spec_json <- function(spec, pretty = TRUE) {
  if (!is.list(spec)) {
    cli::cli_abort("{.arg spec} must be a {.cls spec_ir} or list.")
  }
  payload <- spec_to_serializable(spec, target = "json")
  as.character(
    jsonlite::toJSON(
      payload,
      auto_unbox = TRUE,
      null = "null",
      pretty = isTRUE(pretty),
      digits = NA
    )
  )
}

#' Parse a `spec_ir` object from YAML or JSON text
#'
#' Parses declarative IR text safely via `yaml::yaml.load()` or
#' `jsonlite::fromJSON()` (never evaluating R expressions) and normalizes it
#' into a typed `spec_ir` object.
#'
#' @param text Character scalar containing YAML or JSON text.
#' @param format One of `"yaml"` or `"json"`.
#' @param validate Logical; if `TRUE` (default), validates the parsed IR.
#' @param auto_migrate Logical; if `TRUE`, automatically migrates legacy IR
#'   versions via [migrate_spec_ir()]. Defaults to `FALSE`.
#'
#' @return A `spec_ir` object.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' txt <- as_spec_yaml(spec)
#' spec2 <- parse_spec_text(txt, format = "yaml")
parse_spec_text <- function(text, format = c("yaml", "json"), validate = TRUE, auto_migrate = FALSE) {
  format <- match.arg(format)
  if (!is_chr_scalar(text) || !nzchar(trimws(text))) {
    cli::cli_abort("{.arg text} must be a non-empty character scalar.")
  }

  raw_obj <- if (format == "yaml") {
    assert_safe_yaml_input(text)
    yaml::yaml.load(text, eval.expr = FALSE)
  } else {
    if (nchar(text, type = "bytes") > 1048576L) {
      cli::cli_abort("JSON specification exceeds maximum allowed size (1 MB).", class = "specshiny_security_error")
    }
    jsonlite::fromJSON(text, simplifyVector = FALSE)
  }

  if (isTRUE(auto_migrate) && is.list(raw_obj) &&
      is_chr_scalar(raw_obj$schema_version) &&
      !identical(raw_obj$schema_version, SPEC_SCHEMA_VERSION)) {
    return(migrate_spec_ir(raw_obj, target_version = SPEC_SCHEMA_VERSION, validate = validate))
  }

  as_spec_ir(raw_obj, validate = validate)
}

#' Write a `spec_ir` object to disk as YAML or JSON
#'
#' @param spec A `spec_ir` object.
#' @param path File path to write.
#' @param format Optional format (`"yaml"` or `"json"`). Inferred from `path`
#'   extension when `NULL`.
#' @param validate Logical; if `TRUE` (default), validates `spec` prior to writing.
#'
#' @return Invisibly returns `path`.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' tmp <- tempfile(fileext = ".yaml")
#' write_spec_ir(spec, tmp)
#' unlink(tmp)
write_spec_ir <- function(spec, path, format = NULL, validate = TRUE) {
  if (isTRUE(validate)) {
    assert_valid_spec_ir(spec)
  }
  fmt <- resolve_spec_format(path, format)
  txt <- if (fmt == "json") as_spec_json(spec, pretty = TRUE) else as_spec_yaml(spec)
  writeLines(txt, con = path, useBytes = TRUE)
  invisible(path)
}

#' Read a `spec_ir` object from a YAML or JSON file
#'
#' @param path Path to a `.yaml`, `.yml`, or `.json` specification file.
#' @param format Optional format (`"yaml"` or `"json"`). Inferred from `path`
#'   extension when `NULL`.
#' @param validate Logical; if `TRUE` (default), validates the loaded IR.
#' @param auto_migrate Logical; if `TRUE`, automatically migrates legacy IR
#'   versions via [migrate_spec_ir()]. Defaults to `FALSE`.
#'
#' @return A `spec_ir` object.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' tmp <- tempfile(fileext = ".json")
#' write_spec_ir(spec, tmp)
#' spec2 <- read_spec_ir(tmp)
#' unlink(tmp)
read_spec_ir <- function(path, format = NULL, validate = TRUE, auto_migrate = FALSE) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    cli::cli_abort("Specification file not found: {.path {path}}.")
  }
  fmt <- resolve_spec_format(path, format)
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  parse_spec_text(txt, format = fmt, validate = validate, auto_migrate = auto_migrate)
}

resolve_spec_format <- function(path, format = NULL) {
  if (!is.null(format)) {
    return(match.arg(format, c("yaml", "json")))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "json") {
    "json"
  } else if (ext %in% c("yaml", "yml")) {
    "yaml"
  } else {
    cli::cli_abort(
      "Cannot infer format from extension {.val {ext}}; specify {.arg format} as {.val yaml} or {.val json}."
    )
  }
}

spec_to_serializable <- function(spec, target = c("yaml", "json")) {
  target <- match.arg(target)
  x <- unclass(spec)

  # Convert correlation matrices to list-of-rows so YAML & JSON store clean 2D arrays
  if (is.matrix(x$predictor_corr)) {
    pcm <- x$predictor_corr
    x$predictor_corr <- lapply(seq_len(nrow(pcm)), function(i) {
      row_vals <- as.numeric(pcm[i, ])
      if (target == "json") I(row_vals) else row_vals
    })
  }
  if (is.list(x$effects) && is.list(x$effects$random) && is.matrix(x$effects$random$corr_matrix)) {
    cm <- x$effects$random$corr_matrix
    x$effects$random$corr_matrix <- lapply(seq_len(nrow(cm)), function(i) {
      row_vals <- as.numeric(cm[i, ])
      if (target == "json") I(row_vals) else row_vals
    })
  }

  # Ensure JSON keeps arrays as arrays and empty maps as objects
  if (target == "json") {
    if (is.list(x$metadata)) {
      x$metadata$tags <- I(as.character(x$metadata$tags %||% character()))
    }
    if (is.list(x$predictors)) {
      x$predictors <- I(unname(x$predictors))
    }
    if (is.list(x$effects)) {
      if (length(x$effects$fixed) == 0L) {
        x$effects$fixed <- structure(list(), names = character())
      }
      x$effects$interactions <- I(lapply(unname(x$effects$interactions %||% list()), function(it) {
        it$terms <- I(as.character(it$terms))
        it
      }))
      if (is.list(x$effects$random)) {
        x$effects$random$slopes <- I(unname(x$effects$random$slopes %||% list()))
      }
    }
    if (is.list(x$simulation)) {
      pg <- x$simulation$parameter_grid %||% list()
      if (length(pg) == 0L) {
        x$simulation$parameter_grid <- structure(list(), names = character())
      } else {
        x$simulation$parameter_grid <- lapply(pg, function(v) I(as.numeric(v)))
      }
    }
    if (length(x$defaults_rationale) == 0L) {
      x$defaults_rationale <- structure(list(), names = character())
    }
  } else {
    if (is.list(x$effects) && length(x$effects$fixed) == 0L) {
      x$effects$fixed <- structure(list(), names = character())
    }
    if (is.list(x$simulation) && length(x$simulation$parameter_grid) == 0L) {
      x$simulation$parameter_grid <- structure(list(), names = character())
    }
    if (length(x$defaults_rationale) == 0L) {
      x$defaults_rationale <- structure(list(), names = character())
    }
  }

  x
}
