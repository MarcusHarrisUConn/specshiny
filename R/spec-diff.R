#' Compute a structured semantic diff between two `spec_ir` objects
#'
#' Compares two `spec_ir` specifications path-by-path and returns a structured
#' `spec_diff` report suitable for CLI auditing and live visual diff badges in
#' dynamic Shiny UIs.
#'
#' @param old Previous `spec_ir` object (or `NULL` when comparing against empty state).
#' @param new Updated `spec_ir` object.
#' @param ignore_rationale Logical; if `TRUE` (default), ignores changes in
#'   `defaults_rationale`.
#' @param ignore_metadata Logical; if `TRUE`, ignores changes in `metadata`.
#'   Defaults to `FALSE` (tracks `metadata$title` and `metadata$description`).
#'
#' @return An S3 object of class `spec_diff` with elements:
#'   - `changes`: data.frame (`path`, `section`, `field`, `change_type`,
#'     `old_value`, `new_value`, `summary`).
#'   - `changed_paths`: character vector of changed dotted paths.
#'   - `changed_sections`: unique character vector of changed top-level sections.
#'   - `n_changes`: integer number of semantic changes.
#'   - `has_changes`: logical scalar.
#' @export
#' @examples
#' s1 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
#' s2 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 250L))
#' diff_spec_ir(s1, s2)
diff_spec_ir <- function(old, new, ignore_rationale = TRUE, ignore_metadata = FALSE) {
  if (!is.list(new)) {
    cli::cli_abort("{.arg new} must be a {.cls spec_ir} or list.")
  }

  flat_old <- if (is.null(old)) list() else flatten_spec_for_diff(old, ignore_rationale, ignore_metadata)
  flat_new <- flatten_spec_for_diff(new, ignore_rationale, ignore_metadata)

  all_paths <- union(names(flat_old), names(flat_new))
  rows <- list()

  for (p in all_paths) {
    in_old <- p %in% names(flat_old)
    in_new <- p %in% names(flat_new)
    v_old <- if (in_old) flat_old[[p]] else NULL
    v_new <- if (in_new) flat_new[[p]] else NULL

    if (values_semantically_equal(v_old, v_new)) {
      next
    }

    change_type <- if (!in_old || is.null(v_old)) {
      "added"
    } else if (!in_new || is.null(v_new)) {
      "removed"
    } else {
      "modified"
    }

    parts <- strsplit(p, ".", fixed = TRUE)[[1L]]
    sec <- parts[[1L]]
    fld <- if (length(parts) > 1L) paste(parts[-1L], collapse = ".") else p
    old_str <- format_diff_val(v_old)
    new_str <- format_diff_val(v_new)
    summ <- switch(
      change_type,
      added = sprintf("%s: added (%s)", p, new_str),
      removed = sprintf("%s: removed (was %s)", p, old_str),
      modified = sprintf("%s: %s -> %s", p, old_str, new_str)
    )

    rows[[length(rows) + 1L]] <- list(
      path = p,
      section = sec,
      field = fld,
      change_type = change_type,
      old_value = old_str,
      new_value = new_str,
      summary = summ
    )
  }

  changes_df <- if (length(rows) == 0L) {
    data.frame(
      path = character(),
      section = character(),
      field = character(),
      change_type = character(),
      old_value = character(),
      new_value = character(),
      summary = character(),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      path = vapply(rows, `[[`, character(1), "path"),
      section = vapply(rows, `[[`, character(1), "section"),
      field = vapply(rows, `[[`, character(1), "field"),
      change_type = vapply(rows, `[[`, character(1), "change_type"),
      old_value = vapply(rows, `[[`, character(1), "old_value"),
      new_value = vapply(rows, `[[`, character(1), "new_value"),
      summary = vapply(rows, `[[`, character(1), "summary"),
      stringsAsFactors = FALSE
    )
  }

  structure(
    list(
      changes = changes_df,
      changed_paths = changes_df$path,
      changed_sections = unique(changes_df$section),
      n_changes = nrow(changes_df),
      has_changes = nrow(changes_df) > 0L
    ),
    class = "spec_diff"
  )
}

#' Format a concise one-line or bullet summary of a `spec_diff`
#'
#' @param diff A `spec_diff` object returned by [diff_spec_ir()].
#' @param max_items Maximum number of changes to list before truncating.
#'
#' @return A character scalar summarizing the changes.
#' @export
#' @examples
#' s1 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
#' s2 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 250L))
#' format_diff_summary(diff_spec_ir(s1, s2))
format_diff_summary <- function(diff, max_items = 6L) {
  if (!inherits(diff, "spec_diff") || !diff$has_changes) {
    return("No changes.")
  }
  n <- diff$n_changes
  shown <- utils::head(diff$changes$summary, max_items)
  suffix <- if (n > max_items) sprintf(" (+%d more)", n - max_items) else ""
  paste0(paste(shown, collapse = "; "), suffix)
}

#' @export
print.spec_diff <- function(x, ...) {
  if (!x$has_changes) {
    cli::cli_alert_info("No semantic differences between specifications.")
    return(invisible(x))
  }
  cli::cli_h3("spec_diff ({x$n_changes} change{?s} across {length(x$changed_sections)} section{?s})")
  for (i in seq_len(nrow(x$changes))) {
    sym <- switch(x$changes$change_type[[i]], added = "v", removed = "x", modified = "*")
    line_txt <- x$changes$summary[[i]]
    cli::cli_bullets(stats::setNames("{line_txt}", sym))
  }
  invisible(x)
}

flatten_spec_for_diff <- function(spec, ignore_rationale = TRUE, ignore_metadata = FALSE) {
  out <- list()

  add_val <- function(path, val) {
    if (!is.null(val)) {
      out[[path]] <<- val
    }
  }

  add_val("design_type", spec$design_type)

  if (!isTRUE(ignore_metadata) && is.list(spec$metadata)) {
    add_val("metadata.title", spec$metadata$title)
    if (nzchar(spec$metadata$description %||% "")) {
      add_val("metadata.description", spec$metadata$description)
    }
  }

  if (is.list(spec$hierarchy)) {
    for (k in c("levels", "n_obs", "n_clusters", "cluster_size", "cluster_size_dist", "cluster_size_sd", "time_points", "time_spacing", "cluster_name", "unit_name", "icc")) {
      add_val(paste0("hierarchy.", k), spec$hierarchy[[k]])
    }
  }

  if (is.list(spec$predictors)) {
    for (p in spec$predictors) {
      if (!is.list(p) || is.null(p$name)) next
      prefix <- paste0("predictors.", p$name)
      add_val(paste0(prefix, ".level"), p$level)
      add_val(paste0(prefix, ".distribution"), p$distribution)
      add_val(paste0(prefix, ".centering"), p$centering)
      add_val(paste0(prefix, ".role"), p$role)
      for (pk in c("mean", "sd", "prob", "min", "max")) {
        add_val(paste0(prefix, ".", pk), p[[pk]])
      }
    }
  }

  if (!is.null(spec$predictor_corr)) {
    add_val("predictor_corr", spec$predictor_corr)
  }

  if (is.list(spec$outcome)) {
    for (k in c("name", "family", "link", "residual_sd", "error_dist", "error_df", "error_skew", "ar1_rho")) {
      add_val(paste0("outcome.", k), spec$outcome[[k]])
    }
  }

  if (is.list(spec$effects)) {
    add_val("effects.intercept", spec$effects$intercept)
    add_val("effects.focal_predictor", spec$effects$focal_predictor)
    if (is.list(spec$effects$fixed)) {
      for (fn in names(spec$effects$fixed)) {
        add_val(paste0("effects.fixed.", fn), spec$effects$fixed[[fn]])
      }
    }
    if (is.list(spec$effects$interactions)) {
      for (it in spec$effects$interactions) {
        if (is.list(it) && length(it$terms) == 2L) {
          iname <- paste(it$terms, collapse = ":")
          add_val(paste0("effects.interactions.", iname, ".beta"), it$beta)
        }
      }
    }
    if (is.list(spec$effects$random)) {
      r <- spec$effects$random
      add_val("effects.random.intercept", r$intercept)
      add_val("effects.random.intercept_sd", r$intercept_sd)
      if (is.list(r$slopes)) {
        for (sl in r$slopes) {
          if (is.list(sl) && !is.null(sl$predictor)) {
            add_val(paste0("effects.random.slopes.", sl$predictor, ".sd"), sl$sd)
          }
        }
      }
      if (!is.null(r$corr_matrix)) {
        add_val("effects.random.corr_matrix", r$corr_matrix)
      }
    }
  }

  if (is.list(spec$missingness)) {
    for (k in c("mechanism", "rate", "target", "auxiliary", "strength")) {
      add_val(paste0("missingness.", k), spec$missingness[[k]])
    }
  }

  if (is.list(spec$estimator)) {
    for (k in c("engine", "reml", "conf_level", "missing_handling")) {
      add_val(paste0("estimator.", k), spec$estimator[[k]])
    }
  }

  if (is.list(spec$simulation)) {
    for (k in c("n_reps", "seed", "alpha")) {
      add_val(paste0("simulation.", k), spec$simulation[[k]])
    }
    if (is.list(spec$simulation$parameter_grid)) {
      for (gn in names(spec$simulation$parameter_grid)) {
        add_val(paste0("simulation.parameter_grid.", gn), spec$simulation$parameter_grid[[gn]])
      }
    }
  }

  if (!isTRUE(ignore_rationale) && is.list(spec$defaults_rationale)) {
    for (rn in names(spec$defaults_rationale)) {
      add_val(paste0("defaults_rationale.", rn), spec$defaults_rationale[[rn]])
    }
  }

  out
}

values_semantically_equal <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)
  if (is.matrix(a) && is.matrix(b)) {
    if (!identical(dim(a), dim(b))) return(FALSE)
    an <- as.numeric(a)
    bn <- as.numeric(b)
    if (any(!is.finite(an)) || any(!is.finite(bn))) {
      return(identical(an, bn))
    }
    return(isTRUE(max(abs(an - bn)) < 1e-10))
  }
  if (is.numeric(a) && is.numeric(b)) {
    if (length(a) != length(b)) return(FALSE)
    an <- as.numeric(a)
    bn <- as.numeric(b)
    if (any(!is.finite(an)) || any(!is.finite(bn))) {
      return(identical(an, bn))
    }
    return(isTRUE(max(abs(an - bn)) < 1e-10))
  }
  identical(a, b)
}

format_diff_val <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.matrix(x)) {
    rows <- apply(x, 1L, function(r) paste0("[", paste(round(r, 3), collapse = ", "), "]"))
    return(paste0("[", paste(rows, collapse = ", "), "]"))
  }
  if (is.numeric(x)) {
    fmt <- vapply(x, function(v) format(round(v, 4), trim = TRUE), character(1))
    if (length(fmt) == 1L) return(fmt)
    return(paste0("c(", paste(fmt, collapse = ", "), ")"))
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "TRUE" else "FALSE")
  }
  paste(as.character(x), collapse = ", ")
}
