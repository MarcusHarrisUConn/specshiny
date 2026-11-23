#' Create an undo/redo specification history stack
#'
#' Tracks validated `spec_ir` snapshots together with semantic [diff_spec_ir()]
#' transitions and human-readable labels so both programmatic pipelines and
#' dynamic Shiny modules can undo and redo visual widget tweaks and
#' conversational refinements seamlessly.
#'
#' @param initial_spec Optional initial validated `spec_ir` object.
#' @param max_entries Positive integer maximum number of snapshots retained in
#'   the stack (defaults to `50L`).
#'
#' @return An S3 object of class `spec_history`.
#' @export
#' @examples
#' s1 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
#' h <- spec_history(s1)
#' s2 <- refine_spec(s1, "set sample size to 250")$spec
#' history_push(h, s2, label = "Increase N to 250")
#' history_can_undo(h)
#' undone <- history_undo(h)
#' undone$spec$hierarchy$n_obs
spec_history <- function(initial_spec = NULL, max_entries = 50L) {
  if (!is_whole_scalar(max_entries) || as.integer(max_entries) < 2L) {
    cli::cli_abort("{.arg max_entries} must be an integer >= 2.")
  }

  st <- new.env(parent = emptyenv())
  st$entries <- list()
  st$index <- 0L
  st$max_entries <- as.integer(max_entries)

  hist <- structure(list(.state = st), class = "spec_history")
  if (!is.null(initial_spec)) {
    assert_valid_spec_ir(initial_spec)
    st$entries[[1L]] <- list(
      spec = initial_spec,
      label = "Initial specification",
      diff = diff_spec_ir(initial_spec, initial_spec)
    )
    st$index <- 1L
  }
  hist
}

#' Push a validated `spec_ir` snapshot onto a `spec_history` stack
#'
#' Ignores no-op pushes whose semantic diff against the current snapshot is
#' empty. Truncates any redo branch above the current pointer and trims oldest
#' entries if the stack exceeds `max_entries`.
#'
#' @param hist A `spec_history` object created by [spec_history()].
#' @param spec A validated `spec_ir` object.
#' @param label Optional character description of the edit (defaults to
#'   [format_diff_summary()]).
#'
#' @return Invisibly returns the `spec_diff` relative to the previous snapshot.
#' @export
#' @examples
#' h <- spec_history(new_spec_ir())
#' s2 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 300L))
#' history_push(h, s2, label = "N = 300")
#' history_summary(h)
history_push <- function(hist, spec, label = NULL) {
  assert_spec_history(hist)
  assert_valid_spec_ir(spec)

  st <- hist$.state
  if (st$index > 0L) {
    prev <- st$entries[[st$index]]$spec
    d <- diff_spec_ir(prev, spec)
    if (!d$has_changes) {
      return(invisible(d))
    }
    # Truncate any redo tail
    if (st$index < length(st$entries)) {
      st$entries <- st$entries[seq_len(st$index)]
    }
    lbl <- if (is_chr_scalar(label) && nzchar(trimws(label))) trimws(label) else format_diff_summary(d, max_items = 3L)
  } else {
    d <- diff_spec_ir(NULL, spec)
    lbl <- if (is_chr_scalar(label) && nzchar(trimws(label))) trimws(label) else "Initial specification"
  }

  st$entries[[length(st$entries) + 1L]] <- list(
    spec = spec,
    label = lbl,
    diff = d
  )
  st$index <- length(st$entries)

  if (length(st$entries) > st$max_entries) {
    overflow <- length(st$entries) - st$max_entries
    st$entries <- st$entries[-seq_len(overflow)]
    st$index <- length(st$entries)
  }

  invisible(d)
}

#' Undo the most recent change in a `spec_history` stack
#'
#' @param hist A `spec_history` object.
#' @return A list with `spec`, `diff`, `label`, `can_undo`, and `can_redo`, or
#'   `NULL` if already at the earliest snapshot.
#' @export
#' @examples
#' s1 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
#' h <- spec_history(s1)
#' history_push(h, new_spec_ir(hierarchy = list(levels = 1L, n_obs = 250L)))
#' res <- history_undo(h)
#' res$spec$hierarchy$n_obs
history_undo <- function(hist) {
  assert_spec_history(hist)
  st <- hist$.state
  if (st$index <= 1L) {
    return(NULL)
  }
  prev_spec <- st$entries[[st$index]]$spec
  st$index <- st$index - 1L
  target <- st$entries[[st$index]]
  list(
    spec = target$spec,
    diff = diff_spec_ir(prev_spec, target$spec),
    label = target$label,
    can_undo = st$index > 1L,
    can_redo = st$index < length(st$entries)
  )
}

#' Redo a previously undone change in a `spec_history` stack
#'
#' @param hist A `spec_history` object.
#' @return A list with `spec`, `diff`, `label`, `can_undo`, and `can_redo`, or
#'   `NULL` if already at the newest snapshot.
#' @export
#' @examples
#' s1 <- new_spec_ir(hierarchy = list(levels = 1L, n_obs = 100L))
#' h <- spec_history(s1)
#' history_push(h, new_spec_ir(hierarchy = list(levels = 1L, n_obs = 250L)))
#' history_undo(h)
#' res <- history_redo(h)
#' res$spec$hierarchy$n_obs
history_redo <- function(hist) {
  assert_spec_history(hist)
  st <- hist$.state
  if (st$index < 1L || st$index >= length(st$entries)) {
    return(NULL)
  }
  prev_spec <- st$entries[[st$index]]$spec
  st$index <- st$index + 1L
  target <- st$entries[[st$index]]
  list(
    spec = target$spec,
    diff = diff_spec_ir(prev_spec, target$spec),
    label = target$label,
    can_undo = st$index > 1L,
    can_redo = st$index < length(st$entries)
  )
}

#' Inspect undo/redo availability and current snapshot in a `spec_history`
#'
#' @param hist A `spec_history` object.
#' @return Logical scalar for `history_can_undo()` and `history_can_redo()`, or
#'   the current `spec_ir` (or `NULL`) for `history_current()`.
#' @export
#' @examples
#' h <- spec_history(new_spec_ir())
#' history_can_undo(h)
#' history_can_redo(h)
#' history_current(h)$hierarchy$n_obs
history_can_undo <- function(hist) {
  assert_spec_history(hist)
  hist$.state$index > 1L
}

#' @rdname history_can_undo
#' @export
history_can_redo <- function(hist) {
  assert_spec_history(hist)
  st <- hist$.state
  st$index >= 1L && st$index < length(st$entries)
}

#' @rdname history_can_undo
#' @export
history_current <- function(hist) {
  assert_spec_history(hist)
  st <- hist$.state
  if (st$index < 1L) return(NULL)
  st$entries[[st$index]]$spec
}

#' Summarize all snapshots stored in a `spec_history` stack
#'
#' @param hist A `spec_history` object.
#' @return A `data.frame` with columns `index`, `is_current`, `label`, and `n_changes`.
#' @export
#' @examples
#' h <- spec_history(new_spec_ir())
#' history_push(h, new_spec_ir(hierarchy = list(levels = 1L, n_obs = 350L)), label = "N=350")
#' history_summary(h)
history_summary <- function(hist) {
  assert_spec_history(hist)
  st <- hist$.state
  n <- length(st$entries)
  if (n == 0L) {
    return(data.frame(index = integer(), is_current = logical(), label = character(), n_changes = integer(), stringsAsFactors = FALSE))
  }
  data.frame(
    index = seq_len(n),
    is_current = seq_len(n) == st$index,
    label = vapply(st$entries, `[[`, character(1), "label"),
    n_changes = vapply(st$entries, function(e) as.integer(e$diff$n_changes), integer(1)),
    stringsAsFactors = FALSE
  )
}

assert_spec_history <- function(hist) {
  if (!inherits(hist, "spec_history") || !is.environment(hist$.state)) {
    cli::cli_abort("{.arg hist} must be a {.cls spec_history} object created by {.fn spec_history}.")
  }
}

#' @export
print.spec_history <- function(x, ...) {
  st <- x$.state
  cli::cli_text(
    "<spec_history: {length(st$entries)} snapshot{?s}, position {st$index}/{length(st$entries)} | undo: {history_can_undo(x)}, redo: {history_can_redo(x)}>"
  )
  invisible(x)
}
