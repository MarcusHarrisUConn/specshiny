# ==============================================================================
# Centralized Security, Sanitization, Escaping, and Static AST Verification
# ==============================================================================

# Expanded reserved words, dangerous base symbols, and internal compiler names
SPEC_RESERVED_WORDS <- c(
  "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
  "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
  "NA_complex_", "NA_character_", "T", "F", "c", "q", "t", "I",
  "return", "switch", "eval", "evalq", "parse", "system", "system2", "shell",
  "do.call", "source", "get", "mget", "assign", "unlink", "file", "library",
  "require", "requireNamespace", "options", "stop", "warning",
  "cluster_id", "unit_id", "row_id", "eta", "mu", "prob_miss",
  "params", "cfg", "dat", "dat_cc", "results", "Z_l1", "Z_l2"
)

# Dangerous symbols strictly forbidden anywhere in compiled R ASTs
SPEC_AST_BANNED_SYMBOLS <- c(
  "eval", "evalq", "eval.parent", "local", "parse", "str2lang", "str2expression",
  "system", "system2", "shell", "shell.exec", "pipe", "do.call", "match.fun",
  "source", "sys.source", "unlink", "file.remove", "file.create", "file.copy",
  "file.rename", "file.append", "dir.create", "writeLines", "writeChar",
  "writeBin", "write.table", "write.csv", "cat", "sink", "save", "saveRDS",
  "readRDS", "load", "readLines", "scan", "file", "gzfile", "bzfile", "xzfile",
  "url", "download.file", "socketConnection", "fifo", "get", "mget", "get0",
  "dynGet", "assign", "rm", "remove", "attach", "detach", "dyn.load",
  "dyn.unload", "Sys.setenv", "Sys.unsetenv", "Sys.chmod", "Sys.umask",
  "options", "trace", "untrace", "debug", "debugonce", "unlockBinding",
  "lockBinding", "assignInNamespace", "fixInNamespace", "q", "quit",
  "Recall", "baseenv", "globalenv", "environment", "new.env", "parent.env",
  "setHook", "reg.finalizer", "on.exit", "browser"
)

# Strict positive allowlist of bare base/control calls emitted by compile_r_script()
SPEC_AST_ALLOWED_BARE_CALLS <- c(
  # Language & control flow
  "{", "(", "<-", "=", "if", "for", "function", "return", "~", "::",
  # Indexing & extraction
  "$", "[", "[[", ":",
  # Arithmetic, comparison, logical & matrix operators
  "+", "-", "*", "/", "^", "%%", "%/%", "%*%",
  "==", "!=", "<", "<=", ">", ">=", "&", "&&", "|", "||", "!",
  # Safe pure base/data/math/simulation primitives
  "c", "list", "data.frame", "matrix", "diag", "chol", "cbind", "rbind",
  "rep", "seq_len", "seq_along", "length", "nrow", "ncol", "names", "which",
  "factor", "as.integer", "as.numeric", "as.character", "is.finite", "is.na",
  "isTRUE", "is.null", "is.matrix", "expand.grid", "vector",
  "logical", "numeric", "integer", "character", "summary",
  "mean", "sqrt", "exp", "log", "abs", "pmin", "pmax", "round", "floor", "ceiling",
  "sum", "sign", "unname", "set.seed", "RNGkind", "tryCatch", "lapply", "vapply",
  "suppressMessages", "stop", "print", "requireNamespace", "library"
)

# Strict positive allowlist of namespace-qualified calls (pkg::fn)
SPEC_AST_ALLOWED_NS_CALLS <- list(
  base = c(
    "c", "list", "data.frame", "matrix", "diag", "chol", "cbind", "rbind",
    "rep", "seq_len", "seq_along", "length", "nrow", "ncol", "names", "which",
    "factor", "as.integer", "as.numeric", "is.finite", "isTRUE", "mean", "sqrt",
    "exp", "abs", "pmin", "pmax", "round", "sum", "sign", "unname", "set.seed",
    "RNGkind", "tryCatch", "lapply", "vapply", "suppressMessages", "stop", "print"
  ),
  stats = c(
    "rnorm", "rbinom", "runif", "rpois", "rt", "rgamma",
    "plogis", "pnorm", "qnorm", "qt", "qlogis", "uniroot", "sd", "ave",
    "lm", "glm", "gaussian", "binomial", "poisson", "na.omit", "coef",
    "summary", "as.formula", "setNames"
  ),
  lme4 = c(
    "lmer", "glmer", "isSingular"
  )
)

SPEC_AST_ALLOWED_PACKAGES <- c("lme4")

# Dangerous LaTeX macros that must never survive into Quarto/LaTeX output
SPEC_DANGEROUS_LATEX_RE <- paste0(
  "\\\\(write18|immediate|input|include|openout|openin|closeout|read|",
  "catcode|csname|endcsname|def|edef|xdef|gdef|let|futurelet|",
  "usepackage|RequirePackage|verbatiminput|lstinputlisting|ShellEscape|",
  "directlua|luaexec|pdffiledump|pdffilemoddate|special)"
)

# Check pure ASCII (rejects Unicode homoglyphs, zero-width chars, non-ASCII bytes)
is_pure_ascii <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  raw_bytes <- charToRaw(enc2utf8(x))
  length(raw_bytes) == 0L || all(raw_bytes <= as.raw(0x7f))
}

# Strict ASCII R identifier allowlist
is_valid_r_ident <- function(x) {
  is_chr_scalar(x) &&
    is_pure_ascii(x) &&
    nchar(x, type = "bytes") >= 1L &&
    nchar(x, type = "bytes") <= 64L &&
    grepl("^[A-Za-z][A-Za-z0-9_]*$", x, perl = TRUE) &&
    !(x %in% SPEC_RESERVED_WORDS)
}

# Strict ASCII dotted IR path key validator for defaults_rationale
is_valid_rationale_key <- function(x) {
  is_chr_scalar(x) &&
    is_pure_ascii(x) &&
    nchar(x, type = "bytes") >= 1L &&
    nchar(x, type = "bytes") <= 128L &&
    grepl("^[A-Za-z][A-Za-z0-9_.]*$", x, perl = TRUE) &&
    !grepl("\\.\\.", x, perl = TRUE) &&
    !grepl("\\.$", x, perl = TRUE)
}

# Strip all ASCII control chars (NUL, CR, LF, TAB, ESC) and collapse whitespace
sanitize_single_line <- function(x, max_chars = 500L) {
  x <- as.character(x %||% "")
  if (length(x) == 0L || is.na(x[[1L]])) return("")
  x <- x[[1L]]
  x <- gsub("[[:cntrl:]]+", " ", x)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  if (nchar(x) > max_chars) {
    x <- substr(x, 1L, max_chars)
  }
  x
}

# Safe single-line comment sanitizer for R scripts (also neutralizes backticks,
# HTML tags, and dangerous LaTeX macros so embedded R scripts in Quarto stay clean)
sanitize_r_comment <- function(x, max_chars = 300L) {
  s <- sanitize_single_line(x, max_chars = max_chars)
  s <- gsub("`", "'", s, fixed = TRUE)
  s <- gsub("<", "(", s, fixed = TRUE)
  s <- gsub(">", ")", s, fixed = TRUE)
  s <- gsub(SPEC_DANGEROUS_LATEX_RE, "LATEX_BLOCKED_\\1", s, ignore.case = TRUE, perl = TRUE)
  s
}

# Safe prose/table-cell sanitizer for Quarto markdown (blocks knitr inline `r`,
# markdown fences, HTML/XSS tags, table breakouts, and dangerous LaTeX macros)
sanitize_quarto_prose <- function(x, max_chars = 500L) {
  s <- sanitize_single_line(x, max_chars = max_chars)
  # Neutralize ALL backticks (blocks inline `r ...`, `{{r}} ...`, and ``` fences)
  s <- gsub("`", "'", s, fixed = TRUE)
  # Neutralize tilde fences and Quarto fenced div markers
  s <- gsub("~{3,}", "-", s, perl = TRUE)
  s <- gsub(":{3,}", ":", s, perl = TRUE)
  # Neutralize YAML frontmatter separators
  s <- gsub("---+", "--", s, perl = TRUE)
  # Escape HTML angle brackets (blocks raw HTML / XSS inside HTML Quarto output)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  # Neutralize dangerous LaTeX commands and raw backslashes (blocks \write18, \input, etc.)
  s <- gsub(SPEC_DANGEROUS_LATEX_RE, "LATEX_BLOCKED_\\1", s, ignore.case = TRUE, perl = TRUE)
  s <- gsub("\\", "/", s, fixed = TRUE)
  # Escape markdown table cell pipes
  s <- gsub("|", "\\|", s, fixed = TRUE)
  s
}

# Safe quoted YAML frontmatter scalar (for Quarto YAML header)
sanitize_yaml_frontmatter_scalar <- function(x, max_chars = 200L) {
  s <- sanitize_quarto_prose(x, max_chars = max_chars)
  s <- gsub("!expr\\b", "expr", s, ignore.case = TRUE, perl = TRUE)
  s <- gsub("\"", "'", s, fixed = TRUE)
  paste0("\"", s, "\"")
}

# Sanitize embedded YAML IR block inside Quarto so it can NEVER close a fence
# or inject executable chunks/knitr inline code
sanitize_yaml_block_for_quarto <- function(yaml_txt) {
  lines <- strsplit(as.character(yaml_txt), "\r?\n", perl = TRUE)[[1L]]
  cleaned <- vapply(lines, function(ln) {
    # Replace backtick runs, tilde runs, angle brackets, and dangerous LaTeX macros
    ln <- gsub("`", "'", ln, fixed = TRUE)
    ln <- gsub("~{3,}", "---", ln, perl = TRUE)
    ln <- gsub(":{3,}", "::", ln, perl = TRUE)
    ln <- gsub("<", "&lt;", ln, fixed = TRUE)
    ln <- gsub(">", "&gt;", ln, fixed = TRUE)
    ln <- gsub(SPEC_DANGEROUS_LATEX_RE, "LATEX_BLOCKED_\\1", ln, ignore.case = TRUE, perl = TRUE)
    ln
  }, character(1))
  paste(cleaned, collapse = "\n")
}

# Guard raw YAML text before yaml::yaml.load() (reject !expr / oversized payloads)
assert_safe_yaml_input <- function(text, max_bytes = 1048576L) {
  if (!is_chr_scalar(text) || !nzchar(trimws(text))) {
    cli::cli_abort("{.arg text} must be a non-empty character scalar.")
  }
  n_bytes <- nchar(text, type = "bytes")
  if (n_bytes > max_bytes) {
    cli::cli_abort(
      "YAML specification exceeds maximum allowed size ({n_bytes} > {max_bytes} bytes).",
      class = "specshiny_security_error"
    )
  }
  if (grepl("(^|\\s)!(?:expr|r)\\b", text, ignore.case = TRUE, perl = TRUE)) {
    cli::cli_abort(
      "YAML expression tags ({.code !expr} / {.code !r}) are forbidden for security.",
      class = "specshiny_security_error"
    )
  }
  invisible(TRUE)
}

# Safe LaTeX identifier escaping (strictly validates ASCII identifier first)
escape_latex_ident <- function(x) {
  if (!is_valid_r_ident(x)) {
    cli::cli_abort("Refusing to emit non-identifier {.val {x}} into LaTeX math.", class = "specshiny_security_error")
  }
  gsub("_", "\\\\_", as.character(x), fixed = TRUE)
}

# API key masking helper (never exposes raw secrets in logs, prints, or errors)
mask_api_key <- function(key) {
  if (!is_chr_scalar(key) || !nzchar(key)) {
    return("<unset>")
  }
  n <- nchar(key)
  if (n < 12L) {
    return("****")
  }
  paste0(substr(key, 1L, 3L), "...", substr(key, n - 3L, n))
}

# Redact any occurrence of a secret key from an error message
sanitize_secret_error <- function(msg, secret = NULL) {
  msg <- as.character(msg %||% "")
  if (is_chr_scalar(secret) && nzchar(secret)) {
    msg <- gsub(secret, "[REDACTED]", msg, fixed = TRUE)
  }
  # Also redact Bearer tokens generically
  msg <- gsub("Bearer\\s+[A-Za-z0-9._~+/=-]+", "Bearer [REDACTED]", msg, ignore.case = TRUE, perl = TRUE)
  msg
}

# Strict URL validator for LLM backends (blocks file://, gopher://, CRLF, non-local HTTP)
validate_backend_url <- function(url, allow_http_local = FALSE) {
  if (!is_chr_scalar(url) || !nzchar(trimws(url))) {
    cli::cli_abort("{.arg base_url} must be a non-empty URL string.", class = "specshiny_security_error")
  }
  if (!is_pure_ascii(url) || grepl("[[:cntrl:][:space:]]", url)) {
    cli::cli_abort("{.arg base_url} must be pure ASCII without whitespace or control characters.", class = "specshiny_security_error")
  }
  url <- sub("/+$", "", url)
  if (grepl("@", url, fixed = TRUE)) {
    cli::cli_abort("{.arg base_url} must not embed credentials ({.code user:pass@}).", class = "specshiny_security_error")
  }

  is_https <- grepl("^https://[A-Za-z0-9._-]+(:[0-9]{1,5})?(/[A-Za-z0-9._~/%-]*)?$", url, perl = TRUE)
  is_http_local <- grepl(
    "^http://(localhost|127\\.0\\.0\\.1|\\[::1\\])(:[0-9]{1,5})?(/[A-Za-z0-9._~/%-]*)?$",
    url, ignore.case = TRUE, perl = TRUE
  )

  if (is_https || (isTRUE(allow_http_local) && is_http_local)) {
    return(url)
  }

  if (isTRUE(allow_http_local)) {
    cli::cli_abort(
      "{.arg base_url} must use {.code https://} or local {.code http://localhost} / {.code http://127.0.0.1}.",
      class = "specshiny_security_error"
    )
  } else {
    cli::cli_abort(
      "{.arg base_url} must use {.code https://} (got {.val {url}}).",
      class = "specshiny_security_error"
    )
  }
}

validate_backend_timeout <- function(timeout) {
  if (!is_num_scalar(timeout) || timeout < 1 || timeout > 600) {
    cli::cli_abort("{.arg timeout} must be a finite number between 1 and 600 seconds.", class = "specshiny_security_error")
  }
  as.numeric(timeout)
}

validate_max_response_bytes <- function(max_bytes) {
  if (!is_num_scalar(max_bytes) || max_bytes < 1024 || max_bytes > 10485760) {
    cli::cli_abort("{.arg max_response_bytes} must be between 1024 bytes and 10 MB.", class = "specshiny_security_error")
  }
  as.integer(max_bytes)
}

#' Statically verify the AST safety of a compiled R script
#'
#' Parses an R script into an Abstract Syntax Tree (AST) without evaluating any
#' code and verifies that **every** function call belongs strictly to a closed
#' allowlist of safe statistical/base functions (or functions defined within the
#' script itself). Rejects all `:::`, dynamic/computed callees, arbitrary
#' package loading, and dangerous primitives (`eval`, `parse`, `system`,
#' `system2`, `do.call`, `source`, `unlink`, `file.remove`, `get`, `assign`,
#' etc.)—even when passed as higher-order function arguments (e.g., to `lapply`).
#'
#' @param script Character scalar (or vector of lines) containing R source code.
#' @param strict Logical; if `TRUE`, aborts with a `specshiny_ast_violation`
#'   error when any violation is detected. Defaults to `FALSE`.
#'
#' @return An S3 object of class `spec_ast_verification` with elements:
#'   - `safe`: logical scalar (`TRUE` if zero violations).
#'   - `violations`: data.frame with columns `code`, `symbol`, `message`.
#'   - `called_functions`: unique character vector of all called symbols.
#'   - `n_expressions`: integer number of top-level parsed expressions.
#' @export
#' @examples
#' spec <- new_spec_ir()
#' code <- compile_r_script(spec)
#' verify_compiled_r_ast(code)
#'
#' # Unsafe scripts are caught statically without ever being executed:
#' verify_compiled_r_ast("x <- 1; system('id')")$safe
verify_compiled_r_ast <- function(script, strict = FALSE) {
  if (!is.character(script) || length(script) < 1L || any(is.na(script))) {
    cli::cli_abort("{.arg script} must be a non-empty character vector.")
  }
  code_txt <- paste(script, collapse = "\n")

  violations <- list()
  called <- character()
  local_funs <- character()

  add_viol <- function(code, symbol, message) {
    violations[[length(violations) + 1L]] <<- list(
      code = as.character(code),
      symbol = as.character(symbol),
      message = as.character(message)
    )
  }

  build_out <- function(n_expr = 0L) {
    vdf <- if (length(violations) == 0L) {
      data.frame(code = character(), symbol = character(), message = character(), stringsAsFactors = FALSE)
    } else {
      data.frame(
        code = vapply(violations, `[[`, character(1), "code"),
        symbol = vapply(violations, `[[`, character(1), "symbol"),
        message = vapply(violations, `[[`, character(1), "message"),
        stringsAsFactors = FALSE
      )
    }
    res <- structure(
      list(
        safe = nrow(vdf) == 0L,
        violations = vdf,
        called_functions = sort(unique(called)),
        n_expressions = as.integer(n_expr)
      ),
      class = "spec_ast_verification"
    )
    if (isTRUE(strict) && !res$safe) {
      bullets <- stats::setNames(
        sprintf("[%s | %s] %s", res$violations$code, res$violations$symbol, res$violations$message),
        rep("x", nrow(res$violations))
      )
      rlang::abort(
        c("Compiled R script failed static AST safety verification:", bullets),
        class = "specshiny_ast_violation"
      )
    }
    res
  }

  exprs <- tryCatch(
    parse(text = code_txt, keep.source = FALSE),
    error = function(e) e
  )
  if (inherits(exprs, "error")) {
    add_viol("parse_error", "<syntax>", sprintf("Script is not syntactically valid R: %s", conditionMessage(exprs)))
    return(build_out(0L))
  }

  # First pass: discover top-level local function definitions (`fn <- function(...) ...`)
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (is.call(e) && length(e) == 3L &&
        is.symbol(e[[1L]]) && as.character(e[[1L]]) %in% c("<-", "=") &&
        is.symbol(e[[2L]]) &&
        is.call(e[[3L]]) && is.symbol(e[[3L]][[1L]]) && identical(as.character(e[[3L]][[1L]]), "function")) {
      fn_nm <- as.character(e[[2L]])
      if (fn_nm %in% SPEC_AST_BANNED_SYMBOLS) {
        add_viol("banned_definition", fn_nm, sprintf("Redefining banned symbol '%s' is forbidden.", fn_nm))
      } else {
        local_funs <- unique(c(local_funs, fn_nm))
      }
    }
  }

  # Recursive AST walker
  walk_node <- function(node) {
    if (is.null(node) || is.atomic(node)) {
      return(invisible(NULL))
    }

    if (is.symbol(node)) {
      sym <- as.character(node)
      # Catch higher-order references to banned functions (e.g., lapply(x, system))
      if (sym %in% SPEC_AST_BANNED_SYMBOLS) {
        add_viol("banned_symbol_reference", sym,
                 sprintf("Reference to banned symbol '%s' is forbidden.", sym))
      }
      return(invisible(NULL))
    }

    if (is.pairlist(node)) {
      for (k in seq_along(node)) {
        if (!identical(node[[k]], quote(expr = ))) {
          walk_node(node[[k]])
        }
      }
      return(invisible(NULL))
    }

    if (is.call(node)) {
      head <- node[[1L]]

      # Case A: Triple-colon internal access pkg:::fn is strictly forbidden
      if (is.symbol(head) && identical(as.character(head), ":::")) {
        sym_str <- paste0(deparse1(node[[2L]]), ":::", deparse1(node[[3L]]))
        called <<- c(called, sym_str)
        add_viol("disallowed_triple_colon", sym_str, "Internal namespace access ':::' is forbidden.")
        return(invisible(NULL))
      }

      # Case B: Namespace-qualified call pkg::fn(...)
      if (is.call(head) && length(head) == 3L && is.symbol(head[[1L]])) {
        op <- as.character(head[[1L]])
        if (identical(op, ":::")) {
          sym_str <- paste0(deparse1(head[[2L]]), ":::", deparse1(head[[3L]]))
          called <<- c(called, sym_str)
          add_viol("disallowed_triple_colon", sym_str, "Internal namespace access ':::' is forbidden.")
        } else if (identical(op, "::")) {
          pkg <- as.character(head[[2L]])
          fn <- as.character(head[[3L]])
          full_nm <- paste0(pkg, "::", fn)
          called <<- c(called, full_nm)
          allowed_fns <- SPEC_AST_ALLOWED_NS_CALLS[[pkg]]
          if (fn %in% SPEC_AST_BANNED_SYMBOLS) {
            add_viol("banned_call", full_nm, sprintf("Call to banned function '%s' is forbidden.", full_nm))
          } else if (is.null(allowed_fns) || !(fn %in% allowed_fns)) {
            add_viol("unwhitelisted_call", full_nm,
                     sprintf("Namespace call '%s' is not in the static AST allowlist.", full_nm))
          }
        } else {
          # Computed callee such as (get("system"))() or funs[[1]]()
          callee_str <- deparse1(head)
          called <<- c(called, callee_str)
          add_viol("dynamic_callee", callee_str,
                   sprintf("Dynamic/computed callee '%s' is forbidden.", callee_str))
          walk_node(head)
        }
      } else if (is.symbol(head)) {
        fn_nm <- as.character(head)
        called <<- c(called, fn_nm)

        if (identical(fn_nm, "::")) {
          # Bare pkg::sym value reference (e.g., stats::rnorm passed to something)
          pkg <- as.character(node[[2L]])
          fn <- as.character(node[[3L]])
          full_nm <- paste0(pkg, "::", fn)
          allowed_fns <- SPEC_AST_ALLOWED_NS_CALLS[[pkg]]
          if (fn %in% SPEC_AST_BANNED_SYMBOLS || is.null(allowed_fns) || !(fn %in% allowed_fns)) {
            add_viol("unwhitelisted_symbol", full_nm,
                     sprintf("Namespace symbol '%s' is not in the static AST allowlist.", full_nm))
          }
          return(invisible(NULL))
        }

        if (fn_nm %in% SPEC_AST_BANNED_SYMBOLS) {
          add_viol("banned_call", fn_nm, sprintf("Call to banned function '%s' is forbidden.", fn_nm))
        } else if (!(fn_nm %in% SPEC_AST_ALLOWED_BARE_CALLS) && !(fn_nm %in% local_funs)) {
          add_viol("unwhitelisted_call", fn_nm,
                   sprintf("Function call '%s' is not in the static AST allowlist.", fn_nm))
        }

        # Special guard on library() and requireNamespace(): allow ONLY whitelisted packages
        if (fn_nm %in% c("library", "requireNamespace")) {
          pkg_arg <- if (length(node) >= 2L) node[[2L]] else NULL
          pkg_name <- if (is.symbol(pkg_arg)) {
            as.character(pkg_arg)
          } else if (is.character(pkg_arg) && length(pkg_arg) == 1L) {
            pkg_arg
          } else {
            "<dynamic>"
          }
          if (!(pkg_name %in% SPEC_AST_ALLOWED_PACKAGES)) {
            add_viol("unwhitelisted_package", pkg_name,
                     sprintf("Loading package '%s' is not allowed in compiled scripts.", pkg_name))
          }
          return(invisible(NULL))
        }

        # For function(args) body: walk default arg values and body cleanly
        if (identical(fn_nm, "function")) {
          if (length(node) >= 2L) walk_node(node[[2L]])
          if (length(node) >= 3L) walk_node(node[[3L]])
          return(invisible(NULL))
        }

        # For $ access (dat$col): walk only LHS, never treat RHS column name as symbol ref
        if (identical(fn_nm, "$")) {
          if (length(node) >= 2L) walk_node(node[[2L]])
          return(invisible(NULL))
        }
      } else {
        # Literal/anonymous callee e.g. ("system")()
        callee_str <- deparse1(head)
        called <<- c(called, callee_str)
        add_viol("dynamic_callee", callee_str, sprintf("Dynamic callee '%s' is forbidden.", callee_str))
      }

      # Walk all call arguments safely (skipping missing/empty subscript args like x[g, ])
      if (length(node) >= 2L) {
        for (k in 2L:length(node)) {
          if (!identical(node[[k]], quote(expr = ))) {
            walk_node(node[[k]])
          }
        }
      }
      return(invisible(NULL))
    }

    if (is.expression(node) || is.list(node)) {
      for (k in seq_along(node)) {
        walk_node(node[[k]])
      }
    }
  }

  for (i in seq_along(exprs)) {
    walk_node(exprs[[i]])
  }

  build_out(length(exprs))
}

#' @export
print.spec_ast_verification <- function(x, ...) {
  if (x$safe) {
    cli::cli_alert_success(
      "AST safety verification PASSED ({x$n_expressions} expressions, {length(x$called_functions)} unique whitelisted calls)."
    )
  } else {
    cli::cli_alert_danger("AST safety verification FAILED ({nrow(x$violations)} violation{?s}):")
    for (i in seq_len(nrow(x$violations))) {
      v_line <- sprintf("[%s | %s] %s", x$violations$code[[i]], x$violations$symbol[[i]], x$violations$message[[i]])
      cli::cli_bullets(c("x" = "{v_line}"))
    }
  }
  invisible(x)
}
