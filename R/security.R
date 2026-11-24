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
  "writeBin", "write.table", "write.csv", "cat", "sink", "save", "save.image",
  "saveRDS", "readRDS", "load", "readLines", "read.table", "read.csv",
  "read.delim", "scan", "file", "gzfile", "bzfile", "xzfile",
  "url", "download.file", "socketConnection", "fifo", "get", "mget", "get0",
  "dynGet", "getFromNamespace", "getExportedValue", "assign", "rm", "remove",
  "attach", "detach", "dyn.load", "dyn.unload", "Sys.getenv", "Sys.setenv",
  "Sys.unsetenv", "Sys.chmod", "Sys.umask", "Sys.which", "Sys.glob",
  "options", "trace", "untrace", "debug", "debugonce", "unlockBinding",
  "lockBinding", "assignInNamespace", "fixInNamespace", "q", "quit",
  "Recall", "baseenv", "globalenv", ".GlobalEnv", "environment", "new.env", "parent.env",
  "parent.frame", "sys.frame", "sys.call", "match.call", "setHook",
  "reg.finalizer", "browser", "install.packages", "loadNamespace",
  "attachNamespace", "asNamespace", "getNamespace", ".Call", ".External",
  ".External2", ".Internal", ".Primitive", ".C", ".Fortran"
)

# Safe bare non-call value symbols permitted without local assignment
SPEC_AST_ALLOWED_VALUE_SYMBOLS <- c(
  "T", "F", "TRUE", "FALSE", "NA", "NA_real_", "NA_integer_",
  "NA_character_", "NA_complex_", "Inf", "NaN", "NULL", "pi", ".Random.seed"
)

# Strict positive allowlist of bare base/control calls emitted by compile_r_script()
SPEC_AST_ALLOWED_BARE_CALLS <- c(
  # Language & control flow
  "{", "(", "<-", "=", "if", "for", "function", "return", "~", "::", "on.exit",
  # Indexing & extraction (@ is further restricted to @optinfo only)
  "$", "@", "[", "[[", ":",
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

# Strict URL validator for LLM backends (blocks file://, gopher://, CRLF,
# non-local HTTP, invalid TCP ports, and cloud metadata / link-local SSRF targets)
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

  is_https <- grepl("^https://(\\[[0-9A-Fa-f:.]+\\]|[A-Za-z0-9._-]+)(:[0-9]{1,5})?(/[A-Za-z0-9._~/%-]*)?$", url, perl = TRUE)
  is_http_local <- grepl(
    "^http://(localhost|127\\.0\\.0\\.1|\\[::1\\])(:[0-9]{1,5})?(/[A-Za-z0-9._~/%-]*)?$",
    url, ignore.case = TRUE, perl = TRUE
  )

  if (!(is_https || (isTRUE(allow_http_local) && is_http_local))) {
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

  # Extract authority (host + optional :port)
  authority <- sub("^https?://([^/]+).*$", "\\1", url, ignore.case = TRUE, perl = TRUE)
  host <- authority
  port_str <- ""
  if (startsWith(authority, "[")) {
    m_v6 <- regexec("^\\[([^\\]]+)\\](?::([0-9]+))?$", authority, perl = TRUE)
    reg_v6 <- regmatches(authority, m_v6)[[1L]]
    if (length(reg_v6) < 2L) {
      cli::cli_abort("Malformed IPv6 authority in {.arg base_url}.", class = "specshiny_security_error")
    }
    host <- tolower(reg_v6[[2L]])
    if (length(reg_v6) >= 3L) port_str <- reg_v6[[3L]]
  } else if (grepl(":", authority, fixed = TRUE)) {
    parts_ap <- strsplit(authority, ":", fixed = TRUE)[[1L]]
    if (length(parts_ap) != 2L) {
      cli::cli_abort("Malformed host:port in {.arg base_url}.", class = "specshiny_security_error")
    }
    host <- tolower(parts_ap[[1L]])
    port_str <- parts_ap[[2L]]
  } else {
    host <- tolower(authority)
  }

  if (nzchar(port_str)) {
    port_val <- suppressWarnings(as.integer(port_str))
    if (is.na(port_val) || port_val < 1L || port_val > 65535L) {
      cli::cli_abort(
        "{.arg base_url} specifies an invalid TCP port ({.val {port_str}}); must be in 1..65535.",
        class = "specshiny_security_error"
      )
    }
  }

  # Reject cloud metadata endpoints, link-local ranges, unspecified addresses, and numeric IP obfuscations
  is_blocked_host <- grepl("^169\\.254\\.", host, perl = TRUE) ||
    grepl("^0\\.", host, perl = TRUE) ||
    grepl("^(fe80:|fd00:ec2:|::ffff:169\\.254\\.|::$)", host, perl = TRUE) ||
    host %in% c("metadata", "metadata.google.internal", "metadata.goog", "instance-data") ||
    grepl("^[0-9]+$", host, perl = TRUE) ||
    grepl("^0x[0-9a-f]+", host, perl = TRUE) ||
    grepl("^0[0-7]+\\.", host, perl = TRUE)

  if (is_blocked_host) {
    cli::cli_abort(
      "{.arg base_url} targets a restricted link-local, metadata, or obfuscated host ({.val {host}}).",
      class = "specshiny_security_error"
    )
  }

  url
}

validate_backend_model <- function(model) {
  if (!is_chr_scalar(model) || !nzchar(trimws(model))) {
    cli::cli_abort("{.arg model} must be a non-empty character scalar.", class = "specshiny_security_error")
  }
  if (!is_pure_ascii(model) || grepl("[[:cntrl:]]", model)) {
    cli::cli_abort("{.arg model} must be pure ASCII without control characters.", class = "specshiny_security_error")
  }
  cleaned <- trimws(model)
  if (grepl("[[:space:]]", cleaned) || nchar(cleaned, type = "bytes") > 256L) {
    cli::cli_abort("{.arg model} must not contain whitespace or exceed 256 characters.", class = "specshiny_security_error")
  }
  cleaned
}

validate_backend_api_key <- function(api_key) {
  if (is.null(api_key)) {
    return("")
  }
  if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key)) {
    cli::cli_abort("{.arg api_key} must be a character scalar.", class = "specshiny_security_error")
  }
  if (!nzchar(api_key)) {
    return("")
  }
  if (!is_pure_ascii(api_key) || grepl("[[:cntrl:][:space:]]", api_key)) {
    cli::cli_abort(
      "{.arg api_key} must be pure ASCII without whitespace or control characters.",
      class = "specshiny_security_error"
    )
  }
  api_key
}

validate_backend_temperature <- function(temperature) {
  if (!is_num_scalar(temperature) || temperature < 0 || temperature > 2) {
    cli::cli_abort(
      "{.arg temperature} must be a finite numeric scalar in [0, 2].",
      class = "specshiny_security_error"
    )
  }
  as.numeric(temperature)
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

is_system_function_symbol <- function(sym) {
  if (!is_chr_scalar(sym) || !nzchar(sym)) return(FALSE)
  envs <- list(baseenv(), globalenv())
  for (pkg in c("utils", "stats", "tools", "methods", "graphics", "grDevices")) {
    ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
    if (!is.null(ns)) envs[[length(envs) + 1L]] <- ns
  }
  for (env in envs) {
    if (exists(sym, envir = env, inherits = FALSE)) {
      obj <- tryCatch(get(sym, envir = env, inherits = FALSE), error = function(e) NULL)
      if (is.function(obj)) return(TRUE)
    }
  }
  FALSE
}

is_narrow_rng_exists <- function(node) {
  if (!is.call(node) || length(node) != 4L) return(FALSE)
  arg_nms <- names(node)
  if (is.null(arg_nms)) arg_nms <- rep("", length(node))
  # Expect: exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  arg1 <- node[[2L]]
  if (!(is.character(arg1) && length(arg1) == 1L && identical(arg1, ".Random.seed"))) return(FALSE)
  env_idx <- which(arg_nms == "envir")
  inh_idx <- which(arg_nms == "inherits")
  if (length(env_idx) != 1L || length(inh_idx) != 1L) return(FALSE)
  env_arg <- node[[env_idx]]
  inh_arg <- node[[inh_idx]]
  is.symbol(env_arg) && identical(as.character(env_arg), ".GlobalEnv") && identical(inh_arg, FALSE)
}

is_narrow_rng_seed_access <- function(node) {
  is.call(node) && length(node) == 3L &&
    is.symbol(node[[1L]]) && identical(as.character(node[[1L]]), "$") &&
    is.symbol(node[[2L]]) && identical(as.character(node[[2L]]), ".GlobalEnv") &&
    is.symbol(node[[3L]]) && identical(as.character(node[[3L]]), ".Random.seed")
}

#' Statically verify the AST safety of a compiled R script
#'
#' Parses an R script into an Abstract Syntax Tree (AST) without evaluating any
#' code and verifies that **every** function call belongs strictly to a closed
#' allowlist of safe statistical/base functions (or non-shadowing local
#' functions defined earlier in lexical order). Rejects all `:::`, `<<-`,
#' dynamic/computed callees, arbitrary S4 slots, arbitrary package loading,
#' extra-argument injections in `library()` / `requireNamespace()`, and
#' dangerous or unwhitelisted primitives—even when passed as higher-order
#' function arguments (e.g., to `lapply`, `vapply`, or `tryCatch`).
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

  add_viol <- function(code, symbol, message) {
    violations[[length(violations) + 1L]] <<- list(
      code = as.character(code),
      symbol = as.character(symbol),
      message = as.character(message)
    )
  }

  make_scope <- function(vars = character(), funs = character()) {
    sc <- new.env(parent = emptyenv())
    sc$vars <- vars
    sc$funs <- funs
    sc
  }

  clone_scope <- function(sc) {
    make_scope(vars = sc$vars, funs = sc$funs)
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

  # Strict positive allowlist check on higher-order callable arguments
  check_higher_order_callable <- function(arg_node, host_fn, scope) {
    if (is.null(arg_node) || identical(arg_node, quote(expr = ))) {
      add_viol("unwhitelisted_higher_order_fn", "<missing>",
               sprintf("Missing callable argument to higher-order function '%s'.", host_fn))
      return(invisible(NULL))
    }
    # 1. Inline function(...) definition (body walked separately)
    if (is.call(arg_node) && is.symbol(arg_node[[1L]]) && identical(as.character(arg_node[[1L]]), "function")) {
      return(invisible(NULL))
    }
    # 2. Allowlisted pkg::fn reference
    if (is.call(arg_node) && length(arg_node) == 3L &&
        is.symbol(arg_node[[1L]]) && identical(as.character(arg_node[[1L]]), "::")) {
      pkg <- as.character(arg_node[[2L]])
      fn <- as.character(arg_node[[3L]])
      allowed_fns <- SPEC_AST_ALLOWED_NS_CALLS[[pkg]]
      if (!(fn %in% SPEC_AST_BANNED_SYMBOLS) && !is.null(allowed_fns) && (fn %in% allowed_fns)) {
        return(invisible(NULL))
      }
      add_viol("unwhitelisted_higher_order_fn", paste0(pkg, "::", fn),
               sprintf("Higher-order callable '%s::%s' passed to '%s' is not in the static AST allowlist.", pkg, fn, host_fn))
      return(invisible(NULL))
    }
    # 3. Bare symbol strictly in allowlist or previously defined local_funs
    if (is.symbol(arg_node)) {
      sym <- as.character(arg_node)
      if (!(sym %in% SPEC_AST_BANNED_SYMBOLS) &&
          (sym %in% SPEC_AST_ALLOWED_BARE_CALLS || sym %in% scope$funs)) {
        return(invisible(NULL))
      }
      add_viol("unwhitelisted_higher_order_fn", sym,
               sprintf("Higher-order callable symbol '%s' passed to '%s' is not in the static AST allowlist.", sym, host_fn))
      return(invisible(NULL))
    }
    # 4. Anything else (strings like "system", arbitrary expressions, etc.) is forbidden
    arg_str <- deparse1(arg_node)
    add_viol("unwhitelisted_higher_order_fn", arg_str,
             sprintf("Non-allowlisted higher-order callable '%s' passed to '%s' is forbidden.", arg_str, host_fn))
    invisible(NULL)
  }

  inspect_higher_order_args <- function(node, full_fn, scope) {
    if (length(node) < 2L) return(invisible(NULL))
    arg_nms <- names(node)
    if (is.null(arg_nms)) arg_nms <- rep("", length(node))

    if (full_fn %in% c("lapply", "base::lapply", "vapply", "base::vapply")) {
      fun_pos <- which(nzchar(arg_nms) & startsWith("FUN", arg_nms))
      if (length(fun_pos) >= 1L) {
        check_higher_order_callable(node[[fun_pos[[1L]]]], full_fn, scope)
      } else {
        has_x_named <- any(arg_nms == "X")
        unnamed <- which(arg_nms[-1L] == "") + 1L
        need_pos <- if (has_x_named) 1L else 2L
        if (length(unnamed) >= need_pos) {
          check_higher_order_callable(node[[unnamed[[need_pos]]]], full_fn, scope)
        }
      }
    } else if (identical(full_fn, "stats::ave")) {
      fun_pos <- which(nzchar(arg_nms) & startsWith("FUN", arg_nms))
      if (length(fun_pos) >= 1L) {
        check_higher_order_callable(node[[fun_pos[[1L]]]], full_fn, scope)
      }
    } else if (identical(full_fn, "stats::uniroot")) {
      f_pos <- which(arg_nms == "f")
      if (length(f_pos) >= 1L) {
        check_higher_order_callable(node[[f_pos[[1L]]]], full_fn, scope)
      } else {
        unnamed <- which(arg_nms[-1L] == "") + 1L
        if (length(unnamed) >= 1L) {
          check_higher_order_callable(node[[unnamed[[1L]]]], full_fn, scope)
        }
      }
    } else if (full_fn %in% c("tryCatch", "base::tryCatch")) {
      expr_pos <- which(nzchar(arg_nms) & startsWith("expr", arg_nms))
      first_pos <- if (length(expr_pos) >= 1L) expr_pos[[1L]] else 2L
      for (k in 2L:length(node)) {
        nm_k <- arg_nms[[k]]
        if (k == first_pos || (nzchar(nm_k) && startsWith("finally", nm_k))) next
        check_higher_order_callable(node[[k]], full_fn, scope)
      }
    }
    invisible(NULL)
  }

  # Recursive lexical-scope AST walker
  walk_node <- function(node, scope, in_formula = FALSE) {
    if (is.null(node) || is.atomic(node)) {
      return(invisible(NULL))
    }

    if (is.symbol(node)) {
      sym <- as.character(node)
      if (sym %in% SPEC_AST_BANNED_SYMBOLS) {
        add_viol("banned_symbol_reference", sym,
                 sprintf("Reference to banned symbol '%s' is forbidden.", sym))
      } else if (!isTRUE(in_formula) &&
                 !(sym %in% scope$vars) &&
                 !(sym %in% scope$funs) &&
                 !(sym %in% SPEC_AST_ALLOWED_BARE_CALLS) &&
                 !(sym %in% SPEC_AST_ALLOWED_VALUE_SYMBOLS) &&
                 is_system_function_symbol(sym)) {
        add_viol("unwhitelisted_function_symbol", sym,
                 sprintf("Reference to unwhitelisted function symbol '%s' is forbidden.", sym))
      }
      return(invisible(NULL))
    }

    if (is.pairlist(node)) {
      for (k in seq_along(node)) {
        if (!identical(node[[k]], quote(expr = ))) {
          walk_node(node[[k]], scope, in_formula = in_formula)
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
          } else {
            inspect_higher_order_args(node, full_nm, scope)
          }
        } else {
          # Computed callee such as (get("system"))() or funs[[1]]()
          callee_str <- deparse1(head)
          called <<- c(called, callee_str)
          add_viol("dynamic_callee", callee_str,
                   sprintf("Dynamic/computed callee '%s' is forbidden.", callee_str))
          walk_node(head, scope, in_formula = in_formula)
        }
      } else if (is.symbol(head)) {
        fn_nm <- as.character(head)
        called <<- c(called, fn_nm)

        # Narrow RNG restoration primitive:
        # exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        if (identical(fn_nm, "exists")) {
          if (is_narrow_rng_exists(node)) {
            return(invisible(NULL))
          }
          add_viol("unwhitelisted_call", "exists",
                   "Call to 'exists' is only allowed for checking '.Random.seed' in .GlobalEnv.")
          # Still walk arguments to catch any nested attacks
          if (length(node) >= 2L) {
            for (k in 2L:length(node)) {
              if (!identical(node[[k]], quote(expr = ))) walk_node(node[[k]], scope, in_formula = in_formula)
            }
          }
          return(invisible(NULL))
        }

        if (identical(fn_nm, "::")) {
          # Bare pkg::sym value reference
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

        # Sequential lexical assignment: evaluate RHS BEFORE registering LHS (no hoisting)
        if (fn_nm %in% c("<-", "=") && length(node) == 3L) {
          lhs <- node[[2L]]
          rhs <- node[[3L]]
          walk_node(rhs, scope, in_formula = FALSE)
          if (is.symbol(lhs)) {
            lhs_nm <- as.character(lhs)
            is_fn_def <- is.call(rhs) && is.symbol(rhs[[1L]]) && identical(as.character(rhs[[1L]]), "function")
            if (lhs_nm %in% SPEC_AST_BANNED_SYMBOLS) {
              add_viol("banned_definition", lhs_nm, sprintf("Redefining banned symbol '%s' is forbidden.", lhs_nm))
            } else if (is_fn_def) {
              if (!(lhs_nm %in% SPEC_AST_ALLOWED_BARE_CALLS) && is_system_function_symbol(lhs_nm)) {
                add_viol("banned_definition", lhs_nm, sprintf("Shadowing system function '%s' is forbidden.", lhs_nm))
              } else {
                scope$funs <- unique(c(scope$funs, lhs_nm))
                scope$vars <- unique(c(scope$vars, lhs_nm))
              }
            } else {
              scope$vars <- unique(c(scope$vars, lhs_nm))
            }
          } else {
            walk_node(lhs, scope, in_formula = FALSE)
          }
          return(invisible(NULL))
        }

        # Check bare call allowlist against current lexical scope$funs
        if (fn_nm %in% SPEC_AST_BANNED_SYMBOLS) {
          add_viol("banned_call", fn_nm, sprintf("Call to banned function '%s' is forbidden.", fn_nm))
        } else if (!(fn_nm %in% SPEC_AST_ALLOWED_BARE_CALLS) && !(fn_nm %in% scope$funs)) {
          add_viol("unwhitelisted_call", fn_nm,
                   sprintf("Function call '%s' is not in the static AST allowlist.", fn_nm))
        } else {
          inspect_higher_order_args(node, fn_nm, scope)
        }

        # Special guard on library() and requireNamespace():
        # Validate package name AND recursively walk EVERY extra argument!
        if (fn_nm %in% c("library", "requireNamespace")) {
          arg_nms <- names(node)
          if (is.null(arg_nms)) arg_nms <- rep("", length(node))
          pkg_pos <- which(nzchar(arg_nms) & startsWith("package", arg_nms))
          pkg_idx <- if (length(pkg_pos) >= 1L) {
            pkg_pos[[1L]]
          } else {
            unnamed <- which(arg_nms[-1L] == "") + 1L
            if (length(unnamed) >= 1L) unnamed[[1L]] else NA_integer_
          }
          pkg_arg <- if (!is.na(pkg_idx)) node[[pkg_idx]] else NULL
          is_lit_pkg <- is.symbol(pkg_arg) || (is.character(pkg_arg) && length(pkg_arg) == 1L)
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
          if (length(node) >= 2L) {
            for (k in 2L:length(node)) {
              if (identical(k, pkg_idx) && is_lit_pkg) next
              if (!identical(node[[k]], quote(expr = ))) {
                walk_node(node[[k]], scope, in_formula = in_formula)
              }
            }
          }
          return(invisible(NULL))
        }

        # For function(args) body: walk default arg values in current scope,
        # bind formals in child scope, and walk body in child scope
        if (identical(fn_nm, "function")) {
          child_scope <- clone_scope(scope)
          if (length(node) >= 2L && is.pairlist(node[[2L]])) {
            walk_node(node[[2L]], scope, in_formula = FALSE)
            f_nms <- names(node[[2L]])
            if (!is.null(f_nms)) {
              for (fnm in f_nms) {
                if (fnm %in% SPEC_AST_BANNED_SYMBOLS) {
                  add_viol("banned_definition", fnm, sprintf("Using banned symbol '%s' as parameter name is forbidden.", fnm))
                } else if (nzchar(fnm)) {
                  child_scope$vars <- unique(c(child_scope$vars, fnm))
                }
              }
            }
          }
          if (length(node) >= 3L) {
            walk_node(node[[3L]], child_scope, in_formula = FALSE)
          }
          return(invisible(NULL))
        }

        # For for (var in seq) body: walk seq, bind loop var, walk body
        if (identical(fn_nm, "for") && length(node) == 4L) {
          walk_node(node[[3L]], scope, in_formula = FALSE)
          if (is.symbol(node[[2L]])) {
            v_nm <- as.character(node[[2L]])
            if (v_nm %in% SPEC_AST_BANNED_SYMBOLS) {
              add_viol("banned_definition", v_nm, sprintf("Using banned symbol '%s' as loop variable is forbidden.", v_nm))
            } else {
              scope$vars <- unique(c(scope$vars, v_nm))
            }
          }
          walk_node(node[[4L]], scope, in_formula = FALSE)
          return(invisible(NULL))
        }

        # For ~ formulas: walk all calls strictly, allow non-banned column symbols
        if (identical(fn_nm, "~")) {
          if (length(node) >= 2L) {
            for (k in 2L:length(node)) {
              if (!identical(node[[k]], quote(expr = ))) {
                walk_node(node[[k]], scope, in_formula = TRUE)
              }
            }
          }
          return(invisible(NULL))
        }

        # For $ access (dat$col): permit narrow .GlobalEnv$.Random.seed;
        # otherwise walk only LHS, never treat RHS column name as symbol ref
        if (identical(fn_nm, "$")) {
          if (is_narrow_rng_seed_access(node)) {
            return(invisible(NULL))
          }
          if (length(node) >= 2L) walk_node(node[[2L]], scope, in_formula = in_formula)
          return(invisible(NULL))
        }

        # For @ slot access: allow ONLY @optinfo (for lme4 convergence check), walk LHS
        if (identical(fn_nm, "@")) {
          slot_arg <- if (length(node) >= 3L) node[[3L]] else NULL
          slot_nm <- if (is.symbol(slot_arg)) {
            as.character(slot_arg)
          } else if (is.character(slot_arg) && length(slot_arg) == 1L) {
            slot_arg
          } else {
            "<dynamic>"
          }
          if (!identical(slot_nm, "optinfo")) {
            add_viol("unwhitelisted_slot", slot_nm,
                     sprintf("S4 slot access '@%s' is not in the static AST allowlist.", slot_nm))
          }
          if (length(node) >= 2L) walk_node(node[[2L]], scope, in_formula = in_formula)
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
            walk_node(node[[k]], scope, in_formula = in_formula)
          }
        }
      }
      return(invisible(NULL))
    }

    if (is.expression(node) || is.list(node)) {
      for (k in seq_along(node)) {
        walk_node(node[[k]], scope, in_formula = in_formula)
      }
    }
  }

  top_scope <- make_scope()
  for (i in seq_along(exprs)) {
    walk_node(exprs[[i]], top_scope, in_formula = FALSE)
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
