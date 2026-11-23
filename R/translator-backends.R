#' Construct the offline deterministic rule-based translator backend
#'
#' Creates a zero-dependency, offline, deterministic natural-language parser
#' that compiles English statistical and simulation study descriptions into
#' validated `spec_ir` objects, identifies underspecified parameters for
#' structured clarification, and documents inferred defaults.
#'
#' @param strict Logical; reserved for future strict-parsing extensions.
#' @return An S3 object of class `c("spec_translator_rule", "spec_translator")`.
#' @export
#' @examples
#' tr <- translator_rule()
#' res <- nl_to_spec("2-level power simulation with 50 schools, 20 students per school, ICC 0.2", tr)
#' res$spec
translator_rule <- function(strict = FALSE) {
  structure(
    list(
      name = "rule",
      strict = isTRUE(strict)
    ),
    class = c("spec_translator_rule", "spec_translator")
  )
}

#' Construct an OpenAI-compatible LLM translator backend
#'
#' Configures an `httr2`-based translator targeting any OpenAI-compatible
#' `/chat/completions` endpoint. Enforces strict URL validation (HTTPS by
#' default), API key masking in prints and error traces, bounded HTTP timeouts
#' and maximum response sizes, JSON-only parsing via `jsonlite::fromJSON()`
#' (never `eval(parse())`), and strict validation via [validate_spec_ir()].
#'
#' @param model Character model identifier (e.g., `"gpt-4o-mini"`).
#' @param api_key API key scalar; defaults to `Sys.getenv("OPENAI_API_KEY")`.
#'   Stored in a private environment and masked in printed output and errors.
#' @param base_url Base API URL (defaults to `"https://api.openai.com/v1"`).
#' @param temperature Numeric sampling temperature (defaults to `0` for reproducibility).
#' @param timeout Numeric HTTP timeout in seconds (`1` to `600`).
#' @param max_response_bytes Maximum permitted HTTP response size in bytes
#'   (defaults to `524288L`, i.e., 512 KB).
#' @param allow_http_local Logical; if `TRUE`, allows `http://localhost` or
#'   `http://127.0.0.1` endpoints for local OpenAI-compatible servers.
#'
#' @return An S3 object of class `c("spec_translator_openai", "spec_translator")`.
#' @export
#' @examples
#' tr <- translator_openai(model = "gpt-4o-mini", api_key = "sk-demo-1234567890")
#' print(tr)
translator_openai <- function(model = "gpt-4o-mini",
                              api_key = Sys.getenv("OPENAI_API_KEY"),
                              base_url = "https://api.openai.com/v1",
                              temperature = 0,
                              timeout = 60,
                              max_response_bytes = 524288L,
                              allow_http_local = FALSE) {
  if (!is_chr_scalar(model) || !nzchar(trimws(model))) {
    cli::cli_abort("{.arg model} must be a non-empty character scalar.")
  }
  clean_url <- validate_backend_url(base_url, allow_http_local = isTRUE(allow_http_local))
  clean_timeout <- validate_backend_timeout(timeout)
  clean_max_bytes <- validate_max_response_bytes(max_response_bytes)

  raw_key <- as.character(api_key %||% "")
  if (length(raw_key) != 1L || is.na(raw_key)) raw_key <- ""
  auth_env <- new.env(parent = emptyenv())
  auth_env$api_key <- raw_key

  structure(
    list(
      name = "openai",
      model = trimws(model),
      api_key_masked = mask_api_key(raw_key),
      .auth = auth_env,
      base_url = clean_url,
      temperature = as.numeric(temperature),
      timeout = clean_timeout,
      max_response_bytes = clean_max_bytes
    ),
    class = c("spec_translator_openai", "spec_translator")
  )
}

#' Construct a local Ollama LLM translator backend
#'
#' Configures an `httr2`-based translator targeting a local Ollama `/api/chat`
#' server with JSON-only output formatting, URL validation (HTTPS or local
#' loopback HTTP only), bounded timeouts and response sizes, and strict
#' validation via [validate_spec_ir()].
#'
#' @param model Local Ollama model name (e.g., `"llama3.1"`).
#' @param base_url Ollama server URL (defaults to `"http://localhost:11434"`).
#' @param temperature Numeric sampling temperature (defaults to `0`).
#' @param timeout Numeric HTTP timeout in seconds (`1` to `600`).
#' @param max_response_bytes Maximum permitted HTTP response size in bytes
#'   (defaults to `524288L`, i.e., 512 KB).
#'
#' @return An S3 object of class `c("spec_translator_ollama", "spec_translator")`.
#' @export
#' @examples
#' tr <- translator_ollama(model = "llama3.1")
#' print(tr)
translator_ollama <- function(model = "llama3.1",
                              base_url = "http://localhost:11434",
                              temperature = 0,
                              timeout = 120,
                              max_response_bytes = 524288L) {
  if (!is_chr_scalar(model) || !nzchar(trimws(model))) {
    cli::cli_abort("{.arg model} must be a non-empty character scalar.")
  }
  clean_url <- validate_backend_url(base_url, allow_http_local = TRUE)
  clean_timeout <- validate_backend_timeout(timeout)
  clean_max_bytes <- validate_max_response_bytes(max_response_bytes)

  structure(
    list(
      name = "ollama",
      model = trimws(model),
      base_url = clean_url,
      temperature = as.numeric(temperature),
      timeout = clean_timeout,
      max_response_bytes = clean_max_bytes
    ),
    class = c("spec_translator_ollama", "spec_translator")
  )
}

#' Return the strict JSON system prompt used by LLM translator backends
#'
#' @param mode One of `"create"` (new spec from prompt) or `"refine"` (modify existing spec).
#' @return Character scalar system prompt.
#' @export
spec_system_prompt <- function(mode = c("create", "refine")) {
  mode <- match.arg(mode)
  task_line <- if (mode == "create") {
    "Translate the user's statistical or simulation study description into a valid spec_ir v1.0.0 JSON object."
  } else {
    "Modify the provided spec_ir v1.0.0 JSON object according to the user's refinement instruction, preserving all untouched fields."
  }

  paste(
    "You are a neuro-symbolic statistical specification compiler for the R package specshiny.",
    task_line,
    "CRITICAL RULES:",
    "1. Return ONLY a single valid JSON object with keys 'spec' (a valid spec_ir v1.0.0 object) and 'clarifications' (array of clarification objects).",
    "2. NEVER emit executable R code, shell commands, unknown extra JSON properties, or prose outside the JSON object.",
    "3. schema_version must be '1.0.0'. design_type must be one of: monte_carlo_lm, power_analysis, multilevel_sim, two_level_mlm.",
    "4. All variable identifiers must be pure ASCII matching ^[A-Za-z][A-Za-z0-9_]*$ (<= 64 chars) and avoid R reserved words.",
    "5. Enforce statistical sanity: positive SDs, ICC in [0, 1), positive-definite corr_matrix/predictor_corr, level-1 predictors only for random slopes, missingness rate in [0, 0.95).",
    "6. Document every inferred default in spec.defaults_rationale as dotted-path -> plain-language explanation.",
    sep = "\n"
  )
}

#' @export
print.spec_translator <- function(x, ...) {
  cli::cli_text("<spec_translator:{x$name}>")
  if (!is.null(x$model)) {
    cli::cli_text("  model: {x$model} | endpoint: {x$base_url}")
    if (!is.null(x$api_key_masked)) {
      cli::cli_text("  api_key: {x$api_key_masked} | timeout: {x$timeout}s | max_bytes: {x$max_response_bytes}")
    } else {
      cli::cli_text("  timeout: {x$timeout}s | max_bytes: {x$max_response_bytes}")
    }
  }
  invisible(x)
}

# Internal S3 dispatch for backend execution
translate_prompt_backend <- function(translator, prompt) {
  UseMethod("translate_prompt_backend")
}

refine_spec_backend <- function(translator, spec, instruction) {
  UseMethod("refine_spec_backend")
}

get_openai_api_key <- function(translator) {
  if (is.environment(translator$.auth) && exists("api_key", envir = translator$.auth, inherits = FALSE)) {
    translator$.auth$api_key
  } else {
    ""
  }
}

perform_bounded_json_request <- function(req, max_bytes, secret = NULL) {
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      safe_msg <- sanitize_secret_error(conditionMessage(e), secret = secret)
      cli::cli_abort(
        c("LLM HTTP request failed.", "x" = safe_msg),
        class = "specshiny_llm_http_error"
      )
    }
  )

  raw_body <- httr2::resp_body_raw(resp)
  if (length(raw_body) > max_bytes) {
    cli::cli_abort(
      sprintf("LLM HTTP response size (%d bytes) exceeds maximum allowed (%d bytes).", length(raw_body), max_bytes),
      class = "specshiny_security_error"
    )
  }

  txt <- rawToChar(raw_body)
  Encoding(txt) <- "UTF-8"
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        c("LLM endpoint returned malformed HTTP JSON envelope.", "x" = sanitize_secret_error(conditionMessage(e), secret = secret)),
        class = "specshiny_llm_parse_error"
      )
    }
  )
}

#' @export
translate_prompt_backend.spec_translator_openai <- function(translator, prompt) {
  key <- get_openai_api_key(translator)
  if (!nzchar(key)) {
    cli::cli_abort("OpenAI translator requires a non-empty {.arg api_key} (or {.envvar OPENAI_API_KEY}).")
  }
  body <- list(
    model = translator$model,
    temperature = translator$temperature,
    response_format = list(type = "json_object"),
    messages = list(
      list(role = "system", content = spec_system_prompt("create")),
      list(role = "user", content = prompt)
    )
  )
  req <- httr2::request(paste0(translator$base_url, "/chat/completions"))
  req <- httr2::req_headers(
    req,
    Authorization = paste("Bearer", key),
    `Content-Type` = "application/json"
  )
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, translator$timeout)

  parsed <- perform_bounded_json_request(req, max_bytes = translator$max_response_bytes, secret = key)
  content <- parsed$choices[[1L]]$message$content
  unpack_llm_json_payload(content, prompt = prompt, max_bytes = translator$max_response_bytes)
}

#' @export
refine_spec_backend.spec_translator_openai <- function(translator, spec, instruction) {
  key <- get_openai_api_key(translator)
  if (!nzchar(key)) {
    cli::cli_abort("OpenAI translator requires a non-empty {.arg api_key} (or {.envvar OPENAI_API_KEY}).")
  }
  user_msg <- paste0(
    "Current spec_ir JSON:\n",
    as_spec_json(spec, pretty = TRUE),
    "\n\nRefinement instruction:\n",
    instruction
  )
  body <- list(
    model = translator$model,
    temperature = translator$temperature,
    response_format = list(type = "json_object"),
    messages = list(
      list(role = "system", content = spec_system_prompt("refine")),
      list(role = "user", content = user_msg)
    )
  )
  req <- httr2::request(paste0(translator$base_url, "/chat/completions"))
  req <- httr2::req_headers(
    req,
    Authorization = paste("Bearer", key),
    `Content-Type` = "application/json"
  )
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, translator$timeout)

  parsed <- perform_bounded_json_request(req, max_bytes = translator$max_response_bytes, secret = key)
  content <- parsed$choices[[1L]]$message$content
  unpack_llm_json_payload(content, prompt = spec$metadata$source_prompt, max_bytes = translator$max_response_bytes)
}

#' @export
translate_prompt_backend.spec_translator_ollama <- function(translator, prompt) {
  body <- list(
    model = translator$model,
    stream = FALSE,
    format = "json",
    options = list(temperature = translator$temperature),
    messages = list(
      list(role = "system", content = spec_system_prompt("create")),
      list(role = "user", content = prompt)
    )
  )
  req <- httr2::request(paste0(translator$base_url, "/api/chat"))
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, translator$timeout)

  parsed <- perform_bounded_json_request(req, max_bytes = translator$max_response_bytes)
  content <- parsed$message$content
  unpack_llm_json_payload(content, prompt = prompt, max_bytes = translator$max_response_bytes)
}

#' @export
refine_spec_backend.spec_translator_ollama <- function(translator, spec, instruction) {
  user_msg <- paste0(
    "Current spec_ir JSON:\n",
    as_spec_json(spec, pretty = TRUE),
    "\n\nRefinement instruction:\n",
    instruction
  )
  body <- list(
    model = translator$model,
    stream = FALSE,
    format = "json",
    options = list(temperature = translator$temperature),
    messages = list(
      list(role = "system", content = spec_system_prompt("refine")),
      list(role = "user", content = user_msg)
    )
  )
  req <- httr2::request(paste0(translator$base_url, "/api/chat"))
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, translator$timeout)

  parsed <- perform_bounded_json_request(req, max_bytes = translator$max_response_bytes)
  content <- parsed$message$content
  unpack_llm_json_payload(content, prompt = spec$metadata$source_prompt, max_bytes = translator$max_response_bytes)
}

unpack_llm_json_payload <- function(content, prompt = NULL, max_bytes = 524288L) {
  if (!is_chr_scalar(content) || !nzchar(trimws(content))) {
    cli::cli_abort("LLM backend returned an empty response; expected a JSON specification.")
  }
  if (nchar(content, type = "bytes") > max_bytes) {
    cli::cli_abort("LLM JSON content exceeds maximum allowed byte size.", class = "specshiny_security_error")
  }
  cleaned <- trimws(content)
  cleaned <- sub("^```(?:json)?\\s*", "", cleaned, ignore.case = TRUE)
  cleaned <- sub("\\s*```$", "", cleaned)

  raw_obj <- tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(
        c("LLM backend did not return valid JSON.", "x" = conditionMessage(e)),
        class = "specshiny_llm_parse_error"
      )
    }
  )

  if (!is.list(raw_obj)) {
    cli::cli_abort("LLM JSON response must be an object.")
  }

  if (!is.null(raw_obj$spec) && is.list(raw_obj$spec)) {
    spec_raw <- raw_obj$spec
    clarifs <- raw_obj$clarifications %||% list()
  } else {
    spec_raw <- raw_obj
    clarifs <- list()
  }

  if (is.list(spec_raw$metadata) && is.null(spec_raw$metadata$source_prompt) && !is.null(prompt)) {
    spec_raw$metadata$source_prompt <- prompt
  }

  spec <- as_spec_ir(spec_raw, validate = FALSE)
  list(
    spec = spec,
    clarifications = normalize_clarifications(clarifs),
    parsed_fields = character()
  )
}

normalize_clarifications <- function(clarifs) {
  if (is.null(clarifs) || length(clarifs) == 0L) return(list())
  lapply(seq_along(clarifs), function(i) {
    cl <- clarifs[[i]]
    list(
      id = as.character(cl$id %||% paste0("clarif_", i)),
      path = as.character(cl$path %||% ""),
      question = as.character(cl$question %||% ""),
      current_default = cl$current_default,
      options = cl$options %||% list(),
      rationale = as.character(cl$rationale %||% "")
    )
  })
}
