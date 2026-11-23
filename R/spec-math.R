#' Synthesize mathematical and formula summaries from a `spec_ir`
#'
#' Produces deterministic R formula, plain-text, and LaTeX mathematical
#' representations of a validated `spec_ir` specification for display in
#' dynamic Shiny previews and Quarto reports.
#'
#' @param spec A valid `spec_ir` object.
#' @return A named list with elements `formula`, `text`, and `latex`.
#' @export
#' @examples
#' spec <- new_spec_ir(
#'   design_type = "multilevel_sim",
#'   hierarchy = list(levels = 2L, n_clusters = 50L, cluster_size = 20L, icc = 0.2)
#' )
#' spec_to_math(spec)
spec_to_math <- function(spec) {
  assert_valid_spec_ir(spec)

  y_name <- spec$outcome$name
  pred_names <- vapply(spec$predictors, `[[`, character(1), "name")
  fixed_rhs <- paste(pred_names, collapse = " + ")

  levels_val <- as.integer(spec$hierarchy$levels)
  rand <- spec$effects$random
  slope_preds <- vapply(rand$slopes, `[[`, character(1), "predictor")

  # R model formula
  if (levels_val == 2L && (isTRUE(rand$intercept) || length(slope_preds) > 0L)) {
    re_terms <- character()
    if (isTRUE(rand$intercept)) {
      re_terms <- c(re_terms, "1")
    } else {
      re_terms <- c(re_terms, "0")
    }
    if (length(slope_preds) > 0L) {
      re_terms <- c(re_terms, slope_preds)
    }
    re_str <- sprintf("(%s | %s)", paste(re_terms, collapse = " + "), spec$hierarchy$cluster_name)
    formula_str <- sprintf("%s ~ %s + %s", y_name, fixed_rhs, re_str)
  } else {
    formula_str <- sprintf("%s ~ %s", y_name, fixed_rhs)
  }

  # Link function notation
  link_prefix <- switch(
    spec$outcome$link,
    identity = "",
    logit = "\\text{logit}",
    probit = "\\Phi^{-1}",
    log = "\\log"
  )

  # Build fixed terms in LaTeX
  idx_sub <- if (levels_val == 2L) "ij" else "i"
  fixed_latex_terms <- vapply(seq_along(spec$predictors), function(k) {
    p <- spec$predictors[[k]]
    p_sub <- if (levels_val == 2L && identical(as.integer(p$level), 2L)) "j" else idx_sub
    b_val <- spec$effects$fixed[[p$name]] %||% 0
    pname_tex <- escape_latex_ident(p$name)
    sprintf("\\beta_{%d}\\,\\text{%s}_{%s}", k, pname_tex, p_sub)
  }, character(1))

  beta_defs <- vapply(seq_along(spec$predictors), function(k) {
    p <- spec$predictors[[k]]
    b_val <- spec$effects$fixed[[p$name]] %||% 0
    sprintf("\\beta_{%d} = %s", k, format(round(b_val, 4), trim = TRUE))
  }, character(1))

  y_tex <- escape_latex_ident(y_name)
  lines <- character()

  if (levels_val == 1L) {
    if (spec$outcome$family == "gaussian") {
      lines <- c(
        lines,
        sprintf("\\text{%s}_i &= \\beta_0 + %s + \\varepsilon_i, \\quad \\varepsilon_i \\sim \\mathcal{N}(0, %s^2)",
                y_tex, paste(fixed_latex_terms, collapse = " + "),
                format(round(spec$outcome$residual_sd, 4), trim = TRUE))
      )
    } else {
      dist_tex <- if (spec$outcome$family == "binomial") "\\text{Bernoulli}(\\mu_i)" else "\\text{Poisson}(\\mu_i)"
      lines <- c(
        lines,
        sprintf("\\text{%s}_i &\\sim %s", y_tex, dist_tex),
        sprintf("%s(\\mu_i) &= \\beta_0 + %s", link_prefix, paste(fixed_latex_terms, collapse = " + "))
      )
    }
  } else {
    # 2-level model
    re_latex_terms <- character()
    if (isTRUE(rand$intercept)) {
      re_latex_terms <- c(re_latex_terms, "u_{0j}")
    }
    for (s_idx in seq_along(slope_preds)) {
      re_latex_terms <- c(re_latex_terms, sprintf("u_{%dj}\\,\\text{%s}_{ij}", s_idx, escape_latex_ident(slope_preds[[s_idx]])))
    }
    re_part <- if (length(re_latex_terms) > 0L) paste0(" + ", paste(re_latex_terms, collapse = " + ")) else ""

    if (spec$outcome$family == "gaussian") {
      lines <- c(
        lines,
        sprintf("\\text{%s}_{ij} &= \\beta_0 + %s%s + \\varepsilon_{ij}, \\quad \\varepsilon_{ij} \\sim \\mathcal{N}(0, %s^2)",
                y_tex, paste(fixed_latex_terms, collapse = " + "), re_part,
                format(round(spec$outcome$residual_sd, 4), trim = TRUE))
      )
    } else {
      dist_tex <- if (spec$outcome$family == "binomial") "\\text{Bernoulli}(\\mu_{ij})" else "\\text{Poisson}(\\mu_{ij})"
      lines <- c(
        lines,
        sprintf("\\text{%s}_{ij} &\\sim %s", y_tex, dist_tex),
        sprintf("%s(\\mu_{ij}) &= \\beta_0 + %s%s", link_prefix, paste(fixed_latex_terms, collapse = " + "), re_part)
      )
    }

    tau0 <- spec_implied_intercept_sd(spec)
    q_rand <- as.integer(isTRUE(rand$intercept)) + length(slope_preds)
    if (q_rand == 1L && isTRUE(rand$intercept)) {
      lines <- c(
        lines,
        sprintf("u_{0j} &\\sim \\mathcal{N}(0, %s^2) \\quad (\\text{ICC} = %s, \\; J = %d, \\; n_j = %d)",
                format(round(tau0, 4), trim = TRUE),
                format(round(spec$hierarchy$icc, 4), trim = TRUE),
                as.integer(spec$hierarchy$n_clusters),
                as.integer(spec$hierarchy$cluster_size))
      )
    } else if (q_rand > 1L) {
      sd_vec <- c(
        if (isTRUE(rand$intercept)) tau0 else numeric(),
        vapply(rand$slopes, function(s) as.numeric(s$sd), numeric(1))
      )
      u_vec_names <- c(
        if (isTRUE(rand$intercept)) "u_{0j}" else character(),
        sprintf("u_{%dj}", seq_along(slope_preds))
      )
      lines <- c(
        lines,
        sprintf("(%s)^\\top &\\sim \\mathcal{N}(\\mathbf{0}, \\boldsymbol{\\Sigma}), \\quad \\text{diag}(\\boldsymbol{\\Sigma})^{1/2} = (%s)",
                paste(u_vec_names, collapse = ", "),
                paste(format(round(sd_vec, 4), trim = TRUE), collapse = ", "))
      )
    }
  }

  lines <- c(
    lines,
    sprintf("\\text{Parameters:} &\\quad \\beta_0 = %s, \\; %s",
            format(round(spec$effects$intercept, 4), trim = TRUE),
            paste(beta_defs, collapse = ", \\; "))
  )

  if (!identical(spec$missingness$mechanism, "none")) {
    miss_line <- if (identical(spec$missingness$mechanism, "MCAR")) {
      sprintf("\\text{Missingness:} &\\quad \\text{MCAR on } \\text{%s}, \\; P(R = 1) = %s",
              y_tex, format(round(spec$missingness$rate, 3), trim = TRUE))
    } else {
      sprintf("\\text{Missingness:} &\\quad \\text{MAR on } \\text{%s} \\text{ via } \\text{%s} \\; (\\text{rate} = %s, \\; \\gamma = %s)",
              y_tex, escape_latex_ident(spec$missingness$auxiliary),
              format(round(spec$missingness$rate, 3), trim = TRUE),
              format(round(spec$missingness$strength, 3), trim = TRUE))
    }
    lines <- c(lines, miss_line)
  }

  latex_str <- paste0("\\begin{aligned}\n", paste(lines, collapse = " \\\\\n"), "\n\\end{aligned}")

  # Concise plain-text summary
  hier_txt <- if (levels_val == 2L) {
    sprintf("2-level (%d %ss x %d %ss, total N = %d, ICC = %.2f)",
            spec$hierarchy$n_clusters, spec$hierarchy$cluster_name,
            spec$hierarchy$cluster_size, spec$hierarchy$unit_name,
            spec_total_n(spec), spec$hierarchy$icc)
  } else {
    sprintf("1-level (N = %d %ss)", spec$hierarchy$n_obs, spec$hierarchy$unit_name)
  }
  miss_txt <- if (identical(spec$missingness$mechanism, "none")) {
    "Complete data (no missingness)"
  } else {
    sprintf("%s missingness (%.0f%% on %s)", spec$missingness$mechanism, 100 * spec$missingness$rate, y_name)
  }
  text_str <- paste(
    sprintf("Model: %s (%s, %s link)", formula_str, spec$outcome$family, spec$outcome$link),
    sprintf("Structure: %s | Estimator: %s", hier_txt, spec$estimator$engine),
    sprintf("Focal predictor: %s (beta = %s) | %s",
            spec$effects$focal_predictor %||% "none",
            if (!is.null(spec$effects$focal_predictor)) format(spec$effects$fixed[[spec$effects$focal_predictor]]) else "NA",
            miss_txt),
    sep = "\n"
  )

  list(
    formula = formula_str,
    text = text_str,
    latex = latex_str
  )
}

escape_latex_ident <- function(x) {
  gsub("_", "\\\\_", as.character(x), fixed = TRUE)
}
