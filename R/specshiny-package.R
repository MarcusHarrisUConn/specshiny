#' specshiny: Neuro-Symbolic Specification Engine for Dynamic Shiny UIs
#'
#' `specshiny` implements a neuro-symbolic architecture for statistical Shiny
#' applications: natural-language study descriptions are compiled into a
#' strictly validated Intermediate Representation (`spec_ir`), which then
#' dynamically synthesizes only the relevant `bslib`/Shiny controls with
#' bidirectional synchronization and visual change diffs, and deterministically
#' compiles into standalone R scripts and Quarto reports without executing
#' untrusted language-model output.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL
