#' Summaries of an object of class `ar`
#'
#' Provide a summary for an object of class `ar`.
#'
#' @param model an object of class `ar`.
#' @param ... additional arguments.
#' @return The summary of an object of class `ar` with the following components: `acceptance rate`, `empirical expected value of target distribution`, `empirical variance of target distribution`, `suggested minimum c`.
#' @export
#' @method summary ar

summary.ar <- function(model, ...) {
  if (!inherits(model, "ar")){
    stop("Input must be an object of class \"ar\"")
  }

  obj_name <- deparse(substitute(model))

  cat("Summary of ", obj_name, ":\n")
  cat("----------------------\n")
  cat("     Acceptance rate: ", round(model$acceptance_rate, 4), "\n")
  cat("     Empirical expected value of target distribution: ", round(model$expected_values_f, 4), "\n")
  cat("     Empirical variance of target distribution: ", round(model$variance_f, 4), "\n")
  cat("     Suggested minimum c: ", ifelse(is.numeric(model$suggested_c), round(model$suggested_c, 4), model$suggested_c), "\n")
  cat("----------------------\n")

  invisible(model)
}
