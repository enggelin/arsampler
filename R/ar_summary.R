#' Summaries of an object of class `ar`
#'
#' Provide a summary for an object of class `ar`.
#'
#' @param object an object of class `ar`.
#' @return The summary of an object of class `ar` with the following components: `acceptance rate`, `empirical expected value of target distribution`, `empirical variance of target distribution`, `suggested minimum c`.
#' @export

ar_summary <- function(object) {
  if (!inherits(object, "ar")){
    stop("Input must be an object of class \"ar\"")
  }

  obj_name <- deparse(substitute(object))

  cat("Summary of ", obj_name, ":\n")
  cat("----------------------\n")
  cat("     Acceptance rate: ", round(object$acceptance_rate, 4), "\n")
  cat("     Empirical expected value of target distribution: ", round(object$expected_values_f, 4), "\n")
  cat("     Empirical variance of target distribution: ", round(object$variance_f, 4), "\n")
  cat("     Suggested minimum c: ", ifelse(is.numeric(object$suggested_c), round(object$suggested_c, 4), object$suggested_c), "\n")
  cat("----------------------\n")

  invisible(object)
}
