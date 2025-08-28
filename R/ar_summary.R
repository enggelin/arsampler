#' ar_summary
#'
#' Multiplies a number by two using a C++ back end.
#'
#' @param x A numeric value.
#' @return The value of `x * 2`.
#' @export

ar_summary <- function(model) {
  if (!inherits(model, "ar_result")){
    stop("Input must be an object of class \"ar_result\"")
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
