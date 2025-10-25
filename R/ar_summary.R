#' Summaries of an object of class `ar`
#'
#' Provide a summary for an object of class `ar`.
#'
#' @param object an object of class `ar`.
#' @return The summary of an object of class `ar` with the following components: `number of iterations`, `acceptance rate`, `empirical expected value of target distribution`, `empirical variance of target distribution`, `suggested minimum c`.
#' @export

ar_summary <- function(object) {
  if (!(inherits(object, "ar") || inherits(object, "ar_conv"))){
    stop("Input must be an object of class \"ar\" or \"ar_conv\".")
  }

  obj_name <- deparse(substitute(object))

  if (inherits(object, "ar")){
    cat("Summary of ", obj_name, ":\n")
    cat("----------------------\n")
    cat("     Number of iterations: ", object$params$n, "\n")
    cat("     Acceptance rate: ", round(object$acceptance_rate, 4), "\n")
    cat("     Empirical expected value of target distribution: ", round(object$expected_values_f, 4), "\n")
    cat("     Empirical variance of target distribution: ", round(object$variance_f, 4), "\n")
    cat("     Suggested minimum c: ", ifelse(is.numeric(object$suggested_c), round(object$suggested_c, 4), object$suggested_c), "\n")
    cat("----------------------\n")

    invisible(object)
  }

  else if (inherits(object, "ar_conv")){
    cat("Convergence summary of ", obj_name, ":\n")
    cat("----------------------\n")
    cat("     Acceptance rate: ", round(object$data$acceptance_rate_cumulative[length(object$data$acceptance_rate_cumulative)], 4), "\n")
    cat("     Empirical expected value of target distribution: ", round(object$data$mean_cumulative[length(object$data$mean_cumulative)], 4), "\n")
    cat("     Empirical variance of target distribution: ", round(object$data$var_cumulative[length(object$data$var_cumulative)], 4), "\n")
    cat("----------------------\n")

    invisible(object)
  }
}
