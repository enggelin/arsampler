#' Data of an object of class `ar` or `ar_conv`.
#'
#' Show the data for an object of class `ar` or `ar_conv`.
#'
#' @param object an object of class `ar` or `ar_conv`.
#' @return \code{ar_print} returns different results depending on the class of the input object. If the input object is `ar`, this function will return a list with the following components: `x`, `f`, `q`, `y`, `r`, and `u`, whereas if the input object is of class `ar_conv`, this function will return a list with the following components: `acceptance_rate_cumulative`, `y`, `mean_cumulative`, and `var_cumulative`.
#'
#' @examples
#' # pair of standard normal distribution (target) and uniform distribution (proposal)
#' f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
#' q_unif <- "runif(n, -4, 4)"
#' example_norm_unif <- ar(f_norm, q_unif, c=3.2, n=1000)
#' ar_print(example_norm_unif)
#'
#' @export

ar_print <- function(object) {
  if (!(inherits(object, "ar") || inherits(object, "ar_conv"))){
    stop("Input must be an object of class \"ar\" or \"ar_conv\".")
  }

  obj_name <- deparse(substitute(object))

  if (inherits(object, "ar")){
    cat("Data of ", obj_name, ":\n\n")
    print(unclass(object))
  } else if (inherits(object, "ar_conv")){
    cat("Convergence data of ", obj_name, ":\n\n")
    print(unclass(object))
  }

  invisible(object)
}
