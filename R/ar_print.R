#' Data of an object of class `ar` or `ar_conv`.
#'
#' Show the data for an object of class `ar` or `ar_conv`.
#'
#' @param object an object of class `ar` or `ar_conv`.
#' @return \code{ar_print} returns different results depending on the class of the input object. If the input object is `ar`, this function will return a list with the following components: `x`, `f`, `q`, `y`, `r`, and `u`, whereas if the input object is of class `ar_conv`, this function will return a list with the following components: `acceptance_rate_cumulative`, `y`, `mean_cumulative`, and `var_cumulative`.
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
