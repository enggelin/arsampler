#' Data of an object of class `ar`
#'
#' Show the data for an object of class `ar`.
#'
#' @param x an object of class `ar`.
#' @param ... additional arguments.
#' @return the data of an object of class `ar` with the following components: `x`, `f`, `q`, `y`, `r`, and `u`.
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
