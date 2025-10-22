#' Data of an object of class `ar`
#'
#' Show the data for an object of class `ar`.
#'
#' @param x an object of class `ar`.
#' @param ... additional arguments.
#' @return the data of an object of class `ar` with the following components: `x`, `f`, `q`, `y`, `r`, and `u`.
#' @export
#' @method print ar

print.ar <- function(x, ...) {
  if (!inherits(x, "ar")){
    stop("Input must be an object of class \"ar\"")
  }

  obj_name <- deparse(substitute(x))

  cat("Data of ", obj_name, ":\n\n")
  print(unclass(x))

  invisible(x)
}


#' Convergence data of an object of class `ar_conv`
#'
#' Show the data for an object of class `ar_conv`.
#'
#' @param x an object of class `ar_conv`.
#' @param ... additional arguments.
#' @return the data of an object of class `ar_conv` with the following components: `acceptance_rate_cumulative`, `y`, `mean_cumulative`, and `var_cumulative`.
#' @export
#' @method print ar_conv

print.ar_conv <- function(x, ...) {
  if (!inherits(x, "ar_conv")){
    stop("Input must be an object of class \"ar_conv\"")
  }

  obj_name <- deparse(substitute(x))

  cat("Convergence data of ", obj_name, ":\n\n")
  print(unclass(x))

  invisible(x)
}
