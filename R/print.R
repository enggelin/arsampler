#' Data of an object of class `ar`
#'
#' Show the data for an object of class `ar`.
#'
#' @param model an object of class `ar`.
#' @param ... additional arguments.
#' @return the data of an object of class `ar` with the following components: `x`, `f`, `q`, `y`, `r`, and `u`.
#' @export
#' @method print ar

print.ar <- function(model, ...) {
  if (!inherits(model, "ar")){
    stop("Input must be an object of class \"ar\"")
  }

  obj_name <- deparse(substitute(model))

  cat("Data of ", obj_name, ":\n\n")
  print(unclass(model))

  invisible(model)
}
