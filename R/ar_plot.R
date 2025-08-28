#' ar_plot
#'
#' Multiplies a number by two using a C++ back end.
#'
#' @param x A numeric value.
#' @return The value of `x * 2`.
#' @export

ar_plot <- function(model){
  if (!inherits(model, "ar_result")){
    stop("Input must be an object of class \"ar_result\"")
  }

  df <- model$data
  params <- model$params

  # plot f(x)
  plot(df$x, df$f, pch=19, cex=0.6, col="blue", main="Target vs Proposal PDF", xlab="x", ylab="Density")

  # overlay proposal q(x)
  points(df$x, params$c*df$q, pch=19, cex=0.4, col="red")

  legend("topright", legend=c("Target f(x)", "Proposal q(x)"), col=c("blue", "red"), pch=c(19,19), bty="n")
}
