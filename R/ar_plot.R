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
  plot(df$x, df$f, pch=19, cex=0.5, col="blue", main="Target vs Proposal PDF", xlab="x", ylab="Density")

  # overlay proposal q(x)
  points(df$x, params$c*df$q, pch=19, cex=0.5, col="red")

  # overlay accepted y as green points
  points(df$y[df$y != 0], df$f[df$y != 0]/params$c*df$q[df$y != 0], pch=19, cex=0.5, col="green")

  legend("topright", legend=c("Target f(x)", "Proposal q(x)", "Accepted y"), col=c("blue", "red", "green"), pch=c(19,19,19), bty="n")
}
