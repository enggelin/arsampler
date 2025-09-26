#' ar_plot
#'
#' Display an overlay plot of the target density f(x) and proposal density q(x).
#'
#' @param model an object of class `ar`.
#' @return An overlay plot of the target density f(x) and proposal density q(x).
#' @export

ar_plot <- function(model){
  if (!inherits(model, "ar")){
    stop("Input must be an object of class \"ar\"")
  }

  df <- model$data
  params <- model$params

  # plot f(x)
  graphics::plot(df$x, df$f, pch=19, cex=0.6, col="blue", main="Target vs Proposal PDF", xlab="x", ylab="Density")

  # overlay proposal q(x)
  graphics::points(df$x, params$c*df$q, pch=19, cex=0.4, col="red")

  graphics::legend("topright", legend=c("Target f(x)", "Proposal q(x)"), col=c("blue", "red"), pch=c(19,19), bty="n")
}
