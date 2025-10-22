#' Plots of an object of class `ar`.
#'
#' Display an overlay plot of the target density f(x) and proposal density q(x).
#'
#' @param model an object of class `ar`.
#' @param ... additional arguments
#' @return An overlay plot of the target density f(x) and proposal density q(x).
#' @export
#' @method plot ar

plot.ar <- function(model, main="Target vs Proposal PDF", xlab="x", ylab="Density", pos="bottom"){
  if (!inherits(model, "ar")){
    stop("Input must be an object of class \"ar\".")
  }

  df <- model$data
  params <- model$params

  # plot f(x) and overlay proposal q(x)
  ggplot2::ggplot(df, ggplot2::aes(x=x)) +
    ggplot2::geom_line(ggplot2::aes(y = f, color="Target f(x)")) +
    ggplot2::geom_line(ggplot2::aes(y = params$c * q, color="Proposal q(x) scaled by c")) +
    ggplot2::scale_color_manual(name=NULL, values=c("Target f(x)" = "blue", "Proposal q(x) scaled by c" = "red")) +
    ggplot2::labs(title=main, x=xlab, y=ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = pos)
}


#' Plots of an object of class `ar_conv`.
#'
#' Display an overlay plot of the target density f(x) and proposal density q(x).
#'
#' @param model an object of class `ar_conv`.
#' @param ... additional arguments
#' @return Convergence plots of the sampler.
#' @export
#' @method plot ar_conv

plot.ar_conv <- function(model, which=c(1,2,3,4)){
  if (!inherits(model, "ar_conv")){
    stop("Input must be an object of class \"ar_conv\".")
  }

  if (!is.numeric(which)){
    stop ("\"which\" must be numeric indices.")
  }

  df <- model$data

  plot_funs <- list(
    acceptance_rate = function(){
      ggplot2::ggplot(df, ggplot2::aes(x=index, y=acceptance_rate_cumulative)) +
        ggplot2::geom_line(color="green") +
        ggplot2::labs(title="Plot of acceptance rate", x="n", y="Acceptance rate") +
        ggplot2::theme_classic()
    },

    y_values = function(){
      ggplot2::ggplot(df, ggplot2::aes(x=index, y=y)) +
        ggplot2::geom_line(color="purple") +
        ggplot2::labs(title="Trace plot of y", x="n", y="Accepted y values") +
        ggplot2::theme_classic()
    },

    mean = function(){
      ggplot2::ggplot(df, ggplot2::aes(x=index, y=mean_cumulative)) +
        ggplot2::geom_line(color="brown") +
        ggplot2::labs(title="Convergence plot of expected values", x="n", y="Expected values of y") +
        ggplot2::theme_classic()
    },

    variance = function(){
      ggplot2::ggplot(df, ggplot2::aes(x=index, y=var_cumulative)) +
        ggplot2::geom_line(color="grey") +
        ggplot2::labs(title="Convergence plot of variance", x="n", y="Variance of y") +
        ggplot2::theme_classic()
    }
  )

  if(any(which < 1 | which > length(plot_funs))){
    stop("\"which\" indices must be in 1:", length(plot_funs))
  }

  idx <- as.integer(which)

  # draw only requested plot
  plots <- lapply(idx, function(i) plot_funs[[i]]())
  names(plots) <- names(plot_funs)[idx]
  for (p in plots) print(p)
  invisible(plots)
}
