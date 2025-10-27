#' Plots of `ar` and `ar_conv`.
#'
#' Display an overlay plot of the target density f(x) and proposal density q(x) for an object of class `ar`, or convergence plots for an object of class `ar_conv`.
#'
#' @param model an object of class `ar` or `ar_conv`.
#' @param which index of convergence plot to be shown. Consists of index 1 (acceptance rate), 2 (y values), 3 (mean), and 4 (variance).
#'
#' @return An overlay plot of the target density f(x) and proposal density q(x) for an object of class `ar`, or convergence plots for an object of class `ar_conv`.
#' @export

ar_plot <- function(model, which=c(1,2,3,4)){
  if (!(inherits(model, "ar") || inherits(model, "ar_conv"))){
    stop("Input must be an object of class \"ar\" or \"ar_conv\".")
  }

  # plots for "ar"
  if (inherits(model, "ar")){
    df <- model$data
    params <- model$params

    suppressWarnings({
      # plot f(x) and overlay proposal q(x)
      ggplot2::ggplot(df, ggplot2::aes(x=df$x)) +
        ggplot2::geom_line(ggplot2::aes(y = df$f, color="Target f(x)")) +
        ggplot2::geom_line(ggplot2::aes(y = params$c * df$q, color="Proposal q(x) scaled by c")) +
        ggplot2::scale_color_manual(name=NULL, values=c("Target f(x)" = "blue", "Proposal q(x) scaled by c" = "red")) +
        ggplot2::labs(title="Target vs Proposal PDF", x="x", y="Density") +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position = "bottom")
    })
  }

  # plots for "ar_conv"
  else if (inherits(model, "ar_conv")){
    if (!is.numeric(which)){
      stop ("\"which\" must be numeric indices.")
    }

    df <- model$data

    plot_funs <- list(
      acceptance_rate = function(){
        suppressWarnings({
          ggplot2::ggplot(df, ggplot2::aes(x=df$index, y=df$acceptance_rate_cumulative)) +
            ggplot2::geom_line(color="green") +
            ggplot2::labs(title="Plot of acceptance rate", x="n", y="Acceptance rate") +
            ggplot2::theme_classic()
        })
      },

      y_values = function(){
        suppressWarnings({
          ggplot2::ggplot(df, ggplot2::aes(x=df$index, y=df$y)) +
            ggplot2::geom_point(color="purple", size=1) +
            ggplot2::labs(title="Scatterplot of y", x="n", y="Accepted y values") +
            ggplot2::theme_classic()
        })
      },

      mean = function(){
        suppressWarnings({
          ggplot2::ggplot(df, ggplot2::aes(x=df$index, y=df$mean_cumulative)) +
            ggplot2::geom_line(color="brown") +
            ggplot2::labs(title="Convergence plot of expected values", x="n", y="Expected values of y") +
            ggplot2::theme_classic()
        })
      },

      variance = function(){
        suppressWarnings({
          ggplot2::ggplot(df, ggplot2::aes(x=df$index, y=df$var_cumulative)) +
            ggplot2::geom_line(color="grey") +
            ggplot2::labs(title="Convergence plot of variance", x="n", y="Variance of y") +
            ggplot2::theme_classic()
        })
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
}
