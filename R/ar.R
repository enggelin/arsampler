#' Acceptance-rejection Sampler
#'
#' Perform an acceptance-rejection sampling on target density \eqn{f(x)} using proposal density \eqn{q(x)} with optional arguments \code{c} and \code{n}.
#'
#' @param f the target density, \eqn{f(x)}.
#' @param q the proposal density, \eqn{q(x)}.
#' @param c the value of \code{c}, which is usually is the maximum ratio of \eqn{f(x)} and \eqn{q(x)}. Defaulted at 1.
#' @param n the number of iteration. Defaulted at 1000.
#' @return
#' \describe{\code{ar} returns an object of class "ar". An object of class "ar" is a list containing the following components:
#'   \item{params}{A list containing the input arguments \code{f}, \code{q}, \code{c}, and \code{n}.}
#'   \item{data}{A data frame containing the outputs: \code{index}, \code{x}, \code{f}, \code{q}, \code{y}, \code{r}, \code{u}}
#'   \item{acceptance_rate}{Ratio of accepted \code{x}.}
#'   \item{expected_values_f}{Sample mean of accepted values.}
#'   \item{variance_f}{Sample variance of accepted values.}
#'   \item{suggested_c}{Suggested \code{c} estimated from \eqn{\max f(x)/q(x)}.}
#' }
#'
#' @examples
#' # pair of standard normal distributions
#' f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
#' q_norm <- "rnorm(n, 0, 1)"
#' example_norm <- ar(f_norm, q_norm, c=1, n=1000)
#' example_norm$data
#'
#' @seealso [stats::rnorm()]
#' @export

ar <- function(f, q, c=1, n=1000) {

  # sample x from the proposal distribution
  q_expr <- parse(text = q)
  x <- eval(q_expr)

  # evaluate target density f(x) from string to valid expression using Rcpp
  f_vals <- tsamp(f, x, env = parent.frame())

  # return an error if the target density produces infinite or negative values
  if (any(!is.finite(f_vals)) || any(f_vals < 0)){
    stop("Target density generated infinite or negative values. Check your expression and parameters.")
  }

  # infer proposal pdf function
  q_name <- gsub("\\(.*", "", q)   # "rnorm"
  d_name <- sub("^r", "d", q_name) # "dnorm"
  if (!exists(d_name, mode = "function")) {
    stop(paste("Cannot find density function for", q_name))
  }

  # extract parameters inside parentheses
  param_text <- gsub("^[^(]*\\(|\\)$", "", q)  # "n,0,1"
  param_list <- strsplit(param_text, ",")[[1]]
  param_list <- trimws(param_list)
  # remove the first argument (the "n")
  if (length(param_list) > 0) {
    param_list <- param_list[-1]
  }

  # build density call
  if (length(param_list) > 0) {
    q_call <- paste0(d_name, "(x, ", paste(param_list, collapse=","), ")")
  } else {
    q_call <- paste0(d_name, "(x)")
  }
  q_vals <- eval(parse(text = q_call))

  # return an error if proposal density produces infinite or negative values
  if (any(!is.finite(q_vals)) || any(q_vals < 0)){
    stop("Proposal density generated infinite or negative values. Check your expression and parameters.")
  }

  # compute acceptance ratio
  r <- f_vals/(c*q_vals)
  u <- stats::runif(n)

  #accept if u < r
  accept <- (u < r)
  y <- ifelse(accept, x, 0)

  #suggested c
  suggested_c <- ifelse(max(r[is.finite(r)])> (1+1e-3), max(r[is.finite(r)]), "NA") # tolerance epsilon 1e-3 = 0.001

  # create data frame
  data <- data.frame(
    index = seq_len(n),
    x = x,
    f = f_vals,
    q = q_vals,
    y = y,
    r = r,
    u = u
  )

  # create object result
  result <- list(
    params = list(f = f, q = q, c = c, n = n),
    data = data,
    acceptance_rate = mean(y != 0),
    expected_values_f = mean(y[y != 0]),
    variance_f = stats::var(y[y != 0]),
    suggested_c = suggested_c
  )

  class(result) <- "ar"
  return (result)
}
