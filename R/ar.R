#' Acceptance-rejection Sampler
#'
#' Perform a acceptance-rejection sampling on target density f(x) using proposal density q(x) with optional c and n.
#'
#' @param f the target density, f(x).
#' @param q the proposal density, q(x).
#' @param c the value of c, which is usually is the maximum ratio of f(x) and q(x). Defaulted at 1.
#' @param n the number of iteration. Defaulted at 1000.
#' @return `ar` returns an object of class "ar". An object of class "ar" is a list containing the following components: `params`, `data`, `acceptance_rate`, `expected_values_f`, `variance_f`, `suggested_c`.
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
  suggested_c <- ifelse(max(r[is.finite(r)])>1, max(r[is.finite(r)]), "NA")

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
