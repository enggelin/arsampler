#' ar
#'
#' Multiplies a number by two using a C++ back end.
#'
#' @param x A numeric value.
#' @return The value of `x * 2`.
#' @export

ar <- function(f, q, c=1, n=1000) {
  # generate x from the proposal distribution
  q_expr <- parse(text = q)
  x <- eval(q_expr)

  # parse f(x) (target density) from string to evaluable expression
  f_expr <- parse(text = f)
  f_vals <- eval(f_expr)

  # infer proposal pdf function
  q_name <- gsub("\\(.*", "", q)   # "rnorm"
  d_name <- sub("^r", "d", q_name) # "dnorm"
  if (!exists(d_name, mode = "function")) {
    stop(paste("Cannot find density function for", q_name))
  }

  # extract params inside parentheses
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

  # compute acceptance ratio
  r <- f_vals/(c*q_vals)
  u <- runif(n)

  #accept if u < r
  accept <- (u < r)
  y <- ifelse(accept, x, 0)

  #suggested c
  #ratio <- f_vals / q_vals
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
    expected_values_f = mean(x),
    suggested_c = suggested_c
  )

  class(result) <- "ar_result"
  return (result)
}
