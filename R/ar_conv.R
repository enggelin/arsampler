#' Convergence diagnostics of an object of class `ar`.
#'
#' Provide a convergence diagnostic for an object of class `ar`.
#'
#' @param object an object of class `ar`.
#' @return The convergence diagnostic of an object of class `ar` with the following components: `acceptance rate`, `empirical expected value of target distribution`, `empirical variance of target distribution`, `suggested minimum c`.
#' @export

ar_conv <- function(object) {
  if (!inherits(object, "ar")){
    stop("Input must be an object of class \"ar\"")
  }

  obj_name <- deparse(substitute(object))

  y <- object$data$y
  idx <- length(y)

  # initialise the array for acceptance rate, value of y, mean, and variance
  acceptance_rate_cumulative <- rep(NA, idx)
  mean_cumulative <- rep(NA, idx)
  var_cumulative <- rep(NA, idx)

  # initialise the array for acceptance rate, mean, and variance
  mean_cumulative[1] <- ifelse(!is.na(y[1]), y[1], 0)
  var_cumulative[1] <- ifelse(!is.na(y[1]), y[1], 0)

  # convergence for acceptance rate
  for (i in 1:idx){
    acceptance_rate_cumulative[i] <- mean(!is.na(y[1:i]))
  }

  # convergence for mean
  if (idx > 1){
    for (i in 2:idx){
      if (is.na(y[i])){
        mean_cumulative[i] <- mean_cumulative[i-1]
      }
      else {
        mean_cumulative[i] <- mean(y[1:i], na.rm=TRUE)
      }
    }
  }

  # convergence for variance
  if (idx > 1){
    for (i in 2:idx){
      if (is.na(y[i])){
        var_cumulative[i] <- var_cumulative[i-1]
      }
      else {
        var_cumulative[i] <- stats::var(y[1:i], na.rm=TRUE)
        if (is.na(var_cumulative[i])) {
          var_cumulative[i] <- var_cumulative[i-1]
        }
      }
    }
  }

  # create object result
  data <- data.frame(
    index = seq_len(idx),
    acceptance_rate_cumulative = acceptance_rate_cumulative,
    y = y,
    mean_cumulative = mean_cumulative,
    var_cumulative = var_cumulative
  )

  result <- list(
    data = data
  )

  class(result) <- "ar_conv"
  return (result)
}
