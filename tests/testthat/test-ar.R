test_that("The test of the output for the exact same pair of target and proposal densities.", {

  ## initialise variables for testing
  # pair of standard normal distributions
  f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
  q_norm <- "rnorm(n, 0, 1)"

  # pair of uniform distribution
  f_unif <- "1"
  q_unif <- "runif(n, 0, 1)"

  # pair of gamma distribution, alpha=3, beta=2
  f_gamma <- "((2^3)/(gamma(3)))*x^(3-1)*exp(-2*x)"
  q_gamma <- "rgamma(n, 3, 2)"

  # pair of beta distribution, alpha=3, beta=2
  f_beta <- "((gamma(3+2))/gamma(3)*gamma(2))*x^(3-1)*(1-x)^(2-1)"
  q_beta <- "rbeta(n, 3, 2)"

  # pair of exp distribution, lambda=3
  f_exp <- "3*exp(-3*x)"
  q_exp <- "rexp(n, 3)"

  # expect silent (correct input)
  expect_silent(ar(f_norm, q_norm, c=1, n=1000))
  expect_silent(ar(f_unif, q_unif, c=1, n=1000))
  expect_silent(ar(f_gamma, q_gamma, c=1, n=1000))
  expect_silent(ar(f_beta, q_beta, c=1, n=1000))
  expect_silent(ar(f_exp, q_exp, c=1, n=1000))

  # expect equal: acceptance rate = 1
  expect_equal(ar(f_norm, q_norm, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_unif, q_unif, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_gamma, q_gamma, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_beta, q_beta, c=1, n=1000)$acceptance_rate, 1)
  expect_equal(ar(f_exp, q_exp, c=1, n=1000)$acceptance_rate, 1)

  # expect equal: rounded mean from standard distributions; longer run for convergence
  expect_equal(abs(round(ar(f_norm, q_norm, c=1, n=10000)$expected_values_f, 1)), 0) # need to put abs() on the standard normal distribution because the possible value may range from negative around zero to positive around zero.
  expect_equal(round(ar(f_unif, q_unif, c=1, n=10000)$expected_values_f, 1), 0.5)
  expect_equal(round(ar(f_gamma, q_gamma, c=1, n=10000)$expected_values_f, 1), 1.5)
  expect_equal(round(ar(f_beta, q_beta, c=1, n=10000)$expected_values_f, 1), 0.6)
  expect_equal(round(ar(f_exp, q_exp, c=1, n=10000)$expected_values_f, 1), 0.3)

  # expect match: suggested c = NA
  expect_match(ar(f_norm, q_norm, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_unif, q_unif, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_gamma, q_gamma, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_beta, q_beta, c=1, n=1000)$suggested_c, "NA")
  expect_match(ar(f_exp, q_exp, c=1, n=1000)$suggested_c, "NA")
})
