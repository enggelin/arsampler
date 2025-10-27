test_that("Test of the input of the function ar_print().", {

  # expect error
  expect_error(ar_print("A"), "Input must be an object of class \"ar\" or \"ar_conv\".")
  expect_error(ar_print(1), "Input must be an object of class \"ar\" or \"ar_conv\".")
  expect_error(ar_print(c(1,2,3)), "Input must be an object of class \"ar\" or \"ar_conv\".")
})


test_that("Test of valid outputs of the function ar_print().", {
  # pair of standard normal distribution and uniform distribution
  f_norm <- "(1/sqrt(2*pi*1^2))*exp((-(x-0)^2)/(2*1^2))"
  q_unif <- "runif(n, -4, 4)"
  set.seed(0)
  example_norm_unif <- ar(f_norm, q_unif, c=3.2, n=1000)
  example_norm_unif_conv <- ar_conv(example_norm_unif)

  # expect no errors/fails/messages/warning of the output of input "ar".
  expect_no_condition(ar_print(example_norm_unif))
  expect_no_condition(print(example_norm_unif))

  # expect no errors/fails/messages/warning of the output of input "ar_conv".
  expect_no_condition(print(example_norm_unif_conv))
  expect_no_condition(ar_print(example_norm_unif_conv))
})
