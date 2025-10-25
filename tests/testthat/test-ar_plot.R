test_that("Test of the input of the function ar_plot().", {

  # expect error
  expect_error(ar_plot("A"), "Input must be an object of class \"ar\" or \"ar_conv\".")
  expect_error(ar_plot(1), "Input must be an object of class \"ar\" or \"ar_conv\".")
  expect_error(ar_plot(c(1,2,3)), "Input must be an object of class \"ar\" or \"ar_conv\".")
})
