test_that("Test of the input of the function ar_print().", {

  # expect error
  expect_error(ar_print("A"), "Input must be an object of class \"ar\" or \"ar_conv\".")
  expect_error(ar_print(1), "Input must be an object of class \"ar\" or \"ar_conv\".")
  expect_error(ar_print(c(1,2,3)), "Input must be an object of class \"ar\" or \"ar_conv\".")
})
