test_that("Test of the input of the function ar_conv().", {

  # expect error
  expect_error(ar_conv("A"), "Input must be an object of class \"ar\".")
  expect_error(ar_conv(1), "Input must be an object of class \"ar\".")
  expect_error(ar_conv(c(1,2,3)), "Input must be an object of class \"ar\".")
})
