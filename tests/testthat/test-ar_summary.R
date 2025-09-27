test_that("Test of the input of the function ar_summary().", {

  # expect error
  expect_error(ar_summary("A"), "Input must be an object of class \"ar\"")
  expect_error(ar_summary(1), "Input must be an object of class \"ar\"")
  expect_error(ar_summary(c(1,2,3)), "Input must be an object of class \"ar\"")
})
