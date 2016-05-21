context("crosstab")

test_that("crosstab sets up the class correctly", {
  expect_s3_class(crosstab(titanic, Survived ~ Sex), "crosstab")
})

test_that("input values are checked",{
  expect_error(crosstab(list()), "Data must be a 'data.frame'.")
  expect_error(crosstab(data.frame(), "var"), "two-sided formula")
  expect_error(crosstab(data.frame(), ~ var), "two-sided formula")
  expect_error(crosstab(data.frame(), ~ var + var), "two-sided formula")
})
