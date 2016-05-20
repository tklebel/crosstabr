context("layout")

## TODO: Add more tests

test_that("layout_column adds layout property", {
  tab <- titanic %>%
    crosstab(Survived ~ Sex) %>%
    layout_column()

  expect_equal(tab$layout, "column")
})
