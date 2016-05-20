context("print")

test_that("html-page is set up correctly", {
  page <- create_page("")

  expect_equal(attr(page, "html_dependencies")[[1]]$name, "crosstabr")
  expect_equal(attr(page, "html_dependencies")[[1]]$stylesheet,
               "css/crosstabr.css")
  })
