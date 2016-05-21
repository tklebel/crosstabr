context("print")

test_that("html-page is set up correctly", {
  page <- create_page("")

  expect_equal(attr(page, "html_dependencies")[[1]]$name, "crosstabr")
  expect_equal(attr(page, "html_dependencies")[[1]]$stylesheet,
               "css/crosstabr.css")
  })

test_that("table is constructed as html-table", {
  tab <- build_tab(crosstab(titanic, Survived ~ Sex))
  html_table <- prepare_table(tab)

  expect_equal(stringr::str_detect(html_table, "class='gmisc_table'"), TRUE)
  expect_equal(stringr::str_detect(html_table, "<thead>"), TRUE)
  expect_equal(stringr::str_detect(html_table, "</thead>"), TRUE)
  expect_equal(stringr::str_detect(html_table, "<tbody>"), TRUE)
  expect_equal(stringr::str_detect(html_table, "</tbody>"), TRUE)
  expect_equal(stringr::str_detect(html_table, "style"), FALSE)
})
