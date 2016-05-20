context("build_tab")

test_that("layout param is handled correctly", {
  tab <- crosstab(titanic, Survived ~ Sex)

  expect_message(build_tab(tab), "No layout")
  expect_message({tab %>% layout_column() %>% build_tab()}, regexp = NA)
  expect_error({tab$layout <- "row"; build_tab(tab)}, "just column layout")
})

test_that("result is computed correctly", {
  tab <- crosstab(titanic, Survived ~ Sex)
  correct_result <- structure(c("1364<br>78.8%", "367<br>21.2%", "126<br>26.8%",
                        "344<br>73.2%"),
                      .Dim = c(2L, 2L),
                      .Dimnames = list(c("No", "Yes"), c("Male", "Female")))

  expect_equal(build_tab(tab), correct_result)

})
