context("build_tab")

test_that("layout param is handled correctly", {
  tab <- crosstab(titanic, Survived ~ Sex)

  expect_message(build_tab(tab), "No layout")
  expect_message({tab %>% layout_column() %>% build_tab()}, regexp = NA)
  expect_error({tab$layout <- "row"; build_tab(tab)}, "just column layout")
})

test_that("result is computed correctly", {
  tab <- crosstab(titanic, Survived ~ Sex)
  correct_result <- structure(c("1364<br>78.8%", "367<br>21.2%", "1731<br>100%",
                                "126<br>26.8%", "344<br>73.2%", "470<br>100%", "1490<br>67.7%",
                                "711<br>32.3%", "2201<br>100%"),
                              .Dim = c(3L, 3L),
                              .Dimnames = list(
                                  c("No", "Yes", "Total"), c("Male", "Female", "Total")
                                  ))

  expect_equal(build_tab(tab), correct_result)

})
