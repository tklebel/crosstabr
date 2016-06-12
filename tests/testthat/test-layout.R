context("layout")

## TODO: Add more tests

test_that("layout_column adds layout property", {
  tab <- titanic %>%
    crosstab(Survived ~ Sex) %>%
    layout_column()

  expect_equal(tab$layout, "column")
})

test_that("droplevels sets correct param", {
  tab <- titanic %>%
    crosstab(Survived ~ Sex) %>%
    layout_column()

  expect_equal(tab$droplevels, FALSE)

  tab <- tab %>% layout_column(droplevels = T)
  expect_equal(tab$droplevels, TRUE)
})


test_that("layout_column preserves unused factor levels by default", {
  test_dat <- titanic
  index <- test_dat$Class == "2nd"
  test_dat[index, ] <- NA

  tab <- test_dat %>%
    crosstab(Survived ~ Class)

  freqs <- compute_frequencies(tab) %>%
    dplyr::filter(Class == "2nd")

  # keeps unused levels
  expect_identical(nrow(freqs), 2L)
  expect_identical(sum(freqs[["Freq"]]), 0L)
})

test_that("droplevels drops unused factors", {
  test_dat <- titanic
  index <- test_dat$Class == "2nd"
  test_dat[index, ] <- NA

  tab <- test_dat %>%
    crosstab(Survived ~ Class) %>%
    layout_column(droplevels = T)

  freqs <- compute_frequencies(tab) %>%
    dplyr::filter(Class == "2nd")

  expect_identical(nrow(freqs), 0L)
})
