context("crosstab")

library(crosstabr)

test_that("crosstab sets up the class correctly",
          expect_s3_class(crosstab(titanic, Survived ~ Sex), "crosstab"))
