library(crosstabr)

test_that("cross_table sets up the class correctly",
          expect_s3_class(cross_table(mtcars, cyl ~ gear), "cross_table"))