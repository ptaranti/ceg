

context("dataFrameChecker")

### test sample with NA
df1 <- read.csv("./CHDS.test1.csv")
df2 <- read.csv("./CHDS.test2.csv")
df3 <- read.csv("./CHDS.test3.csv")
df4 <- 3
df5 <- read.csv("./CHDS.test4.csv")

test_that("correct data frame", {
  expect_that(df1, is_a("data.frame"))
  expect_that(df2, is_a("data.frame"))
  expect_that(df3, is_a("data.frame"))
  expect_equal(DataFrameChecker(df4), NULL)
  expect_equal(DataFrameChecker(df1), df1)
  expect_true(!anyNA(DataFrameChecker(df2)))
  expect_true(!anyNA(DataFrameChecker(df3)))
})
