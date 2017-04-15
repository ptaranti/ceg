

context("event.tree")

### test if the correct event tree is created from data CHDS.test1.csv
df1 <- read.csv("./CHDS.test1.csv")
## load event_tree - testing data
load("./event_tree_for_test1.Rdata")

test_that("correct data frame", {
  et <- event.tree(df1)
  expect_equal(et, event_tree)
})
