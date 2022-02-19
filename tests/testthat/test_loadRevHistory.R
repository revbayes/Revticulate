library(Revticulate)
library(testthat)

test_that(
  "Testing loadRevHistory()",
  {
    clearRev()

    file <- tempfile()

    cat(c(1:10), sep = "\n", file = file)

    loadRevHistory(file)

    expect_true(all(getRevHistory() == c("#START", 1:10)))


    clearRev()
  }
)
