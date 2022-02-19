library(Revticulate)
library(testthat)

test_that(
  "Testing saveRev()",
  {
    file <- tempfile()

    expect_false(file.exists(file))

    doRev("2")

    saveRev(file)

    expect_true(file.exists(file))

    clearRev()
  }
)
