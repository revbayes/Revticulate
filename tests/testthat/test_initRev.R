library(testthat)
library(Revticulate)
library(stringr)

test_that(
  "Testing initRev()",
  {
    skip_on_os("windows")

    library(comprehenr)

    clearRev()

    initRev()

    expect_length(getRevHistory(), 0)

    expect_true(str_ends(Sys.getenv("RevHistory"), "/.Revhistory"))

    doRev('"This is a test string"')

    initRev()

    expect_equal(getRevHistory()[1], '\"This is a test string\"')

    clearRev()
  }
)
