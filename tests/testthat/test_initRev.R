library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing initRev()",
  {
    skip_on_cran()

    clearRev()

    initRev()

    expect_length(revEnv$allCode, 0)

    expect_true(str_ends(revEnv$revHistory, "Revticulate/Revhistory.Rhistory"))

    doRev('"This is a test string"')

    initRev(useHistory = TRUE)

    expect_equal(getRevHistory()[1], '\"This is a test string\"')

    clearRev()
  }
)
