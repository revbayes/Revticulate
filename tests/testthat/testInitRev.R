library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing initRev()",
  {
    initRev()

    clearRev()


    path <- "D://RevBayes_Win_1.1.0/RevBayes_Win_1.1.0/rb.exe"

    initRev(path)

    expect_equal(revEnv$RevPath, path)

    expect_length(revEnv$allCode, 0)

    expect_true(str_ends(revEnv$revHistory, "Revticulate/Revhistory.Rhistory"))

    doRev('"This is a test string"')

    initRev(useHistory = TRUE)

    expect_equal(getRevHistory(), '\"This is a test string\"')

    clearRev()
  }
)
