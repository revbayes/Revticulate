library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing ClearRev()",
  {
    initRev()


    for(i in 1:15){
      doRev(i)
    }

    for(i in 1:3){
      slice <- as.integer(runif(1, 0, 5))
      oldlength <- length(getRevHistory())
      clearRev(slice)

      expect_equal(as.integer(length(getRevHistory())), as.integer((oldlength - slice)))
    }

    clearRev()

    doRev('"random input"')

    expect_message(clearRev(), "Successfully reset revEnv!")


  }
)
