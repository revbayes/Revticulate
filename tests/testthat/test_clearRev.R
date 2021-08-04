library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing ClearRev()",
  {

    for(i in 1:30){
      doRev(i)
    }

    for(i in 1:3){
      slice <- as.integer(runif(1, 1, 10))
      oldlength <- length(getRevHistory())
      clearRev(slice)

      expect_equal(as.integer(length(getRevHistory())), as.integer((oldlength - slice)))
    }

    clearRev()

    doRev('"random input"')

    expect_message(clearRev(), "Successfully reset Rev History!")


  }
)
