library(testthat)
library(Revticulate)

test_that(
  "Testing getRevHistory()",
  {
    clearRev()

     expect_equal(getRevHistory(), character(0))

    times <- as.integer(runif(1)*15)

    for(i in 1:times){
      doRev(i)
    }

    expect_length(getRevHistory(), times)

    clearRev()
  }
)
