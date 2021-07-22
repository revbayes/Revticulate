library(testthat)
library(Revticulate)

test_that(
  "Testing getRevHistory()",
  {
    initRev()
    clearRev()
    expect_null(getRevHistory())

    times <- as.integer(runif(1)*15)

    for(i in 1:times){
      doRev(i)
    }

    expect_length(getRevHistory(), times)


  }
)
