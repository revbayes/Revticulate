library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing ClearRev()",
  {
    Revticulate::initRev()

    for(i in 1:10){
      pow <- 2**as.integer(runif(1, 1, 10))
      doRev("simTree(" %+% pow %+% ")")
    }

    clearRev(1)



  }
)
