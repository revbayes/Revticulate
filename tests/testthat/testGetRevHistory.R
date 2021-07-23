library(testthat)
library(Revticulate)

test_that(
  "Testing getRevHistory()",
  {
    initRev()

    skip_if_not_init <- function(){
      if(exists("revEnv")){
        if(exists("RevPath", envir = revEnv)){
          if(file.exists(revEnv$RevPath)){
            return(invisible(TRUE))
          }
          else{
            skip("RevPath is not an existing file!")
          }
        }
        else{
          skip("revEnv not initiated!")
        }
      }
      else{
        skip("RevPath not initiated!")
      }
    }


    skip_if_not_init()

    clearRev()
    expect_null(getRevHistory())

    times <- as.integer(runif(1)*15)

    for(i in 1:times){
      doRev(i)
    }

    expect_length(getRevHistory(), times)


  }
)
