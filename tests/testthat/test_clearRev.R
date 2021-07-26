library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing ClearRev()",
  {
    skip_on_cran()



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
