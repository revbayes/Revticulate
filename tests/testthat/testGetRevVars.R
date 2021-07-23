library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing getRevVars()",
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

    nums <- as.integer(runif(10, 1, 100))

    randVar <- function() paste(letters[as.integer(runif(10, 1, 26))], collapse = "")

    vars <- to_vec(for(i in 1:10) randVar())

    assignments <- to_vec(for(i in 1:10) vars[i] %+% " <- " %+% nums[i])

    lapply(assignments, doRev)


    expect_length(capture.output(getRevVars()), 10)

    for(i in 1:10)
      expect_equal(vars[i] %+% " <- " %+% nums[i], capture.output(getRevVars())[i])

    clearRev()
  }
)
