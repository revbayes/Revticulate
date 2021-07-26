library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Testing initRev()",
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

    clearRev()

    initRev(revEnv$RevPath)

    expect_length(revEnv$allCode, 0)

    expect_true(str_ends(revEnv$revHistory, "Revticulate/Revhistory.Rhistory"))

    doRev('"This is a test string"')

    initRev(useHistory = TRUE)

    expect_equal(getRevHistory()[1], '\"This is a test string\"')

    clearRev()
  }
)
