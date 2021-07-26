library(Revticulate)
library(comprehenr)

test_that(
  "Testing knitRev()",
  {
    skip_on_cran()

    initRev(findRev())

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

    knitRev()

    expect_true(any(names(knitr::knit_engines$get()) == "rb"))

  }
)
