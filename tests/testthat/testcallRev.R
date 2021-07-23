library(testthat)
library(Revticulate)
library(comprehenr)

test_that(
  "Ensure CallRev() coerces as expected",
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


    expect_equal(to_vec(for(i in 1:10) as.character(i**2)) , to_vec(for(i in 1:10) stringr::str_squish(callRev(i %+% "^ 2"))))


    randomseq <- function() paste(unlist(c(LETTERS, letters))[as.integer(runif(30) * 52)], collapse = "")

        testnons <- to_vec(for(i in 1:10) randomseq())

        expect_equal(unlist(lapply(testnons, FUN = doRev)), to_vec(for(i in testnons) "Missing Variable: Variable " %+% i %+% " does not exist"))



    testthat::expect_equal(Revticulate::callRev("simTree(2)"), "   (Taxon_1[&index=2]:1.000000,Taxon_2[&index=1]:1.000000)[&index=3]:0.000000;")


    }
)









