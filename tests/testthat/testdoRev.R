library(Revticulate)
test_that(
  "Testing doRev()",
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

    testthat::expect_equal(doRev("2+2", "3+3"), "4")

    randomseq <- function() paste(unlist(c(LETTERS, letters))[as.integer(runif(30) * 52)], collapse = "")

    testnon <- randomseq()

    expect_message(doRev(testnon, coerce = T), "Missing Variable: Variable " %+% testnon %+% " does not exist")

    expect_equal(Revticulate::doRev("simTree(2)", coerce = T), ape::read.tree(text = "   (Taxon_1[&index=2]:1.000000,Taxon_2[&index=1]:1.000000)[&index=3]:0.000000;"))


    for(i in 1:10){
      nonvar <- paste(LETTERS[as.integer(runif(10)*26)], collapse = "")
      expect_message(doRev(nonvar, coerce = T), "Missing Variable: Variable " %+% nonvar %+% " does not exist")
    }


  }
)
