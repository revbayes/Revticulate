library(Revticulate)
test_that(
  "Testing doRev()",
  {
    skip_on_os("windows")

    clearRev()

    testthat::expect_error(doRev(c("2+2", "3+3")), "Input length must equal one.")

    randomseq <- function() paste(unlist(c(LETTERS, letters))[as.integer(runif(30) * 52)], collapse = "")

    testnon <- randomseq()

    expect_warning(doRev(testnon, coerce = TRUE), "Missing Variable: Variable " %+% testnon %+% " does not exist")

    expect_equal(doRev("simTree(2)", coerce = TRUE), ape::read.tree(text = "   (Taxon_1[&index=2]:1.000000,Taxon_2[&index=1]:1.000000)[&index=3]:0.000000;"))


    for(i in 1:10){
      nonvar <- paste(LETTERS[as.integer(runif(10)*26)], collapse = "")
      expect_warning(doRev(nonvar, coerce = TRUE), "Missing Variable: Variable " %+% nonvar %+% " does not exist")
    }

    clearRev()
  }
)
