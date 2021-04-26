library(testthat)
test_that(
  "Ensure CallRev() coerces as expected",
  {
    RevPath = "D://RevBayes_Win_1.1.0//RevBayes_Win_1.1.0//rb.exe"
    Revticulate::InitRev()

    testthat::expect_warning(Revticulate::CallRev(path = RevPath , "[1, 2, 3]"), "coercing argument of type 'double' to logical")

    expect_message(Revticulate::CallRev(path = RevPath , "c(1, 2, 3)"), "Error:	No function named 'c'")


    testthat::expect_equal(Revticulate::doRev("simTree(2)"), ape::read.tree(text = "   (Taxon_1[&index=2]:1.000000,Taxon_2[&index=1]:1.000000)[&index=3]:0.000000;"))
    }
)
