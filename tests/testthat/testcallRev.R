library(testthat)
test_that(
  "Ensure CallRev() coerces as expected",
  {
    Revticulate::initRev()

    testthat::expect_warning(Revticulate::callRev("[1, 2, 3]"), "coercing argument of type 'double' to logical")

    expect_message(Revticulate::callRev("c(1, 2, 3)"), "Error:	No function named 'c'")


    testthat::expect_equal(Revticulate::doRev("simTree(2)"), ape::read.tree(text = "   (Taxon_1[&index=2]:1.000000,Taxon_2[&index=1]:1.000000)[&index=3]:0.000000;"))
    }
)
