test_that(
  "Testing doRev()",
  {
    testthat::expect_equal(doRev("2+2", "3+3"), list(4, 6))

    expect_message(RevR::RevDefine("myList <- 2+2"), "Numeric object 'myList' has been created in RevEnv!")

    testthat::expect_equal(RevR::doRev("simTree(2)"), ape::read.tree(text = "   (Taxon_1[&index=2]:1.000000,Taxon_2[&index=1]:1.000000)[&index=3]:0.000000;"))

  }
)
