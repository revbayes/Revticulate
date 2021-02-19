test_that(
  "Ensuring common CallRev() objects coerce correctly",
  {
    RevPath = "D://RevBayes_Win_1.1.0/RevBayes_Win_1.1.0/rb.exe"
    expect_equivalent(RevR::doRev(path = RevPath , "2+2"), 4)
    expect_equal(RevR::doRev(path = RevPath , "[1, 2, 3]"), c(1, 2, 3))
    expect_message(RevR::doRev(path = RevPath , "c(1, 2, 3)"), "Error:	No function named 'c'")
  }
)
