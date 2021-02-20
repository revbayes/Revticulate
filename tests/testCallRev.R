test_that(
  "Ensuring common CallRev() objects coerce correctly",
  {
    RevPath = "D://RevBayes_Win_1.1.0//RevBayes_Win_1.1.0//rb.exe"

    warnOut <- (testthat::capture_warnings(RevR::CallRev(path = RevPath , "[1, 2, 3]")))
        testthat::expect_equal(warnOut, "coercing argument of type 'double' to logical")

    expect_message(RevR::CallRev(path = RevPath , "c(1, 2, 3)"), "Error:	No function named 'c'")



    }
)
