library(testthat)
test_that(
  "Ensure RevDefine() creates objects as expected",
  {
    RevPath = "D://RevBayes_Win_1.1.0//RevBayes_Win_1.1.0//rb.exe"
    RevR::InitRev()

    expect_message(RevR::RevDefine("myList <- 2+2"), "Numeric object 'myList' has been created in RevEnv!")

    expect_message(RevR::RevDefine("oof <- @"), "  Syntax error while reading character '@' at position 1 in command:   @")
  }
)
