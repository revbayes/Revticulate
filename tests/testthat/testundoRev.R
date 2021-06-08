library(testthat)
test_that(
  "Testing undoRev",
  {
    Revticulate::InitRev()

    expect_false(all(c("Deterministic", "RevPath", "temps", "Vars") == c(ls(RevEnv))))

    expect_message(ClearRev(), "Successfully removed 2 objects from RevEnv!")

  }
)
