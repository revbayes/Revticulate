library(testthat)

test_that(
  "Testing getRevVars()",
  {
    skip_on_os("windows")

    library(Revticulate)
    library(comprehenr)

    clearRev()

    nums <- as.integer(runif(10, 1, 100))

    randVar <- function() paste(letters[as.integer(runif(10, 1, 26))], collapse = "")

    vars <- to_vec(for(i in 1:10) randVar())

    assignments <- to_vec(for(i in 1:10) vars[i] %+% " <- " %+% nums[i])

    lapply(assignments, doRev)


    expect_length(getRevVars(), 10)

    for(i in 1:10)
      expect_equal(vars[i] %+% " <- " %+% nums[i], getRevVars()[i])

    clearRev()
  }
)
