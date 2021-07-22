library(Revticulate)
library(comprehenr)

test_that(
  "Testing knitRev()",
  {
    initRev()

    knitRev()

    expect_true(any(names(knitr::knit_engines$get()) == "rb"))

  }
)
