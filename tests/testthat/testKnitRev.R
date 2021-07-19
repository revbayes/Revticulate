library(Revticulate)
library(comprehenr)

test_that(
  "Testing knitRev()",
  {

    knitRev()

    expect_true(any(names(knitr::knit_engines$get()) == "rb"))

  }
)
