library(Revticulate)
library(comprehenr)

test_that(
  "Testing knitRev()",
  {
    skip_on_cran()

    knitRev()

    expect_true(any(names(knitr::knit_engines$get()) == "rb"))

  }
)
