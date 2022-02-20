library(Revticulate)
library(knitr)
if(Sys.info()["sysname"] != "Windows")
  library(comprehenr)


test_that(
  "Testing knitRev()",
  {
    clearRev()

    knitRev()

    expect_true(any(names(knitr::knit_engines$get()) == "rb"))

    clearRev()
  }
)
