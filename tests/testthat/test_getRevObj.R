library(testthat)
library(Revticulate)

test_that(
  "Testing getRevObj()",
  {
    skip_on_os("windows")

    clearRev()

    for(i in 1:10){
      rand <- runif(10)
      num <- sum(rand)
      var <- paste(letters[as.integer((rand*26))], collapse = "")
      doRev(var %+% " <- " %+% num)

      expect_identical(as.character(round(num, 6)), getRevObj(var))
    }


    clearRev()

    for(i in 1:8){
      doRev("testtree <- simTree(" %+% "2 ^ " %+% as.integer(runif(1, 1, 5)) %+% ")")
      expect_s3_class(getRevObj("testtree", coerce = TRUE), "phylo")
      clearRev()
    }

    for(i in 1:8){
      doRev("b ~ dnDirichlet([0, 1, 0, 1])")
      expect_equal(sum(getRevObj("b", coerce = TRUE), na.rm = TRUE), 1)
      clearRev()
    }

    clearRev()

    for(i in 1:10){
      doRev("var1 <- " %+% i %+% "; var2 := var1 * 10")

      expect_equal(getRevObj("var1", coerce = TRUE), i)
      expect_equal(getRevObj("var2", coerce = TRUE), i*10)

      doRev("var1 <- " %+% (i*10))

      expect_equal(getRevObj("var1", coerce = TRUE), i*10)
      expect_equal(getRevObj("var2", coerce = TRUE), i*100)

    }

    clearRev()
  }
)
