#'Continuous interactive session with rb.exe
#'
#'Allows user to continuously call rb.exe without having to retype function.
#'
#'    To exit session, type quit().
#'
#'    clearRev() and getrRev() can be called from within session for ease of use.
#'
#'@param path Path to rb.exe. Defaults to revEnv$RevPath, so
#'    InitRev() must typically be called first.
#'
#'@param viewCode If TRUE, String-formatted code in the temporary file used to interact with
#'    rb.exe will be displayed in the viewing pane. Default is FALSE.
#'
#'@param coerce If FALSE, output from rb.exe will be returned in String format. If
#'    TRUE, repRev() will attempt to coerce output into a suitable R object. Default is TRUE.
#'
#'@param use_wd If T, temporary rb.exe session will use the same working directory as
#'    the active R session. If F, it will use its default. Default is T.
#'
#'@param sleep Integer. If a number of seconds are provided, Sys.sleep() will run after user
#'    rb.exe input is provided for the given number of seconds. This will not occur if sleep is
#'    NULL. This parameter was mostly made for testing purposes. Default is NULL.
#'
#'@examples
#'--The below example code enters, uses, and exits an interactive rb.exe session. Attempting to use
#'R code before quit() may cause an error--
#'
#'repRev()
#'
#'myNumber <- 4
#'myNumber
#'
#'posteriorPredictiveProbability(v(2), 3)
#'
#'getrRev()
#'clearRev()
#'quit()
#'@export
#'
repRev <- function (path = revEnv$RevPath, viewCode = F, coerce = F, use_wd = T, sleep = NULL)
{
  while (TRUE) {
    ginput <- readline(prompt = "rb>>>")
    timestamp(ginput, prefix = "", suffix = "", quiet = TRUE)

    numberOfOpenBraces <- stringr::str_count(ginput, "\\{")
    numberOfClosedBraces <- stringr::str_count(ginput, "\\}")

    numberOfOpenParenthesis <- stringr::str_count(ginput, "\\(")
    numberOfClosedParenthesis <- stringr::str_count(ginput, "\\)")

    testBraces <- numberOfOpenBraces == numberOfClosedBraces
    testParenthesis <- numberOfOpenParenthesis == numberOfClosedParenthesis

    while(!(testBraces && testParenthesis)){

      if(
        (numberOfOpenBraces < numberOfClosedBraces) |
        (numberOfOpenBraces < numberOfClosedBraces)
      ) break();

      ginput <- ginput %+% readline(prompt <- "rb>>>")

      numberOfOpenBraces <- stringr::str_count(ginput, "\\{")
      numberOfClosedBraces <- stringr::str_count(ginput, "\\}")

      numberOfOpenParenthesis <- stringr::str_count(ginput, "\\(")
      numberOfClosedParenthesis <- stringr::str_count(ginput, "\\)")

      testBraces <- numberOfOpenBraces == numberOfClosedBraces
      testParenthesis <- numberOfOpenParenthesis == numberOfClosedParenthesis

    }

    if (ginput == "quit()") {
      break
    }

    if (ginput == "clearRev()"){
      clearRev()
      next
    }

    if(ginput == "getRevVars()"){
      print(getRevVars())
      next()
    }

    if(ginput == "getRevHistory()"){
      print(getRevHistory())
      next()
    }

    else{print(doRev(ginput, viewCode = viewCode, coerce = coerce))}


    if(!is.null(sleep)){
      Sys.sleep(sleep)
    }

  }
}
