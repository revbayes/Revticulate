#'Continuous interactive session with rb.exe
#'
#'Allows user to continuously call rb.exe without having to retype function.
#'
#'    To exit session, type quit().
#'
#'    clearRev() and getrRev() can be called from within session for ease of use.
#'
#'@param path Path to rb.exe. Defaults to Sys.getenv(RevBayesPath), so
#'    initRev() may need to be called first.
#'
#'@param useHistory Should rb input be saved to .Revhistory? If so, the up arrow key can be used
#'    while repRev() is active to navigate previous inputs. Default is TRUE.
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
#'
#'@examples
#' \dontrun{
#'repRev()
#'
#'myNumber <- 4
#'myNumber
#'
#'posteriorPredictiveProbability(v(2), 3)

#'getrRev()
#'clearRev()
#'quit()
#'}
#'@export
#'
repRev <- function (path = Sys.getenv("RevBayesPath"), useHistory = T, viewCode = F, coerce = F, use_wd = T)
{
  while (TRUE) {
    ginput <- readline(prompt = "rb>>>")

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

    if(str_detect(ginput, "clearRev\\(([0-9]+)\\)")){
      clearRev(as.integer(str_extract(ginput, "[0-9]+")))
      next
    }

    if(ginput == "getRevVars()"){
      cat(getRevVars(), sep = "\n")
      next()
    }

    if(ginput == "getRevHistory()"){
      cat(getRevHistory(), sep = "\n")
      next()
    }


    else{cat(doRev(ginput, viewCode = viewCode, coerce = coerce))}


  }
}
