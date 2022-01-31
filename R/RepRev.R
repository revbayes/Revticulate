#' Continuous interactive session with RevBayes
#'
#'    Simulates a continuous, interactive session with RevBayes. While this session is active, all code will be interpreted as Rev code, and attempting to run R code may result in error.
#'
#'    By default, the interactive session uses the present R working directory as the RevBayes working directory. This behavior can be turned off with use_wd = FALSE
#'
#'    The exit the session, type 'quit()' or hit the 'esc' key.
#
#'    clearRev(), getRevVars(), and getRevHistory can be called from within the session for user convenience
#'
#'@param path Path to the RevBayes executable. Defaults to Sys.getenv("RevBayesPath"), so
#'    initRev() should be called first.
#'
#'@param viewCode If TRUE, code from the temporary file used to interact with
#'    RevBayes will be displayed in the viewing pane. Default is FALSE.
#'
#'@param coerce If FALSE, output from RevBayes will be printed to the console in character format. If
#'    TRUE, repRev() will attempt to coerce output into a suitable R object. Default is FALSE.
#'
#'@param use_wd If TRUE, the simulated Revbayes session will use the same working directory as
#'    the active R session. If FALSE, it will use its default. Default is TRUE.
#'
#'@return No return. Acts as an interactive session with the RevBayes. RevBayes output is printed to the console via cat(), and Rev variables can be referenced externally via doRev() and getRevObj().
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
repRev <- function (path = Sys.getenv("rb"), viewCode = FALSE, coerce = TRUE, use_wd = TRUE)
{
  while (TRUE) {
    ginput <- readline(prompt = "rb>>> ")

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

    if (ginput == "quit()" || ginput == "q()") {
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

    if(str_detect(ginput, pattern = "^getRevVars\\(\".+\"\\)$")){
      args <- str_remove_all(str_extract(ginput, "\\(.+\\)"), "\\(|\\)|\"")
      cat(getRevVars(args), sep = "\n")
      next()
    }


    if(ginput == "getRevHistory()"){
      cat(getRevHistory(), sep = "\n")
      next()
    }


    else{
      if(coerce)
        cat(capture.output(doRev(ginput, viewCode = viewCode, coerce = coerce)), sep = "\n")
      else
        cat(doRev(ginput, viewCode = viewCode, coerce = coerce), sep = "\n")
      }


  }
}
