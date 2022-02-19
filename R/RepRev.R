#' Interactive Session with RevBayes
#'
#'    Simulates a continuous, interactive session with RevBayes. While this session is active, all code will be interpreted as Rev code, and attempting to run R code may result in error.
#'
#'    By default, the interactive session uses the present R working directory as the RevBayes working directory. This behavior can be turned off with use_wd = FALSE
#'
#'    The exit the session, type 'quit()', 'q()', or hit the 'esc' key.
#
#'    clearRev(), getRevVars(), and getRevHistory() can still be called from within the session for user convenience
#'
#'@param path Path to the RevBayes executable. Defaults to Sys.getenv("rb"), which should be assigned upon first loading the package.
#'
#'@param viewCode If TRUE, code from the temporary file used to interact with RevBayes will be displayed in
#'                the viewing pane. Default is FALSE. The option is mostly for developer convenience, and can be ignored
#'                by most users.
#'
#'@param coerce If FALSE, RevBayes output will be printed to the console in character format. If
#'    TRUE, coerceRev() will attempt to coerce output into a suitable R object. Default is TRUE.
#'
#'@param use_wd If TRUE, the simulated Revbayes session will use the same working directory as
#'    the active R session. If FALSE, it will use the default for the RevBayes executable. Default is TRUE.
#'
#'@return No return. RevBayes variables assigned within the session can be accessed externally via doRev() or viewed with getRevVars().
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
        (numberOfOpenParenthesis < numberOfClosedParenthesis)
      ) break();

      ginput <- ginput %+% "\n" %+% readline(prompt <- "rb>>> ")

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
    else if (ginput == "clearRev()"){
      clearRev()
      next
    }
    else if(str_detect(ginput, "clearRev\\(([0-9]+)\\)")){
      clearRev(as.integer(str_extract(ginput, "[0-9]+")))
      next
    }
    else if(ginput == "getRevVars()"){
      cat(getRevVars(), sep = "\n")
      next()
    }
    else if(str_detect(ginput, pattern = "^getRevVars\\(\".+\"\\)$")){
      args <- str_remove_all(str_extract(ginput, "\\(.+\\)"), "\\(|\\)|\"")
      cat(getRevVars(args), sep = "\n")
      next()
    }
    else if(ginput == "getRevHistory()"){
      cat(getRevHistory(), sep = "\n")
      next()
    }
    else{
      #ginput <- str_replace_all(ginput, "\\{", "\\{\n\t")
      #ginput <- str_replace_all(ginput, "\\}", "\n\\}\n\t")
      if(coerce)
        cat(capture.output(doRev(ginput, viewCode = viewCode, coerce = coerce)), sep = "\n")
      else
        cat(doRev(ginput, viewCode = viewCode, coerce = coerce), sep = "\n")
      }


  }
}
