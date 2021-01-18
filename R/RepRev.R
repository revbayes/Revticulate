#'Continuous interactive session with rb.exe
#'
#'Allows user to continuously call rb.exe without having to retype function.
#'
#'    To exit session, type quit().
#'
#'    ClearRev() and GetRev() can be called from within session for ease of use.
#'
#'@param path Path to rb.exe. Defaults to RevEnv$RevPath, so
#'    InitRev() must typically be called first.
#'
#'@param viewCode If TRUE, String-formatted code in the temporary file used to interact with
#'    rb.exe will be displayed in the viewing pane. Default is FALSE.
#'
#'@param coerce If FALSE, output from rb.exe will be returned in String format. If
#'    TRUE, RepRev() will attempt to coerce output into a suitable R object. Default is TRUE.
#'
#'@param use_wd If T, temporary rb.exe session will use the same working directory as
#'    the active R session. If F, it will use its default. Default is T.
#'
#'@examples
#'--The below example code enters, uses, and exits an interactive rb.exe session. Attempting to use
#'R code before quit() may cause an error--
#'
#'RepRev()
#'
#'myNumber <- 4
#'myNumber
#'
#'posteriorPredictiveProbability(v(2), 3)
#'
#'GetRev()
#'ClearRev()
#'quit()
#'@export
#'
RepRev <- function (path = RevEnv$RevPath, viewCode = F, coerce = TRUE, use_wd = T)
{
  while (TRUE) {
    ginput <- readline(prompt = "rb>>>")
    timestamp(ginput, prefix = "", suffix = "", quiet = TRUE)

    if (ginput == "quit()") {
      break
    }

    if (ginput == "ClearRev()"){
      ClearRev()
      next
    }

    if(ginput == "GetRev()"){
      print(GetRev())
      next()
    }

    else{doRev(ginput, viewCode = viewCode, use_wd = use_wd, interactive = TRUE)}

    RevEnv$Vars <- unique(RevEnv$Vars)

  }
}






