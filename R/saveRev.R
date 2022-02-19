#'Save Revticulate History
#'
#'Captures the current Revticulate history and saves it to an external file.
#'
#'By providing a file path, the user can save the RevBayes code in their current
#'Revticulate session to an external .Rev script.
#'This script can then be executed in RevBayes via source(),
#'or in an RStudio terminal via callRevFromTerminal().
#'Usage of this function can be combined with loadRev() to allow
#'for the continual use of a Revticulate history instance between R sessions.
#'
#'@param filepath Name of a .Rev file to save the Revticulate history in. If the file doesn't exist, it will be created. Otherwise, it will be overwritten.
#'@param use_wd If TRUE, the history will be prepended by setwd({'users current working directory'}). This function will set the script's working directory to
#'              the user's current working directory, which is often desirable for files that will be submitted to callRevFromTerminal() and contain output moniters.
#'@param use_quit If TRUE, q() will be appended to the end of the history before writing it to a file.
#'                This will cause the RevBayes session to quit after evaluating the script's code, and is good practice
#'                when running an mcmc in an RStudio terminal.
#'
#'@examples
#' \dontrun{
#        saveRev("mySavedCode.rev")
#'       saveRev("mySavedCode.rev", use_quit=FALSE)
#'       saveRev("mySavedCode.rev", use_wd=FALSE, use_quit=TRUE)
#' }
#'
#'
#'@export
#'
saveRev <- function(filepath, use_wd=TRUE, use_quit=TRUE){
  if(!file.exists(filepath)){
    file.create(filepath)
  }

  text <- getRevHistory()
  if(use_wd){
    wd <- normalizePath(getwd(), winslash = "/")
    if(Sys.info()['sysname'] == 'Windows'){
      wd <- str_replace_all(wd, "/", "//")
    }
    text <- unlist(c(paste0('setwd("', wd ,'")\n'), text))
  }
  if(use_quit){
    text <- unlist(c(text, "\nq()"))
  }

  cat(text, file = filepath, sep = "\n")
}
