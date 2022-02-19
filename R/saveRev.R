#'Save Revticulate history in a .Rev file for later use
#'
#'Captures the current RevBayes history and saves it to an external file.
#'
#'The user can then choose to run it in RevBayes directly with source,
#'or in an RStudio terminal via callRevFromTerminal().
#'
#'Usage of this function can be combined with loadRev() to allow
#'for the continual use of the same RevBayes history between R sessions.
#'
#'@param filepath File to write the RevBayes history to
#'@param use_wd If TRUE, the history will be prepended by setwd('users current working directory'). This
#'              is especially helpful when writing a file for running an mcmc, as any Moniters would otherwise
#'              write output to the parent direcory of the RevBayes executable. Default is TRUE.
#'@param use_quit If TRUE, q() will be appended to the end of the RevBayes history before writing it to a file.
#'                This will cause the RevBayes session to quit after evaluated the previous code, and is good practice
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
