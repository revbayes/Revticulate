#'Execute a .rev file in RevBayes through the R studio terminal
#'
#'@param revscript .rev file to execute in RevBayes
#'
#'@return termID Unique identifier of the terminal used to call RevBayes
#'
#'@export
#'
callRevFromTerminal <- function(revscript){
  rbPath <- Sys.getenv("rb")
  script <- normalizePath(revscript, winslash = "/")
  if(Sys.info()['sysname'] == 'Windows'){
    script <- str_replace_all(script, "/", "//")
  }
  termID = rstudioapi::terminalExecute(paste(rbPath, script))
  return(termID)
}
