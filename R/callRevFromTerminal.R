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
  wd <- normalizePath(getwd(), winslash = "/")
  if(Sys.info()['sysname'] == 'Windows'){
    script <- str_replace_all(script, "/", "//")
    wd <- str_replace_all(wd, "/", "//")
  }

  his <- getRevHistory()
  his[1] <- paste0('source("', wd, '")')
  cat(his, file=Sys.getenv("revHistory"), sep = "\n", append = FALSE)

  termID = rstudioapi::terminalExecute(paste(rbPath, script))

  his[1] <- paste0('#START\n')
  cat(his, file=Sys.getenv("revHistory"), sep = "\n", append = FALSE)
  return(termID)
}
