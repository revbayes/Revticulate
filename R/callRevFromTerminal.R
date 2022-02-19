#'Execute a .rev file in RevBayes through an RStudio terminal
#'
#'Accesses an RStudio terminal through the RStudio API and runs a .rev script.
#'This allows users to submit mcmcs and longer scripts after writing them in R Markdown through knitr or interactively with repRev()
#'
#'@param revscript .rev file to execute in RevBayes
#'
#'@return termID Unique identifier of the terminal used to call RevBayes
#'
#'@examples
#'\dontrun{
#'saveRev("archertutorial.rev", use_quit=TRUE)
#'callRevFromTerminal("archertutorial.rev")
#'}
#'
#'@export
callRevFromTerminal <- function(revscript){
  rbPath <- Sys.getenv("rb")

  script <- normalizePath(revscript, winslash = "/")
  wd <- normalizePath(getwd(), winslash = "/")
  if(Sys.info()['sysname'] == 'Windows'){
    script <- str_replace_all(script, "/", "//")
    wd <- str_replace_all(wd, "/", "//")
  }

  termID = rstudioapi::terminalExecute(paste(rbPath, script))

  return("")
}
