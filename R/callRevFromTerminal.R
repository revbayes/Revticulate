#'Execute a .rev file in RevBayes Through an RStudio Terminal
#'
#'Accesses an RStudio terminal through the RStudio API and runs a .rev script.
#'
#'Many common RevBayes use-cases, such as generating mcmcs, take a long time to execute.
#'Because of this fact, it is inefficient and inadvisable to run them in knitr documents
#'or with RevBayes calls through repRev(). Instead, tutorial code or code that has been written
#'interactively can be externally saved with saveRev(), and then executed in an RStudio terminal
#'via callRevFromTerminal(). Output files can then be explored and visualized with the
#'RevGadgets package.
#'
#'@param revscript character - .rev file to execute in RevBayes
#'
#'@return termID Unique identifier of the terminal used to call RevBayes
#'
#'@examples
#'\dontrun{
#'saveRev("archertutorial.rev", use_quit=TRUE)
#'callRevFromTerminal("archertutorial.rev")
#'}
#'
#'@import rstudioapi
#'
#'@export
callRevFromTerminal <- function(revscript){
  rbPath <- Sys.getenv("rb")

  script <- normalizePath(revscript, winslash = "/")

  termID = terminalExecute(paste(rbPath, script))

  return("")
}
