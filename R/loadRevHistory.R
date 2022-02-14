#'Load in Rev history from a .Rev file
#'
#'
#'@param filepath File to load .Revhistory from
#'@param overwrite If TRUE, history load from filepath will overwrite current .Revhistory. If FALSE, it will be appended to the end of it.
#'
#'@examples
#' \dontrun{
#
#' }
#'
#'
#'@export
#'
loadRevHistory <- function(filepath, overwrite=FALSE){
  if(!file.exists(filepath)){
    errorCondition("Provided file path does not exist!")
  }
  else{
    rbHistory <- readLines(filepath)
    cat(rbHistory, file = Sys.getenv(Sys.getenv("revHistory")), sep = "\n", append = !overwrite)
  }
}
