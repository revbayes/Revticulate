#'Load in Rev history from a .Rev file
#'
#'Reads in the code from a .Rev file and writes it into the current Rev history.
#'
#'@param filepath File to load .Revhistory from
#'@param overwrite If TRUE, the code in the file at 'filepath' will overwrite current .Revhistory. If FALSE, it will be appended to the end of it.
#'
#'@examples
#' \dontrun{
#'      loadRevHistory("someCode.rev")
#'      loadRevHistory("someMoreCode.rev", overwrite=TRUE)
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
