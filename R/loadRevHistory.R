#'Load Revticulate History from a .Rev File
#'
#'Reads in the code from a .Rev file and writes it into the current Revticulate history.
#'
#'@param filepath character - File to load .Revhistory from
#'@param overwrite logical - If TRUE, the code in the file at 'filepath' will overwrite current .Revhistory. If FALSE, it will be appended to the end of it.
#'
#'@examples
#' \dontrun{
#'      loadRevHistory("someCode.rev")
#'
#'      loadRevHistory("someMoreCode.rev", overwrite=TRUE)
#' }
#'
#'@return No return - reads in new code to the .Revhistory file
#'
#'@export
#'
loadRevHistory <- function(filepath, overwrite=FALSE){
  if(!file.exists(filepath)){
    errorCondition("Provided file path does not exist!")
  }
  else{
    rbHistory <- readLines(filepath)
    cat(rbHistory, file = Sys.getenv("revHistory"), sep = "\n", append = !overwrite)
  }
}
