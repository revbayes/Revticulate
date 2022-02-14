#'Save Revticulate history in a .Rev file for use in RevBayes
#'
#'
#'@param filepath File to save .Revhistory to
#'
#'@examples
#' \dontrun{
#
#' }
#'
#'
#'@export
#'
saveRev <- function(filepath){
  if(!file.exists(filepath)){
    file.create(filepath)
  }
  cat(getRevHistory(), file = filepath, sep = "\n")
}
