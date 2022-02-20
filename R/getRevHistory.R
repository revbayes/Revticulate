#'Get Revticulate History
#'
#'Return the history of RevBayes code submitted with doRev() and similar functions
#'
#'@import comprehenr
#'
#'@return lines - character. A vector of the line-by-line Revticulate history
#'
#'@examples
#' \dontrun{
#' getRevHistory()
#'
#' cat(getRevHistory(), sep="\n")
#' }
#'
#'@export
#'
getRevHistory <- function(){

  lines <- readLines(Sys.getenv("revHistory"), warn = FALSE)

  return(lines)
}



