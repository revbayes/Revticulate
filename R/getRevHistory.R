#'Get full history of RevBayes code from RevEnv
#'
#'Returns a vector of the lines of code in the .Revhistory file
#'
#'@import comprehenr
#'
#'@return lines: character. Lines read from .Revhistory file.
#'
#'@export
#'
getRevHistory <- function(){

  lines <- readLines(Sys.getenv("revHistory"), warn = FALSE)

  return(lines)
}



