#'Get full history of rb code from RevEnv
#'
#'Returns a vector of the lines of code in the .Revhistory file
#'
#'@import comprehenr
#'
#'@export
#'
getRevHistory <- function(){

  lines <- readLines(Sys.getenv("RevHistory"), warn = FALSE)

  return(lines)
}



