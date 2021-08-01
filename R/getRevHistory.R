#'Get full history of rb code from RevEnv
#'
#'Prints revEnv$allCode
#'
#'@import comprehenr
#'
#'@export
#'
getRevHistory <- function(){

  return(readLines(Sys.getenv("RevHistory"), warn = FALSE))

}



