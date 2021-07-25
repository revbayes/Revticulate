#'Get full history of rb code from RevEnv
#'
#'Prints revEnv$allCode
#'
#'@import comprehenr
#'
#'@export
#'
getRevHistory <- function(){

  return(comprehenr::to_vec(for(i in revEnv$allCode) i))

}



