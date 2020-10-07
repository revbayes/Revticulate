#'coerces R string object from revenv into a string format that can be read in rb
#'
#'@param outString name of string object in revenv surrounded by '#' and '#'
#'@return coerceString: String format of outString that can be read into rb
#'
#'@examples getSS("#myString#")
#'
#'@export
getSS <- function(outString){

  initSTR <- get(outString %+% ".str", envir = revenv)

  coerceString <- '"' %+% initSTR %+% '"'

  return(coerceString)
}
