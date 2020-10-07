#'coerces R vector object from revenv into a string format that can be read in rb
#'
#'@param outVector name of vector object in revenv surrounded by '#' and '#'
#'@return revVec: String format of outVector that can be read into rb
#'
#'@examples getVS("#myVector#")
#'
#'@export
getVS <- function(outVector){
  initVec <- get(outVector %+% ".str", envir = revenv)
  revVec <- stringr::str_squish(initVec)
  return(revVec)
}
