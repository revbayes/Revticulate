#'coerces R list or matrix objects from revenv into a string format that can be read in rb
#'
#'@param outMatrix name of list or matrix object in revenv surrounded by '#' and '#'
#'@return coercedMatrix: String format of outMatrix that can be read into rb
#'
#'@examples getMS("#myMatrix#")
#'
#'@export
getMS <- function(outMatrix){

  initSTR <- get(outMatrix %+% ".str", envir = revenv)

  matrix1 <- stringr::str_replace_all(stringr::str_flatten(stringr::str_replace_all(initSTR, "] ,", replacement = "][")), " ", "")
  matrix2 <- stringr::str_replace_all(matrix1, "\\]\\[", "\\],\\[")
  coercedMatrix <- 'matrix(' %+% matrix2 %+% ')'

  return(coercedMatrix)
}
