#'Getter function to retrieve objects created in RevBayes
#'
#'@param name String. Name of object to retrieve
#'
#'@param coerce Boolean. If true, Revticulate attempts to coerce the String formatted Rev object into
#'                       a comparable R object. Default is FALSE.
#'
#'@return revObject: type varies. Object returned from RevBayes that was previously defined in .Revhistory. If coerce == FALSE, returns character. Else, returns type determined by coerceRev().
#'
#'@export
#'

getRevObj <- function(name, coerce = FALSE){
  revObject <- doRev(str_squish(name), viewCode = FALSE, coerce = coerce)
  return(revObject)
}
