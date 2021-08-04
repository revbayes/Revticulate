#'Getter function to retrieve objects created in RevBayes
#'
#'@param name String. Name of object to retrieve
#'
#'@param coerce boolean. If true, Revticulate attempts to coerce the String formatted Rev object into
#'                       a comparable R object. Default is FALSE.
#'
#'
#'@export
#'

getRevObj <- function(name, coerce = FALSE){
  return(doRev(str_squish(name), viewCode = FALSE, coerce = coerce))
}
