#'Getter function to retrieve objects created in rb.exe.
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
  return(cleanCallRev(name, viewCode = FALSE, coerce = coerce))
}
