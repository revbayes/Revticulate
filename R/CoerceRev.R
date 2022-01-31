#' Coerces string of RevBayes output into an equivalent R object.
#'
#' Coerces string of RevBayes output into an equivalent R object.
#' Uses the structure of the characters within the string to identify an appropriate R object type (a list, vector,
#' or even phylo tree) to coerce the string into.
#'
#' @param revString String formatted representation of a Rev language object.
#'            coerceRev() will recognize the formatting of most commonly used
#'            Rev objects and will convert them into equivalent R objects. If coerceRev()
#'            does not recognize the objects formatting, the initial String representation
#'            will be returned.
#'
#' @return rObject: Type varies depending on Rev object type. R object-formatted output coerced from a RevBayes output string.
#'
#' @examples
#' \dontrun{
#' coerceRev("[1, 2, 3, 4]")
#'}
#'@import stringr
#'@import comprehenr
#'@import phytools
#'@import jsonlite
#'
#' @export
#'


coerceRev <- function(revString){

  rObj <- stringr::str_squish(revString)

  if(length(revString) > 1)
    rObj <- rObj[which(rObj != "")]

  if(jsonlite::validate(rObj) == TRUE){
    rObj <- fromJSON(rObj)
    return(rObj)
  }

  toJSONVector <- function(stringArg) {
    return(stringr::str_c("[", stringArg, "]"))
  }

  if(jsonlite::validate(toJSONVector(rObj)) == TRUE){
    rObj <- fromJSON(toJSONVector(rObj))
    return(rObj)
  }

  validateTree <- function(treeString) {
    simplifiedTree <- stringr::str_remove_all(tree1, "[^)(,]")

    openCount <- stringr::str_count(simplifiedTree, "\\(")
    closeCount <- stringr::str_count(simplifiedTree, "\\)")

    if(openCount == closeCount & openCount > 1){
      return(TRUE)
    }
    return(FALSE)
  }

  if(validateTree(rObj)){
    rObj <- ape::read.tree(text = rObj)
    return(rObj)
  }

  return(rObj)
}



