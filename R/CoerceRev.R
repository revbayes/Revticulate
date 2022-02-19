#' Convert RevBayes Output into R Objects
#'
#' Coerces a RevBayes output string into an equivalent R object.
#' Automatically determines R object type based on the structure of the string,
#' and returns the original string if R object equivalent cannot be determined.
#'
#' @param revString character - Output from RevBayes
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
#'@import usethis
#'
#' @export
#'
coerceRev <- function(revString){

  rObj <- stringr::str_squish(revString)

  if(length(revString) > 1){
    rObj <- rObj[rObj != ""]
  }

  if(length(rObj) == 1){
    if(stringr::str_detect(rObj, "^inf?")){
      return(Inf)
    }
    if(stringr::str_detect(rObj, "^NULL?")){
      return(NULL)
    }
    if(stringr::str_detect(rObj, "^nan?")){
      return(NA)
    }
    if(stringr::str_detect(rObj, "^TRUE?")){
      return(TRUE)
    }
    if(stringr::str_detect(rObj, "^FALSE?")){
      return(FALSE)
    }
    if(paste0(rObj) == ""){
      return(invisible(""))
    }
  }

  count <- 0
  copy <- c()
  for(i in rObj){

      if(count < 0){
        lastElement <- copy[length(copy)]
        copy[length(copy)] <- paste0(lastElement, i)
      }
      else{
        copy <- append(copy, i)
      }
      count <- count - stringr::str_count(i, "\\(")
      count <- count + stringr::str_count(i, "\\)")

    }
  rObj <- copy

  validateTree <- function(treeString) {
    isTree <- !is.null(ape::read.tree(text = treeString))
    return(isTree)
  }

  toJSONVector <- function(stringArg) {
    return(stringr::str_c("[", stringArg, "]"))
  }

  detectNumeric <- function(x){
    stringr::str_detect(x, "^[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+$")
  }

  if(jsonlite::validate(rObj) == TRUE){
    rObj <- jsonlite::fromJSON(rObj)
    return(rObj)
  }

  else if(validateTree(rObj)){
    rObj <- ape::read.tree(text = rObj)
    return(rObj)
  }


  if(all(detectNumeric(rObj))){
    return(as.numeric(rObj))
  }

  return(rObj)

}


