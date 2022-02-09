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
    simplifiedTree <- stringr::str_remove_all(treeString, "[^)(,]")

    openCount <- sum(stringr::str_count(simplifiedTree, "\\("))
    closeCount <- sum(stringr::str_count(simplifiedTree, "\\)"))

    if(openCount == closeCount & openCount > 1){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }

  toJSONVector <- function(stringArg) {
    return(stringr::str_c("[", stringArg, "]"))
  }

  detectNumeric <- function(x){
    stringr::str_detect(x, "^[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+$")
  }

  isDiscreteCharacterMatrix <- function(data){
    cleanedData <- str_remove_all(data, "\"")
    points <- (str_detect(cleanedData, "(^(([:alnum:]|-)\\s)+([:alnum:]|-))$"))
    if(length(points[points == FALSE]) >= 1 & (length(points[points == TRUE]) >= length(points[points == FALSE]))){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }

  asDiscreteCharacterMatrix <- function(data){
    cleanedData <- str_remove_all(data, "\"")
    points <- (str_detect(cleanedData, "(^(([:alnum:]|-)\\s)+([:alnum:]|-))$"))

    charNames <- cleanedData[!points]
    charData <- c()

    starts <- which(!points)+1
    stops <- c((which(!points)-1)[-c(1)], length(points))

    for(i in 1:length(starts)){
      start <- starts[i]
      stop <- stops[i]
      charData <- append(charData, paste0(cleanedData[start:stop], collapse = " "))
    }

    charData <- as.list(charData)
    charData <- stringr::str_split(charData, " ")

    charData <- as.matrix(charData)
    names(charData) <- charNames
    return(charData)
  }

  if(jsonlite::validate(rObj) == TRUE){
    rObj <- fromJSON(rObj)
    return(rObj)
  }

  else if(validateTree(rObj)){
    rObj <- ape::read.tree(text = rObj)
    return(rObj)
  }

  if(all(detectNumeric(rObj))){
    return(as.numeric(rObj))
  }

  if(length(rObj) >= 2)
  if(isDiscreteCharacterMatrix(rObj)){
    return(asDiscreteCharacterMatrix(rObj))
  }

  return(rObj)

}


