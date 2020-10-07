#'coerces R phylo objects from revenv into a string format that can be read in rb
#'
#'@param outTree name of phylo object in revenv surrounded by '#' and '#'
#'@return revSTR: String format of outTree that can be read into rb
#'
#'@examples getTS("#myTree#")
#'
#'@export

getTS <- function(outTree){

  revSTR <- "readTrees(" %+% get(outTree, envir = revenv) %+% ")"

  return(revSTR)}
