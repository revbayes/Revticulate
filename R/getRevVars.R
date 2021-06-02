#'Get variable definitions from RevEnv
#'
#'Prints revEnv$vars to return definitions of variables that have been defined in rb.exe
#'    using a Revticulate function.
#'
#'@export
#'
getRevVars <- function(){

  cat(stringr::str_squish(revEnv$vars), sep = "\n")

}
