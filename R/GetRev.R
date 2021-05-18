#'Get variable definitions from RevEnv
#'
#'Prints revEnv$Vars to return definitions of variables that have been defined in rb.exe
#'    using a Revticulate function.
#'
#'@export
#'
getRev <- function(){

  return(revEnv$Vars)

}
