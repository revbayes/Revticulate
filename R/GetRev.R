#'Get variable definitions from RevEnv
#'
#'Prints RevEnv$Vars to return definitions of variables that have been defined in rb.exe
#'    using Revticulate function
#'
#'@export
#'
GetRev <- function(){
  return(RevEnv$Vars)}
