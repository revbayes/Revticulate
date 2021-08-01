#'Get variable definitions from RevEnv
#'
#'Prints revEnv$vars to return definitions of variables that have been defined in rb.exe
#'    using a Revticulate function.
#'
#'@export
#'
getRevVars <- function(){

  cat(
    grep("<-| = |:=|~", readLines(Sys.getenv("RevHistory")), value = TRUE)
    )

}
