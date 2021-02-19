#'Empties RevEnv
#'
#'Removes all objects from RevEnv excluding RevPath
#'
#'
#'@export
ClearRev <- function(){

  NRObjs <- length(RevEnv)-1


  remove(list = ls(envir = RevEnv)[which(stringr::str_detect(ls(envir = RevEnv),
                                                             c("RevPath"), negate = T))], envir = RevEnv)

  message("Successfully removed " %+% c(NRObjs) %+% " objects from RevEnv!")
}
