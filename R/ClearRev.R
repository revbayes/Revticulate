#'Empties RevEnv
#'
#'Removes all objects from RevEnv excluding RevPath
#'
#'
#'@export
ClearRev <- function(){

  NRObjs <- length(RevEnv)-2


  remove(list = ls(envir = RevEnv)[which(ls(envir = RevEnv) != "RevPath" & ls(envir = RevEnv) != "temps")],
         envir = RevEnv)

  message("Successfully removed " %+% c(NRObjs) %+% " objects from RevEnv!")
}
