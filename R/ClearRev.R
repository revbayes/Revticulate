#'Empties RevEnv
#'
#'Removes all objects from RevEnv excluding RevPath
#'
#'
#'@export
ClearRev <- function(){

  NRObjs <- length(RevEnv)-1
  if(any(ls(RevEnv) == "Deterministic")){NRObjs <- NRObjs - 1}
  if(any(ls(RevEnv) == "Vars")){NRObjs <- NRObjs - 1}

  remove(list = ls(envir = RevEnv)[which(stringr::str_detect(ls(envir = RevEnv),
                                                             "RevPath", negate = TRUE))], envir = RevEnv)

  message("Successfully removed " %+% c(NRObjs) %+% " objects from RevEnv!")
}
