#'Empties revEnv
#'
#'Removes all objects from revEnv excluding RevPath
#'
#'
#'@export
clearRev <- function(){

  #prevent temp file list from getting too large
  if(length(revEnv$temps) > 50){
    revEnv$temps <- c()
  }

  cat("", revEnv$revHistory, append = F)

  remove(list = ls(envir = revEnv)[which(ls(envir = revEnv) != "RevPath" & ls(envir = revEnv) != "temps")],
         envir = revEnv)

  message("Successfully reset revEnv!")

}
