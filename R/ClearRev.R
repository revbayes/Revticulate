
#'Empties revEnv
#'
#'Clears allCode and vars from revEnv.
#'
#'
#'@export
clearRev <- function(){

  #prevent temp file list from getting too large
  if(length(revEnv$temps) > 50){
    revEnv$temps <- c()
  }

  cat("", file = revEnv$revHistory, append = F)

  remove(list = ls(envir = revEnv)[which(ls(envir = revEnv) != "RevPath" & ls(envir = revEnv) != "temps" & ls(envir = revEnv) != "revHistory")],
         envir = revEnv)

  message("Successfully reset revEnv!")

}
