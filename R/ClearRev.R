
#'Empties revEnv
#'
#'Clears all code, or a user-specified amount of code, and vars from revEnv.
#'
#'@param n How many lines to remove. Should be an integer
#'
#'@examples
#' \dontrun{
#' clearRev() #Clear all objects from RevBayes
#' clearRev(n = 1) # Clear the last line input to RevBayes
#' }
#'@export
clearRev <- function(n = NULL){

  #prevent temp file list from getting too large
  if(length(revEnv$temps) > 50){
    revEnv$temps <- c()
  }

  cat("", file = revEnv$revHistory, append = F)

  if (is.null(n)){
  remove(list = ls(envir = revEnv)[which(ls(envir = revEnv) != "RevPath" & ls(envir = revEnv) != "temps" & ls(envir = revEnv) != "revHistory")],
         envir = revEnv)

  message("Successfully reset revEnv!")
  } else if (!is.null(n)){
    file = readLines(revEnv$revHistory)
    remove = length(file)-n
    edited = file[-(remove : length(file))]
    writeLines(revEnv$revHistory)
    callRev(revEnv$revHistory)
  }

}
