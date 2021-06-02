#' Undo n number of previous Rev commands.
#'
#' Undo the last specified number of unputs from allCode. Prints revEnv$allCode to
#' the screen to show the user the new Rev code history.
#'
#' @param n Number of prior unputs to remove from allCode.
#'
#' @export
undoRev <- function(n){
  revEnv$allCode <- revEnv$allCode[1:(length(revEnv$allCode)-n)]
  cat(getRevHistory(), file = revEnv$revHistory, append = F)

  revEnv$vars <- c()

  #update revEnv$vars
  for(j in getRevHistory()) {
    if(stringr::str_detect(j, "<-| = |:=|~"))
      revEnv$vars <- c(revEnv$vars, j)
  }

  cat(getRevHistory(), sep = "\n")
}








