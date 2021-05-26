#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode see Rev code input and output in the viewing pane
#'
#'@export
#'

doRev <- function(input, viewCode = FALSE, coerce = FALSE, useHistory = FALSE){
  revEnv$allCode <- readLines(revEnv$revHistory, warn = F)

  try({
  first <- callRev(getRevHistory(), coerce = F)
  revEnv$allCode <- c(revEnv$allCode, input)
  last <- callRev(getRevHistory(), coerce = F, viewCode = viewCode)
  }, silent = T)
    if(length(first) != 0)
       now <- last[-c(1:length(first))]
    else now <- last

    if(length(now) == 0)
      return("")

  if (any(stringr::str_detect(now, pattern = "Error:|error|Missing Variable:"))) {
    revEnv$allCode <- revEnv$allCode[-c(length(revEnv$allCode))]
    if(coerce){
      message(now)
      return("")
      }
  }
  else {
    cat(input, file = revEnv$revHistory, append = TRUE, sep = "\n")
  }

  #update revEnv$vars
  for(j in unlist(stringr::str_split(input, ";"))){
    if(stringr::str_detect(j, "<-| = |:=|~"))
      revEnv$vars <- c(revEnv$vars, j)
  }

  if(coerce)
    return(coerceRev(now))

  now <- stringr::str_squish(now)


  return(now)
}
