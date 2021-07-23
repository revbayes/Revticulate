#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode If true, Rev code input and output will be displayed in the viewing pane.
#'
#'@param coerce If true, the output from RevBayes will be coerced into R format with coerceRev()
#'
#'@param timeout Determines how long the system2() call should wait before timing out (seconds). Default is 5.
#'
#'@export
#'

doRev <- function(input, viewCode = FALSE, coerce = FALSE, timeout = 5){
  revEnv$allCode <- readLines(revEnv$revHistory, warn = F)

  try({
    first <- callRev(getRevHistory(), coerce = F, timeout = timeout)
    revEnv$allCode <- c(revEnv$allCode, input)
    last <- callRev(getRevHistory(), coerce = F, viewCode = viewCode, timeout = timeout)
  }, silent = T)
  if(length(first) != 0)
    now <- last[-c(1:length(first))]
  else now <- last

  if(length(now) == 0)
    now <- ""

  if (any(str_detect(now, pattern = "Error:|error|Missing Variable:"))) {
    revEnv$allCode <- revEnv$allCode[-c(length(revEnv$allCode))]
    if(coerce){
      message(stringr::str_squish(now))
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


  now <- stringr::str_squish(now)

  if(length(now) > 1)
    now <- now[which(now != "")]


  if(coerce){
    return(coerceRev(now))#paste(now, collapse = "")))
  }

  return(now)
}
