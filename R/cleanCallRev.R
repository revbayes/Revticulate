#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode see Rev code input and output in the viewing pane
#'
#'@export
#'

cleanCallRev <- function(input, viewCode = FALSE){

  first <- callRev(getRevHistory(), coerce = F)
  revEnv$allCode <- c(revEnv$allCode, input)
  last <- callRev(getRevHistory(), coerce = F, viewCode = viewCode)

    if(length(first) != 0)
       now <- last[-c(1:length(first))]
    else now <- last

  if(length(now) == 0)
     now = ""

  if(length(now) == 1)
    if(now == "")
      revDefine(RevOut = input)

  return(now)
}
