#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode see Rev code input and output in the viewing pane
#'
#'@export
#'

cleanCallRev <- function(input, viewCode, viewCode = FALSE){

  first <- callRev(revEnv$allCode, coerce = F, viewCode = viewCode)
  revEnv$allCode <- c(revEnv$allCode, input)
  last <- callRev(revEnv$allCode, coerce = F, viewCode = viewCode)

    if(length(first) != 0)
       now <- last[-c(1:length(first))]
    else now <- last

  return(now)
}
