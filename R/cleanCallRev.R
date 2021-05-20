#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@export
#'

cleanCallRev <- function(input){

  first <- callRev(revEnv$allCode, coerce = F)
  revEnv$allCode <- c(revEnv$allCode, input)
  last <- callRev(revEnv$allCode, coerce = F)

    if(length(first) != 0)
       now <- last[-c(1:length(first))]
    else now <- last

  return(now)
}
