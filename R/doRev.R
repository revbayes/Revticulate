#'Wrapper for cleanCallRev(). Makes running multiple lines of rb code cleaner by splitting input by ";".
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode see Rev code input and output in the viewing pane
#'
#'@export
#'

doRev <- function(input, viewCode = FALSE){
  allOutputs <- list()

  chunks <- unlist(stringr::str_split(input, ";"))

  revEnv$allCode <- unlist(append(revEnv$allCode, chunks))

  for(i in 1:length(chunks))
    if(i == length(chunks))
      allOutputs <- append(allOutputs, cleanCallRev(chunks[i], viewCode = viewCode))
    else allOutputs <- append(allOutputs, cleanCallRev(chunks[i]))

  if(length(allOutputs == 1))
     allOutputs <- unlist(allOutputs)

  return(allOutputs)
}










