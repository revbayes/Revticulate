
#'Clear Code from Revticulate History
#'
#'Removes code from .Revhistory file used for managing Revticulate history.
#'Clears all code by default, or last 'n' lines of code specifed by the user.
#'
#'@param n integer - How many lines to remove. If n = NULL, all lines are removed.
#'
#'@param silent logical - If TRUE, output messages will be silenced. Default is FALSE.
#'
#'@examples
#' \dontrun{
#' clearRev() #Clear all Revticulate history
#' clearRev(n = 3) #Remove the last 3 lines of Revticulate history
#' }
#'
#' @return pseudoError: NULL. Message warning user that they attempted to erase more items from the Rev history than exist. message() is used instead of stop() so that clearRev() functions in repRev().
#'
#' @return undoRev(n): NULL. Removes n number of lines from .Revhistory and cats the remaining history to the screen.
#'
#'@export
clearRev <- function(n = NULL, silent=FALSE){

  undoRev <- function(n){

   if(n > length(getRevHistory())){
      pseudoError <- message("Cannot remove more items than exist in Rev History!")
      return(pseudoError)
   }

   if (!is.null(n)){
      file = getRevHistory()
      remove = length(file)-n
      file <- file[1:remove]
      cat(file, file = Sys.getenv("revHistory"), sep = "\n", append = FALSE)
    }
    if(!silent)
      message("Removed " %+% n %+% " item(s) from Rev History!")
  }

  if(!is.null(n)){
    return(undoRev(n))
  }
  else{
    message("Successfully reset Rev History!")
    cat("#START\n", file = Sys.getenv("revHistory"), append = FALSE)
  }
}
