
#'Removes lines from RevBayes history
#'
#'Removes lines of code from the .Revhistory file used for managing RevBayes interactions.
#'
#'@param n How many lines to remove. If n = NULL, all lines are removed.
#'
#'@examples
#' \dontrun{
#' clearRev() #Clear all objects from RevBayes
#' clearRev(n = 1) # Clear the last line input to RevBayes
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
