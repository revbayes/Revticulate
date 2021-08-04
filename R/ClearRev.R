
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
#'@export
clearRev <- function(n = NULL){

  undoRev <- function(n){

   if(n > length(getRevHistory()))
      return(message("Cannot remove more items than exist in Rev History!"))


   if (!is.null(n)){
      file = getRevHistory()
      remove = length(file)-n
      file <- file[1:remove]
      cat(file, file = Sys.getenv("RevHistory"), sep = "\n" ,append = F)
    }

    message("Removed " %+% n %+% " item(s) from Rev History!")

    return(cat("Current History: ", getRevHistory(), sep = "\n"))
  }

  if(!is.null(n)){
    return(undoRev(n))
  }

  cat("", file = Sys.getenv("RevHistory"), append = F)

  message("Successfully reset Rev History!")

}
