#'Show variables that have been defined in RevBayes
#'
#'Wrapper for RevBayes ls() function. Displays variables that have been defined in the working RevBayes session.
#'
#' @examples
#' \dontrun{
#' getRevVars()
#' }
#'
#' @return No return - wrapper for RevBayes ls() function to show variables in rb environment
#'
#'
#'@export
#'
getRevVars <- function(){
    vars <- doRev("ls()")
    clearRev(1, silent = TRUE)
    cat(vars, sep = "\n")
}
