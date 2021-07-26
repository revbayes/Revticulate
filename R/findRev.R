#' Searches for the RevBayes executable
#'
#' Recursively earches the users machine for the RevBayes executable and returns a vector of possible paths
#'
#'
#' @param parentDirectory Parent directory to recursively search for the rb executable. Default is the user's root directory ('~').
#'
#' @examples
#' \dontrun{
#' findRev()
#'}
#'
#' @export
#'
#'

findRev <- function(parentDirectory = "~") {
  rbPAths <-  grep(list.files(parentDirectory, recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T)
  return(rbPAths)
}
