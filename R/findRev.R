#' Searches for the RevBayes executable
#'
#' Searches the users machine for the RevBayes executable and returns a vector of possible paths
#'
#'
#' @examples
#' \dontrun{
#' findRev()
#'}
#'
#' @export
#'
#'

findRev <- function() {
  rbPAths <-  grep(list.files("~", recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T)
  return(rbPAths)
}
