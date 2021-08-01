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

  if(parentDirectory == "~"){
    PATH <- unlist(str_split(Sys.getenv("PATH"), pattern = ";"))
    rootRB <- PATH[str_ends(PATH, "(//|/|\\\\)rb|rb.exe")]

    if(length(rootRB != 0))
       return(rootRB)


  possibleDrives <- paste0(LETTERS[-c(3)], "://")
  drives <- unlist(possibleDrives[dir.exists(possibleDrives)])

  getPaths <- to_vec(for(i in drives) grep(list.files(i, recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T))

  }

  else{
    getPaths <- grep(list.files(parentDirectory, recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T)
  }


  return(getPaths)
}
