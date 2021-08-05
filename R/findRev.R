#' Searches for the RevBayes executable
#'
#' Recursively searches the users machine for the RevBayes executable and returns a vector of possible paths
#'
#'
#' @param parentDirectory Parent directory to recursively search for the RevBayes executable. Default is the user's root directory ('~'). Because many sub-directories likely exist within the root directory, the search time will greatly decrease by providing a lower level directory path to the RevBayes executable.
#'
#' @examples
#' \dontrun{
#' findRev()
#'}
#' \dontrun{
#' findRev("C://Users/caleb/")
#'}
#'
#' @export
#'
#'
findRev <- function(parentDirectory = "~") {

  if(str_ends(parentDirectory, "rb|rb.exe"))
    if(!file.exists(parentDirectory))
      stop("No existing RevBayes version can be found in the path provided!")
    else
      return(parentDirectory)

  if(parentDirectory == "~"){
    if(Sys.info()["sysname"] == "Windows"){
    PATH <- unlist(str_split(Sys.getenv("PATH"), pattern = ";"))
    rootRB <- PATH[str_ends(PATH, "(//|/|\\\\)rb|rb.exe")]

    if(length(rootRB != 0))
       return(rootRB)
    }
    else{
      getPaths <- grep(list.files(parentDirectory, recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T)
      if(length(getPaths) > 0)
        return(getPaths)
    }

  possibleDrives <- paste0(LETTERS[-c(3)], "://")
  drives <- unlist(possibleDrives[dir.exists(possibleDrives)])

  getPaths <- to_vec(for(i in drives) grep(list.files(i, recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T))

  }

  else{
    getPaths <- grep(list.files(parentDirectory, recursive = T, full.names = T), pattern = "/rb$|/rb.exe$", value = T)
  }

  if(length(getPaths) == 0)
    stop("No existing RevBayes version can be found in the path provided!")

  return(getPaths)
}
