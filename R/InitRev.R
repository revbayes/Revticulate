
#'Create an environment for interacting with RevBayes
#'
#'Creates an environment for interecting with rb.exe. This environment contains the variable
#'    RevPath for storing the path to rb.exe. If no path is provided, it will default to trying
#'    to use a path stored in Revticulate/RevPath.txt in .libPaths(). This means the user only has
#'    to provide the path to rb.exe once, and can change the path at any time by applying it again
#'    in initRev().
#'
#'@param searchPath Full path or directtory to search for the RevBayes executable.
#'
#'
#'@examples
#' \dontrun{
#'RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"
#'initRev(RevPath)
#'}
#'@export
initRev <- function(searchPath = "~"){

  path <- findRev(searchPath)[1]

  if(file.exists(path) == TRUE){
    Sys.setenv("RevBayesPath" = path)
  }
  else{
    stop("RevBayes executable not found!")
  }

  Sys.setenv("RevHistory" = (list.files(.libPaths(), "Revticulate", full.names = TRUE) %+% "/.Revhistory")[1])

  Sys.setenv("RevTemps" = (list.files(.libPaths(), "Revticulate", full.names = TRUE) %+% "/temps"))

  if(!dir.exists(Sys.getenv("RevTemps")))
    dir.create(Sys.getenv("RevTemps"), showWarnings = F)
}
