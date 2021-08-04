
#'Initializes external variables for interaction with RevBayes
#'
#'Creates external variables for storing the paths to the RevBayes executable and .Revhistory files, as well as initiating a folder to store temp files for RevBayes interactions.
#'
#'@param searchPath Full path or directory to search for the RevBayes executable. Default is the user's root directory (~).
#'
#'
#'@examples
#' \dontrun{
#'RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"
#'initRev(RevPath)
#'}
#'@export
initRev <- function(searchPath = "~"){

  path <- findRev(searchPath)

  if(file.exists(path)){
    Sys.setenv("RevBayesPath" = path[1])
  }
  else{
    stop("RevBayes executable not found!")
  }

  Sys.setenv("RevHistory" = (list.files(.libPaths(), "Revticulate", full.names = TRUE) %+% "/.Revhistory")[1])

  Sys.setenv("RevTemps" = (list.files(.libPaths(), "Revticulate", full.names = TRUE) %+% "/temps"))

  if(!file.exists(Sys.getenv("RevHistory")))
    file.create(Sys.getenv("RevHistory"), showWarnings = F)

  if(!dir.exists(Sys.getenv("RevTemps")))
    dir.create(Sys.getenv("RevTemps"), showWarnings = F)


}
