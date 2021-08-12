
#'Initializes external variables for interaction with RevBayes
#'
#'Creates external variables for storing the paths to the RevBayes executable and .Revhistory files, as well as initiating a folder to store temp files for RevBayes interactions.
#'
#'@param searchPath Full path or directory to search for the RevBayes executable. Default is the user's root directory (~).
#'
#'@param infoDir Path to parent directory of the RevInfo folder used for managing RevBayes interactions. Default is the parent directory of the user's working directory. If a RevInfo folder already exists in this directory, history stored in the existing folder will be used. Else, a new folder will be created.
#'
#'@examples
#' \dontrun{
#'RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"
#'initRev(RevPath)
#'}
#'
#'@return No return. Initiates external environmental variables and directory for mediating interaction between R and RevBayes.
#'
#'@export
initRev <- function(searchPath = "~", infoDir = dirname(getwd())){

  path <- findRev(searchPath)

  if(file.exists(path)){
    Sys.setenv("RevBayesPath" = path[1])
  }
  else{
    stop("RevBayes executable not found!")
  }

  Sys.setenv("RevHistory" = (infoDir %+% "/RevInfo/.Revhistory"))

  Sys.setenv("RevTemps" = (infoDir %+% "/RevInfo/temps"))

  if(!dir.exists(infoDir %+% "/RevInfo"))
    dir.create(infoDir %+% "/RevInfo", showWarnings = FALSE)

  if(!file.exists(Sys.getenv("RevHistory")))
    file.create(Sys.getenv("RevHistory"), showWarnings = FALSE)

  if(!dir.exists(Sys.getenv("RevTemps")))
    dir.create(Sys.getenv("RevTemps"), showWarnings = FALSE)


}
