#'Create an environment for interacting with RevBayes
#'
#'
#'@param path String path to rb.exe
#'
#'@examples
#'RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"
#'InitRev(RevPath)
#'
#'@export
InitRev <- function(path){
  RevEnv <<- new.env(parent = globalenv())
  RevEnv$RevPath <- path
  RevEnv$Vars <- c()
  RevEnv$Deterministic <- c()
  RevEnv$temps <- c()
}

