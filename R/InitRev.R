#'Create an environment for interacting with RevBayes
#'
#'Creates an environment for interecting with rb.exe. This environment contains the variable
#'    RevPath for storing the path to rb.exe. If no path is provided, it will default to trying
#'    to use a path stored in RevR/RevPath.txt in .libPaths(). This means the user only has
#'    to provide the path to rb.exe once, and can change the path at any time by applying it again
#'    in InitRev().
#'
#'@param path String path to rb.exe
#'
#'@examples
#'RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"
#'InitRev(RevPath)
#'
#'@export
InitRev <- function(path = NULL){
  RevEnv <<- new.env(parent = globalenv())

    if(!is.null(path)){
      RevEnv$RevPath <- path
      write(path, list.files(.libPaths(), "RevR", full.names = TRUE) %+% "/RevPath.txt")
    }
    else{
      RevEnv$RevPath <-  readLines(list.files(.libPaths(), "RevR", full.names = TRUE) %+% "/RevPath.txt")
    }

    RevEnv$Vars <- c()
    RevEnv$Deterministic <- c()
    RevEnv$temps <- c()
    RevEnv$allCode <- c()
}

