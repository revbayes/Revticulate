#'Clears all objects from revenv besides RevPath
#'@export
ClearRev <- function(){
  remove(list = ls(envir = revenv)[which(stringr::str_detect(ls(envir = revenv),
                                                             "RevPath", negate = TRUE))], envir = revenv)
}
