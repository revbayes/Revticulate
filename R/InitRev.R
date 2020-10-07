#'Inititates an enironment to store Rev objects and sets path to rb
#'
#'
#'
#' @param path String path to rb.
#'
#' @export
InitRev <- function(path){
  revenv <<- new.env(parent = globalenv())
  revenv$RevPath <- path
}
