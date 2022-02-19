#'Save Revticulate history in a .Rev file for use in RevBayes
#'
#'
#'@param filepath File to save .Revhistory to
#'
#'@examples
#' \dontrun{
#
#' }
#'
#'
#'@export
#'
saveRev <- function(filepath, use_wd=TRUE, use_quit=FALSE){
  if(!file.exists(filepath)){
    file.create(filepath)
  }

  text <- getRevHistory()
  if(use_wd){
    wd <- normalizePath(getwd(), winslash = "/")
    if(Sys.info()['sysname'] == 'Windows'){
      wd <- str_replace_all(wd, "/", "//")
    }
    text <- unlist(c(paste0('setwd("', wd ,'")\n'), text))
  }
  if(use_quit){
    text <- unlist(c(text, "\nq()"))
  }

  cat(text, file = filepath, sep = "\n")
}
