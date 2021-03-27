#' Submit input to rb.exe and return output
#'
#' Submits input to rb.exe and returns output to R in string format. If coerce = T, the function
#'    will try to coerce output to a similar R object.
#'
#' @param ... String input to send to RevBayes.
#' @param coerce If TRUE, attempts to coerce output to an R object. If FALSE, output
#'     will remain in String format. Default is TRUE.
#' @param path Path to rb.exe. Default is RevEnv$RevPath, which is created with InitRev().
#' @param viewCode If TRUE, the input input and output in the temporary file used to interact
#'     with rb.exe will be displayed in the viewing pane. This option may be useful for
#'     diagnosing errors.
#' @param use_wd If TRUE, sets the working directory in the temporary RevBayes session
#'     to the working directory of the active R session.
#'
#' @return out Output from RevBayes. If coerce = FALSE, out will be in String format.
#'     If coerce = TRUE, the function will attempt to coerce the String to an R object.
#'
#' @examples
#' CallRev("2^3")
#' CallRev("2^3", coerce = FALSE, viewcode = T)
#'
#'@import ape
#'@import utils
#'@import stringr
#'
#' @export
#'

CallRev <- function(..., coerce = TRUE, path = RevEnv$RevPath, viewCode = F, use_wd = T){

  argu <- c(...)
  argu <- c(RevEnv$Vars, argu)

  if(use_wd == T){
    wd <- stringr::str_replace_all(normalizePath(getwd()), pattern = "\\\\", "//")
    argu <- c('setwd("' %+% wd %+% '")', argu)
  }

  tf <- tempfile(pattern = "file", tmpdir = paste(getwd(), "/", sep = ""), fileext = ".rev")
  tf <- gsub(pattern = "\\\\", "//", tf)

  fopen <- file(tf)

  ret <- unlist(argu)

  writeLines(ret, fopen, sep = "\n")

  out <- system2(path, args = c(tf), stdout = T, timeout = 10)
  out <- out[-c(1:13, length(out)-1, length(out))]


  cat("Input:\n -->  " %+% ret %+% "\n//", file = tf, sep = "\n", append = F)
  cat("Output:\n -->  " %+% out %+% "\n//", file = tf, sep = "\n", append = T)

  if(viewCode == T){
    viewOut <- stringr::str_view_all(readLines(tf), pattern = "Error|error|Input:|Output:")
    utils::capture.output(viewOut)}

  close(fopen)

  RevEnv$temps <- c(RevEnv$temps, tf)
  for(i in RevEnv$temps){
    unlink(i)
  }

  if(any(stringr::str_detect(out, pattern = "Error:|error|Missing Variable:"))){
    message(out)
    return()
  }


  if(coerce == FALSE){
    return(out)
  }

  ###
  out <- stringr::str_c(out, collapse = "\n")
  out <- CoerceRev(out)


  #make sure tf is gone

  unlink(tf)

  return(out)
}
