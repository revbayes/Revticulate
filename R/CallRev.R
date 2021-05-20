#' Submit input to rb.exe and return output
#'
#' Submits input to rb.exe and returns output to R in string format. If coerce = T, the function
#'    will try to coerce output to a similar R object.
#'
#' @param ... String input to send to RevBayes.
#' @param coerce If TRUE, attempts to coerce output to an R object. If FALSE, output
#'     will remain in String format. Default is TRUE.
#' @param path Path to rb.exe. Default is revEnv$RevPath, which is created with InitRev().
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
#' callRev("2^3")
#' callRev("2^3", coerce = FALSE, viewcode = T)
#'
#'@import ape
#'@import utils
#'@import stringr
#'
#' @export
#'

callRev <- function (..., coerce = TRUE, path = revEnv$RevPath, viewCode = F,
                     use_wd = T, knit = F){
  argu <- c(...)
  if (knit) {
    clumpBrackets <- function(stringVector) {
      openBraces <- stringr::str_count(stringVector, "\\{")
      closedBraces <- stringr::str_count(stringVector,
                                         "\\}")
      if (all(openBraces == 0)) {
        return(stringVector)
      }
      allBraces <- openBraces - closedBraces
      startsStops <- which(allBraces != 0)
      startStopVals <- allBraces[startsStops]
      stopIndexes <- c()
      net <- startStopVals[1]
      for (i in 2:length(startStopVals)) {
        net <- net + startStopVals[i]
        if (net == 0) {
          stopIndexes <- c(stopIndexes, i)
        }
      }
      finalStopVals <- startsStops[stopIndexes]
      finalStartVals <- c(startsStops[1], finalStopVals +
                            1)
      finalStartVals <- finalStartVals[-c(length(finalStartVals))]
      finalStartStopVals <- list(finalStartVals, finalStopVals)
      finalVector <- c(stringVector[which(1:length(stringVector) <
                                            startsStops[1])])
      for (i in 1:length(finalStartStopVals[[1]])) {
        start <- finalStartStopVals[[1]][i]
        stop <- finalStartStopVals[[2]][i]
        finalVector <- c(finalVector, stringr::str_c(stringVector[start:stop],
                                                     collapse = "\n"))
      }
      highestStopValue <- finalStopVals[length(finalStopVals)]
      if (highestStopValue < length(stringVector)) {
        finalVector <- c(finalVector, stringVector[c((highestStopValue +
                                                        1):length(stringVector))])
      }
      finalVector <- stringr::str_c("\n", finalVector,
                                    sep = "")
      return(finalVector)
    }
    if (stringr::str_c(..., collapse = "") == "") {
      return("")
    }
    argu <- clumpBrackets(c(...))
    argu <- stringr::str_squish(argu)
    argu <- argu[which(argu != "")]
    revEnv$Vars <- c(revEnv$Vars, argu[stringr::str_which(argu,
                                                          " = |:=|<-|~")])
    copy <- c(revEnv$Vars, "")
    copyTwo <- c("", revEnv$Vars)
    revEnv$Vars <- copy[which(copy != copyTwo)]
  }
  argu <- c(revEnv$Vars, argu)
  if (use_wd == T) {
    wd <- stringr::str_replace_all(normalizePath(getwd()),
                                   pattern = "\\\\", "//")
    argu <- c("setwd(\"" %+% wd %+% "\")", argu)
  }
  tf <- tempfile(pattern = "file", tmpdir = paste(getwd(),
                                                  "/", sep = ""), fileext = ".rev")
  tf <- gsub(pattern = "\\\\", "//", tf)
  fopen <- file(tf)
  ret <- unlist(argu)
  writeLines(ret, fopen, sep = "\n")
  out <- system2(path, args = c(tf), stdout = T, timeout = 10)
  out <- out[-c(1:13, length(out) - 1, length(out))]
  cat("Input:\n -->  " %+% ret %+% "\n//", file = tf,
      sep = "\n", append = F)
  cat("Output:\n -->  " %+% out %+% "\n//", file = tf,
      sep = "\n", append = T)
  if (viewCode == T) {
    viewOut <- stringr::str_view_all(readLines(tf), pattern = "Error|error|Input:|Output:")
    utils::capture.output(viewOut)
  }
  close(fopen)
  revEnv$temps <- c(revEnv$temps, tf)
  for (i in revEnv$temps) {
    unlink(i)
  }
  if (any(stringr::str_detect(out, pattern = "Error:|error|Missing Variable:"))) {
    message(out)
    return()
  }
  if (coerce == FALSE) {
    return(out)
  }
  out <- stringr::str_c(out, collapse = "\n")
  if (coerce) {
    out <- coerceRev(out)
  }
  unlink(tf)
  return(out)
}
