
#' Knitr engine for RevBayes
#'
#' Rev code can be ran directly in knitr chunks, without the use of the functions doRev() or repRev(). History is accessed with the .Revhistory file and persists between chunks.
#'
#'
#'@export
knitRev <- function(){
  knitr::knit_engines$set(rb = function(options) {

    output <- doRev(options$code)

    return(knitr::engine_output(options, code = options$code, out = c(output)))
  })
}
