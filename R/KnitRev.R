#' Knitr engine for RevBayes
#'
#'Rev code is ran directly in knitr chunks, and using the wrapper functions isn't necessary.
#'Any created variables will be put in RevEnv, and defined variables can be used across multiple
#'chunks.
#'
#'
#'@export
KnitRev <- function(){
  knitr::knit_engines$set(rb = function(options) {
    code <- options$code
    output <- RevR::CallRev(code, coerce = FALSE, knit = TRUE)
    return(knitr::engine_output(options, code = options$code, out = output))
  })
}
