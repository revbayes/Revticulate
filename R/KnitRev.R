
#' Knitr engine for RevBayes
#'
#'Rev code is ran directly in knitr chunks, and using the wrapper functions isn't necessary.
#'Any created variables will be put in RevEnv, and defined variables can be used across multiple
#'chunks.
#'
#'
#'@export
knitRev <- function(){
  knitr::knit_engines$set(rb = function(options) {

    output <- cleanCallRev(options$code)

    return(knitr::engine_output(options, code = options$code, out = c(output)))
  })
}
