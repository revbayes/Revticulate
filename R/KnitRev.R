#' Knitr engine for RevBayes
#'
#'Rev code is ran directly in knitr chunks, and using the wrapper functions isn't necessary.
#'Any created variables will be put in RevEnv, and defined variables can be used across multiple
#'chunks.
#'
#'
#'
#'@export
KnitRev <- function(){
  knitr::knit_engines$set(rb = function(options) {

    lastOutput <- RevR::CallRev(RevEnv$allCode, coerce = FALSE, knit = TRUE)

    RevEnv$allCode <- c(RevEnv$allCode, options$code)

    nextOutput <- RevR::CallRev(RevEnv$allCode, coerce = FALSE, knit = TRUE)

    output <- nextOutput[-c(1:length(lastOutput))]

    code <- options$code

    return(knitr::engine_output(options, code = options$code, out = output))
  })
}
