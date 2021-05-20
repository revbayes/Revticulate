
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

    if(length(revEnv$allCode) == 0){
      revEnv$allCode <- c(revEnv$allCode, options$code)
      output <- Revticulate::callRev(revEnv$allCode, coerce = FALSE, knit = TRUE)
    }
    else{
      lastOutput <- Revticulate::callRev(revEnv$allCode, coerce = FALSE, knit = TRUE)

      revEnv$allCode <- c(revEnv$allCode, options$code)

      nextOutput <- Revticulate::callRev(revEnv$allCode, coerce = FALSE, knit = TRUE)

      output <- nextOutput[-c(1:length(lastOutput))]
    }
    code <- options$code

    return(knitr::engine_output(options, code = options$code, out = c(output)))
  })
}
