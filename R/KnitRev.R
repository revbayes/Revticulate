
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
    output <- stringr::str_c(RevR::doRev(code, coerce = FALSE, knit = TRUE), sep = " ")

    if (options$eval)
      return(stringr::str_c(output, collapse = " \n"))
    else code

  })
}
