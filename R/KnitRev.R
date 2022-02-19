
#' Knitr engine for RevBayes
#'
#' Rev code can be ran directly in knitr chunks, without the use of the functions doRev() or repRev(). History is accessed with the .Revhistory file and persists between chunks.
#'
#' @return No return. Initiates knitr engine for RevBayes.
#'
#'@export
knitRev <- function(){

  knitr::knit_engines$set(rb = function(options) {

    if(!any(names(options) == "rb_eval")){
      options$rb_eval <- TRUE
    }
    if(!any(names(options) == "coerce")){
      options$coerce <- TRUE
    }

    code <- paste0(options$code, collapse = "\n")

    output <- capture.output(doRev(code, evaluate = options$rb_eval, coerce = options$coerce))

    return(knitr::engine_output(options, code = options$code, out = noquote(output)))
  })

}
