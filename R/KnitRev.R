
#' Knitr engine for RevBayes
#'
#' Create a knitr engine to evaluate RevBayes code. To use RevBayes in a knitr chunk, provide the header 'rb'.
#' Besides the standard knitr chunk options, the rb engine provides two extra options: rb_eval, and coerce.
#'
#' If rb_eval=FALSE, the code in the chunk will not yet be ran in RevBayes, but will saved in the .Revhistory file. This
#' option is useful for code chunks containing the final loop of an mcmc, which would should be saved to an external file
#' with saveRev() and ran in a terminal with callRevFromTerminal().
#'
#' If coerce=TRUE, coerceRev() will attempt to convert RevBayes output into equivalent R formatted objects. The default value
#' for both coerce and rb_eval is TRUE.
#'
#' History is accessed with the .Revhistory file and persists between chunks.
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
