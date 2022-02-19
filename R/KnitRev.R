
#' Knitr Engine for RevBayes
#'
#' Creates a knitr engine for evaluating RevBayes code
#'
#' To use RevBayes in a knitr document, type 'library(Revticulate)' followed by 'knitRev()'
#' on the next line in the initialization block. The language header for RevBayes chunks is 'rb'.
#'
#' The RevBayes knitr engine is built around doRev(), allowing history to persist between chunks.
#' If a variable 'x <- 10' is defined in one RevBayes chunk, 'x' can then be accessed in a later RevBayes chunk.
#' Additionally, it could be accessed via doRev() in an R chunk, allowing for quick R analysis of RevBayes variables.
#'
#' On top of the standard knitr chunk options, the rb engine provides two extras: rb_eval, and coerce.
#'
#' If rb_eval = FALSE, the code in the chunk will be saved to the .Revhistory file, but will not be submitted to RevBayes. This
#' option is useful for code chunks containing the final loop of an mcmc, which could be saved to an external file
#' with saveRev() and ran in a terminal with callRevFromTerminal().
#'
#' If coerce = TRUE, coerceRev() will attempt to convert RevBayes output into equivalent R formatted objects. The default value
#' for both coerce and rb_eval is TRUE.
#'
#'@examples
#'\dontrun{
#' ```{r setup, include=FALSE}
#'      knitr::opts_chunk$set(echo = TRUE)
#'      library(Revticulate)
#'      knitRev()
#' ```
#'
#' ```{rb rb_eval=TRUE, coerce=FALSE}
#'
#' x <- simTree(32)
#'
#' ```
#'
#' ```{r}
#'
#' x <- doRev('x')
#' plot(x)
#'
#' ```
#'}
#'
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
