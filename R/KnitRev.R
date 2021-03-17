#' Knitr engine for RevBayes
#'
#' @param options Argument required for Knitr engines. options$code coerces the text
#' in the code chunk to a string, which is then evaluated with doRev().
#'
#'@export
KnitRev <- function(){
        knitr::knit_engines$set(rb = function(options) {
        code <- options$code
        if (options$eval)
          RevR::doRev(code, coerce = FALSE) else code
         })
}
