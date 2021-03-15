#' knitr engine for revbayes
#'
#' @param options argument required for knitr engines. options$code coerces the text
#' in the code chunk to a string, which is then evaluated with doRev()
#'
#'@export
KnitRev <- function(){
        knitr::knit_engines$set(rb = function(options) {
        code <- options$code
        if (options$eval)
          RevR::doRev(code, coerce = F) else code
         })
        if(!options$eval)
          return("")
}
