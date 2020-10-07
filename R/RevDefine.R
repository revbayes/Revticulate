#'Defines rev objects and returns them to revenv.
#'@param RevOut Rev code string defining object
#'@param viewCode If TRUE, rev code and output will be shown in stringr::str_view widget
#'
#'@examples RevDefine('var1 <- posteriorPredictiveProbability(v(2), 3)', viewCode = TRUE)
#'
#'@export
RevDefine <- function(RevOut, viewCode = FALSE){

  if(stringr::str_detect(RevOut, "=|:=|<-|~")){
    sign <- stringr::str_extract_all(RevOut, "=|:=|<-|~")[[1]]}

  objdef <- unlist(stringr::str_split(RevOut, sign))

  objdef<- stringr::str_squish(objdef)

  output <- RevR::CallRev(objdef[2], viewCode = viewCode)

  output.str <- RevR::CallRev(objdef[2], coerce = F, viewCode = F)

  output.str <- stringr::str_squish(output.str[which(stringr::str_count(stringr::str_squish(output.str), "") > 0)])

  makeActiveBinding(objdef[1], function() output, revenv)
  makeActiveBinding(objdef[1] %+% ".str", function() output.str, revenv)

}
