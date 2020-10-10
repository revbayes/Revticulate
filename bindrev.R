


`%+%` <- function(a, b) stringr::str_c(a, b, sep = "")

RevDefine <- function(RevOut, viewCode = FALSE){
  
  if(!stringr::str_detect(RevOut, "=|:=|<-|~")){return()}
  
  if(stringr::str_detect(RevOut, "=|:=|<-|~")){
  sign <- stringr::str_extract_all(RevOut, "=|:=|<-|~")[[1]]}
  
  objdef <- unlist(stringr::str_split(RevOut, sign))
  
  objdef<- stringr::str_squish(objdef)
  
  output <- CallRev(objdef[2], viewCode = viewCode)
  
  #else{
  #if(!any(stringr::str_detect(output, pattern = "Error:|error"))){
  makeActiveBinding(objdef[1], function() output, RevEnv)
  RevEnv$Vars <- append(RevEnv$Vars,  values = RevOut)
  
  objdefClass <- class(get(objdef[1], envir = RevEnv))
  
  if(objdefClass == "NULL"){
    message(c("Warning: ", output))
    RevEnv$Vars <- head(RevEnv$Vars, length(RevEnv$Vars)-1)
    rm(list = c(objdef[1]), envir = RevEnv)
    return()
  }
  
  
  message(stringr::str_c(stringr::str_to_title(objdefClass), " object '", objdef[1], "' has been created in RevEnv!"))
  #}}
 
  
   RevEnv$Vars <- unique(RevEnv$Vars)
}



doRev <- function(RevOut, viewCode = FALSE, coerce = TRUE, interactive = FALSE){
  
  if(stringr::str_detect(RevOut, " = | := | <- | ~ ")){
      RevDefine(RevOut, viewCode = viewCode)}
  
  else{
  out <- CallRev(RevOut, coerce = coerce, path = RevEnv$RevPath, viewCode = viewCode)
    if(interactive == TRUE){
      return(print(out))}
    if(interactive == FALSE){
      return(out)}
  }
}







