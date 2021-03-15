#' Define Rev objects into RevEnv when calling rb.exe
#'
#' A wrapper for CallRev() used when objects are defined in RevBayes. Identifies if
#'     an object is being defined, and if it is it will create an object of the same
#'     name in RevEnv. Because the object is being created in RevEnv, InitRev() must be called
#'     first.
#'
#' @param RevOut String input that will be sent to rb.exe. If the function detects an
#'     an assignment operator, a similar R object to the one being created in the rb session
#'     will be created in the RevEnv.
#' @param viewCode If TRUE, the code written in the temporary file used to interact with
#'     rb.exe will be displayed in the viewing pane. This may be useful for diagnosing errors.
#' @param hideMessage If FALSE, when RevDefine() creates an object in the RevEnv a message
#'     will be displayed. If TRUE, the message will be hidden.
#'
#' @examples
#' RevDefine("example1 <- posteriorPredictiveProbability(v(2), 3)", viewCode = TRUE)
#'
#' @export
RevDefine <- function(RevOut, viewCode = FALSE, hideMessage = FALSE){

  if(!stringr::str_detect(RevOut, "=|:=|<-|~")){return()}

  if(stringr::str_detect(RevOut, "=|:=|<-|~")){
    sign <- stringr::str_extract_all(RevOut, "=|:=|<-|~")[[1]]

    if(stringr::str_detect(RevOut, ":=")){
      RevEnv$Deterministic <- c(RevEnv$Deterministic, RevOut)}
      ###Keep eye on this
      RevEnv$Deterministic <- unique(RevEnv$Deterministic)
  }



  objdef <- unlist(stringr::str_split(RevOut, sign))

  objdef<- stringr::str_squish(objdef)

  output <- CallRev(objdef[2], viewCode = viewCode)

  if(stringr::str_detect(RevOut, "=|~|<-")){
    makeActiveBinding(objdef[1], function() output, RevEnv)}

  else if (stringr::str_detect(RevOut, ":=")){makeActiveBinding(objdef[1], function(){list <- RevEnv$Vars
  CallRev(list, objdef[2], env = RevEnv)})
  }


  RevEnv$Vars <- append(RevEnv$Vars,  values = RevOut)

  objdefClass <- class(get(objdef[1], envir = RevEnv))

  if(objdefClass == "NULL"){
    message(output)
    RevEnv$Vars <- utils::head(RevEnv$Vars, length(RevEnv$Vars)-1)
    rm(list = c(objdef[1]), envir = RevEnv)
    return()
  }

  if(hideMessage != TRUE){
    message(stringr::str_c(stringr::str_to_title(objdefClass), " object '", objdef[1], "' has been created in RevEnv!"))
  }


  RevEnv$Vars <- unique(RevEnv$Vars)
}
