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
  else{
    sign <- stringr::str_extract_all(RevOut, "=|:=|<-|~")[[1]]

    if(stringr::str_detect(RevOut, ":=")){
      RevEnv$Deterministic <- c(RevEnv$Deterministic, RevOut)}
  }


  objdef <- stringr::str_squish(unlist(stringr::str_split(RevOut, sign)))

  output <- CallRev(objdef[2], viewCode = viewCode)
  #Special coercion cases
  #

  if(any(stringr::str_detect(output, "charactermatrixwith"))){
    output <- CallRev(objdef[2] %+% '.show()', coerce = F)

    output <- output[-c(1:2)]
    output <- data.frame(taxa = output[which(c(1:length(output)) %% 2 == 1)],
                         data = output[which(c(1:length(output)) %% 2 == 0)])

    output$data <- data.frame(stringr::str_split(test$data, " "))

  }

  #

  if(stringr::str_detect(RevOut, "=|~|<-")){
    makeActiveBinding(objdef[1], function() output, RevEnv)}

  else if (stringr::str_detect(RevOut, ":=")){
    makeActiveBinding(objdef[1], function(){
      list <- RevEnv$Vars
      CallRev(list, objdef[2], env = RevEnv)
    }
    )
  }


  RevEnv$Vars <- append(RevEnv$Vars,  values = RevOut)

  objdefClass <- class(get(objdef[1], envir = RevEnv))

  if(objdefClass == "NULL"){
    message(output)
    RevEnv$Vars <- utils::head(RevEnv$Vars, length(RevEnv$Vars)-1)
    rm(list = c(objdef[1]), envir = RevEnv)
    return()
  }

  if(!hideMessage){
    message(stringr::str_c(stringr::str_to_title(objdefClass), " object '", objdef[1], "' has been created in RevEnv!"))
  }


  RevEnv$Vars <- unique(RevEnv$Vars)

}

