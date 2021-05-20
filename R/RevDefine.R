#' Define Rev objects into revEnv when calling rb.exe
#'
#' A wrapper for callRev() used when objects are defined in RevBayes. Identifies if
#'     an object is being defined, and if it is it will create an object of the same
#'     name in revEnv. Because the object is being created in revEnv, InitRev() must be called
#'     first.
#'
#' @param RevOut String input that will be sent to rb.exe. If the function detects an
#'     an assignment operator, a similar R object to the one being created in the rb session
#'     will be created in the revEnv.
#' @param viewCode If TRUE, the code written in the temporary file used to interact with
#'     rb.exe will be displayed in the viewing pane. This may be useful for diagnosing errors.
#' @param hideMessage If FALSE, when revDefine() creates an object in the revEnv a message
#'     will be displayed. If TRUE, the message will be hidden.
#'
#' @examples
#' revDefine("example1 <- posteriorPredictiveProbability(v(2), 3)", viewCode = TRUE)
#'
#' @export
revDefine <- function(RevOut, viewCode = FALSE, hideMessage = FALSE, knit = FALSE){

  hasDefsInBrackets <- function(input){
    input = unlist(stringr::str_split(input, ""))

    inputLength = length(input)-1;
    for(i in 1:inputLength){
      if(input[i] == "=" & input[i+1] == "="){
        inputLength = inputLength + 1
        input[i] = "=="
        input = input[-c(i+1)]
      }
    }

    inputLength = length(input)-1;
    for(i in 1:inputLength){
      if(input[i] == ":" & input[i+1] == "="){
        inputLength = inputLength + 1
        input[i] = ":="
        input = input[-c(i+1)]
      }
    }
    inputLength = length(input)-1;
    for(i in 1:inputLength){
      if(input[i] == "<" & input[i+1] == "-"){
        inputLength = inputLength + 1
        input[i] = "<-"
        input = input[-c(i+1)]
      }
    }



    vals = 0
    hasBracketedDefs = FALSE

    for(i in input){
      if(i == "{")
        vals = vals + 1
      else if(i == "}")
        vals = vals - 1
      else if(i == "=" || i == "<-" || i == ":=" || i == "~" ){
        if(vals > 0){
          hasBracketedDefs = TRUE
          break;
        }
      }
    }

    return(hasBracketedDefs)
  }

  if(!stringr::str_detect(RevOut, "=|:=|<-|~") || (hasDefsInBrackets(RevOut))){
    return()
  }
  else{
    sign <- stringr::str_extract_all(RevOut, "=|:=|<-|~")[[1]]

    if(stringr::str_detect(RevOut, ":=")){
      revEnv$Deterministic <- c(revEnv$Deterministic, RevOut)}
  }


  objdef <- stringr::str_squish(unlist(stringr::str_split(RevOut, sign)))

  output <- callRev(objdef[2], viewCode = viewCode)
  #Special coercion cases
  #

  if(any(stringr::str_detect(output, "charactermatrixwith"))){
    output <- callRev(objdef[2] %+% '.show()', coerce = F)

    output <- output[-c(1:2)]
    output <- data.frame(taxa = output[which(c(1:length(output)) %% 2 == 1)],
                         data = output[which(c(1:length(output)) %% 2 == 0)])

    output$data <- data.frame(stringr::str_split(test$data, " "))

  }

  #

  if(stringr::str_detect(RevOut, "=|~|<-")){
    makeActiveBinding(objdef[1], function() output, revEnv)}

  else if (stringr::str_detect(RevOut, ":=")){
    makeActiveBinding(objdef[1], function(){
      list <- revEnv$Vars
      callRev(list, objdef[2], env = revEnv)
    }
    )
  }


  revEnv$Vars <- append(revEnv$Vars,  values = RevOut)

  objdefClass <- class(get(objdef[1], envir = revEnv))

  if(objdefClass == "NULL"){
    message(output)
    revEnv$Vars <- utils::head(revEnv$Vars, length(revEnv$Vars)-1)
    rm(list = c(objdef[1]), envir = revEnv)
    return()
  }

  if(!hideMessage){
    message(stringr::str_c(stringr::str_to_title(objdefClass), " object '", objdef[1], "' has been created in revEnv!"))
  }


  revEnv$Vars <- unique(revEnv$Vars)

}
