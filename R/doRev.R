#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode see Rev code input and output in the viewing pane
#'
#'@export
#'

doRev <- function(input, viewCode = FALSE, coerce = FALSE){

  first <- callRev(getRevHistory(), coerce = F)
  revEnv$allCode <- c(revEnv$allCode, input)
  last <- callRev(getRevHistory(), coerce = F, viewCode = viewCode)

    if(length(first) != 0)
       now <- last[-c(1:length(first))]
    else now <- last

  if(length(now) == 0)
     now = ""

  if (any(stringr::str_detect(now, pattern = "Error:|error|Missing Variable:"))) {
    revEnv$allCode <- revEnv$allCode[-c(length(revEnv$allCode))]
    if(coerce){
      message(now)
      return("")
      }
  }

  if(stringr::str_detect(now, "<-|=|:=|~")){
      brDefs <- function(input, open = "{", close = "}"){
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
      if(i == open)
        vals = vals + 1
      else if(i == close)
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
      if(!brDefs(now))
        revEnv$vars <- c(revEnv$vars, now)
  }

  if(coerce)
    return(coerceRev(now))

  return(now)
}
