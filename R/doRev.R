#'Wrapper for CallRev() and RevDefine()
#'
#'A wrapper for CallRev() and RevDefine(). If a variable is assigned in the temporary
#'    rb session, it will also be created in the RevEnv. Otherwise, functions like CallRev().
#'
#'@param ... String of code that will be sent to rb.exe
#'
#'@param viewCode If TRUE, code from the temporary file used to interact with rb.exe
#'    will be displayed in the viewing pane. This option may be useful for diagnosing errors.
#'    Default is FALSE.
#'
#'@param coerce If FALSE, output from rb.exe will remain in String format. If TRUE, doRev()
#'    will attempt to coerce output into an appropriate R object.
#'
#'@param interactive Ignore. Used to implement doRev() into RepRev().
#'
#'@param Det_Update If TRUE, previously created object in RevEnv that is redefined in rb.exe
#'    will be updated. If FALSE, it will not. Default is TRUE.
#'
#'@param use_wd If TRUE, the temporary rb.exe session will use the same working directory as
#'    the current R session. Else, it will use its default.
#'
#'@return outobjs Prints output from rb.exe, if required.
#'
#'@examples
#'doRev("sam <- 777")
#'doRev("sam + 2", viewCode = TRUE)
#'
#'@export
#'
doRev <- function(..., viewCode = FALSE, coerce = TRUE, interactive = FALSE, Det_Update = TRUE, use_wd = T){

  #group elements by curly braces
  clumpBrackets <- function(stringVector){

    #get opening and closing curly braces
    openBraces <- stringr::str_count(stringVector, "\\{")
    closedBraces <- stringr::str_count(stringVector, "\\}")

    if(all(openBraces == 0)){
      return(stringVector)
    }

    allBraces <- openBraces - closedBraces
    #get indices where there is a net change in the number of open or closed brackets and the net change at
    #these indices. (-) indicates closing curly braces.
    startsStops <- which(allBraces != 0)
    startStopVals <- allBraces[startsStops]

    #Get indices where outer curly braces close
    stopIndexes <- c()
    net <- startStopVals[1]
    for(i in 2:length(startStopVals)){
      net <- net + startStopVals[i]
      if(net == 0){
        stopIndexes <- c(stopIndexes, i)
      }
    }
    #Get clusters of start and stop values
    finalStopVals <- startsStops[stopIndexes]
    finalStartVals <- c(startsStops[1], finalStopVals + 1)
    finalStartVals <- finalStartVals[-c(length(finalStartVals))]
    finalStartStopVals <- list(finalStartVals, finalStopVals)

    #build final coerced vector
    finalVector <- c(stringVector[which(1:length(stringVector) < startsStops[1])])
    for(i in 1:length(finalStartStopVals[[1]])){
      start <- finalStartStopVals[[1]][i]
      stop <- finalStartStopVals[[2]][i]
      finalVector <- c(finalVector, stringr::str_c(stringVector[start:stop], collapse = "\n"))
    }
    highestStopValue <- finalStopVals[length(finalStopVals)]
    if(highestStopValue < length(stringVector)){
      finalVector <- c(finalVector, stringVector[c((highestStopValue + 1):length(stringVector))])
    }

    finalVector <- stringr::str_c("\n", finalVector, sep = "")

    return(finalVector)
  }



  RevOut <- clumpBrackets(c(...))

  outobjs <- list()

  for(i in 1:length(RevOut)){
    if(stringr::str_detect(RevOut[i], " = | := | <- | ~ ")){
      RevDefine(RevOut[i], viewCode = viewCode)

      if(length(RevEnv$Deterministic) != 0){
        if(Det_Update == TRUE){
          for(i in unique(RevEnv$Deterministic)){
            RevDefine(i, viewCode = F, hideMessage = TRUE)
          }
        }
      }

    }

    else{
      out <- CallRev(RevOut[i], coerce = coerce, path = RevEnv$RevPath, viewCode = viewCode, use_wd = use_wd)
      if(interactive == TRUE){
        return(print(out))}
      if(interactive == FALSE){
        outobjs <- append(outobjs, list(out))
      }
    }
  }

  if(length(outobjs) == 0){
    return()
  }

  if(length(outobjs) == 1){
    outobjs <- outobjs[[1]]
  }

  return(outobjs)
}
