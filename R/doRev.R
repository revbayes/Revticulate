
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
doRev <- function(..., trackCode = FALSE, viewCode = FALSE, coerce = TRUE, interactive = FALSE, Det_Update = TRUE, use_wd = T, knit = FALSE){

  if(stringr::str_c(..., collapse = "") == ""){
    return("")
  }

  if(trackCode == TRUE){
    RevEnv$allCode = append(RevEnv$allCode, ...)
  }

  #group elements by curly braces

  pasteByEnds <- function(stringArray, openEnd, closeEnd){

    clumpedVector <- c()
    tots = 0

    for(i in stringArray){

      if(tots != 0)
        clumpedVector[length(clumpedVector)] = clumpedVector[length(clumpedVector)] %+% i
      else
        clumpedVector = c(clumpedVector, i)

      tots = tots + stringr::str_count(i, openEnd)
      tots = tots - stringr::str_count(i, closeEnd)

    }

    return(clumpedVector)

  }

  if(knit){
    coerce = FALSE}


  RevOut <- c(...)

  RevOut <- pasteByEnds(c(...), "\\{", "\\}")
  RevOut <- pasteByEnds(c(RevOut), "\\[", "\\]")
  RevOut <- unlist(pasteByEnds(c(RevOut), "\\(", "\\)"))



  RevOut <- stringr::str_squish(RevOut)
  RevOut <- RevOut[which(RevOut != "")]

  if(knit){
    RevOut == stringr::str_c(RevOut, sep = "\n")
  }

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

  if(knit == TRUE){
    outobjs <- unlist(outobjs)
  }

  return(outobjs)
}
