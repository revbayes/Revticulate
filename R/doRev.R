

#'Wrapper for callRev() and revDefine()
#'
#'A wrapper for callRev() and revDefine(). If a variable is assigned in the temporary
#'    rb session, it will also be created in the revEnv. Otherwise, functions like callRev().
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
#'@param interactive Ignore. Used to implement doRev() into repRev().
#'
#'@param Det_Update If TRUE, previously created object in revEnv that is redefined in rb.exe
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
doRev <- function(..., viewCode = FALSE, coerce = TRUE, interactive = FALSE, Det_Update = TRUE, use_wd = T, knit = FALSE){

  if(stringr::str_c(..., collapse = "") == ""){
    return("")
  }

  if(knit){
    coerce = FALSE}

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

  RevOut <- pasteByEnds(c(...), "\\{", "\\}")
  RevOut <- pasteByEnds(RevOut, "\\[", "\\]")
  RevOut <- pasteByEnds(RevOut, "\\(", "\\)")

  RevOut <- stringr::str_squish(RevOut)
  RevOut <- RevOut[which(RevOut != "")]

  if(knit){
    RevOut == stringr::str_c(RevOut, sep = "\n")
  }

  outobjs <- list()


  for(i in 1:length(RevOut)){
    if(stringr::str_detect(RevOut[i], " = | := | <- | ~ ")){
        revDefine(RevOut[i], viewCode = viewCode)

        if(length(revEnv$Deterministic) != 0){
          if(Det_Update == TRUE){
            for(i in unique(revEnv$Deterministic)){
              revDefine(i, viewCode = F, hideMessage = TRUE)
            }
          }
        }
    }
    else{
      out <- callRev(RevOut[i], coerce = coerce, path = revEnv$RevPath, viewCode = viewCode, use_wd = use_wd)
      if(interactive == TRUE)
        return(print(out))
      else
        outobjs <- append(outobjs, list(out))
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
