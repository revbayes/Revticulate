


`%+%` <- function(a, b) stringr::str_c(a, b, sep = "")



RevDefine <- function(RevOut){
  
  if(stringr::str_detect(RevOut, "=|:=|<-|~")){
    sign <- stringr::str_extract_all(RevOut, "=|:=|<-|~")[[1]]}
  
  objdef <- unlist(stringr::str_split(RevOut, sign))
  
  objdef<- stringr::str_squish(objdef)
  
  output <- CallRev(objdef[2])
  output.str <- CallRev(objdef[2], coerce = F)
  output.str <- stringr::str_squish(output.str[which(stringr::str_count(stringr::str_squish(output.str), "") > 0)])
  
  makeActiveBinding(objdef[1], function() output, revenv)
  makeActiveBinding(objdef[1] %+% ".str", function() output.str, revenv)
  
}


getTS <- function(outTree){
  
  revSTR <- "readTrees(text = " %+% get(outTree %+% ".str", envir = revenv) %+% ")"

return(revSTR)}

getMS <- function(outMatrix){
  
  initSTR <- get(outMatrix %+% ".str", envir = revenv)

  matrix1 <- stringr::str_replace_all(stringr::str_flatten(stringr::str_replace_all(initSTR, "] ,", replacement = "][")), " ", "")
  matrix2 <- stringr::str_replace_all(matrix1, "\\]\\[", "\\],\\[")
  coercedMatrix <- 'matrix(' %+% matrix2 %+% ')'
  
return(coercedMatrix)  
}

getSS <- function(outString){
  
  initSTR <- get(outString %+% ".str", envir = revenv)
  
  coerceString <- '"' %+% initSTR %+% '"'
   
  return(coerceString)
}

getVS <- function(outVector){
  initVec <- get(outVector %+% ".str", envir = revenv)
  revVec <- stringr::str_squish(initVec)
  return(revVec)
}


detectRevObjects <- function(string){
  
  markers <- stringr::str_locate_all(string, "#")[[1]][,1]
  
  startstop <- data.frame(markers[which(1:length(markers) %% 2 != 0)], markers[which(1:length(markers) %% 2 == 0)])
    
  revObjs <- c()
  
    for(i in 1:nrow(startstop)){
      pattern <- stringr::str_sub(string, startstop[i, 1], startstop[i, 2])
      namePattern <- stringr::str_sub(pattern, 2, stringr::str_length(pattern)-1)
      revObjs <-  append(revObjs, namePattern)
      
    }
  
  return(revObjs)
  
}




repRevObjects <- function(string){
  
  if(stringr::str_count(string, "#") < 2){return(string)}
  
  objects <- detectRevObjects(string)
  
  RObjs <-  sapply(objects, FUN = get, envir = revenv)
  
  class <- sapply(RObjs, FUN = class)
  
  cs <- unique(data.frame(objects, class))
  
  revd <- c()
  
     for(i in 1:nrow(cs)){
     
       if(cs[i, 2] == "character"){
         string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#", getSS(cs[i, 1]))
       }
       if(cs[i, 2] == "numeric"){
         string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#", getVS(cs[i, 1]))
       }
       if(cs[i, 2] == "list"){
         string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#",  getMS(cs[i, 1]))
       }
       if(cs[i, 2] == "phylo"){
         string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#",  getTS(cs[i, 1]))
       }
       
     }
     
  return(string)
}




