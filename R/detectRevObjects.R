#'Locates names of objects from revenv located in a string. Object names must be surrounded by '#'.
#'
#'@param string string vector where rev object names will be located
#'@return revObjs: Names of objects from revenv found in string
#'
#'@export
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
