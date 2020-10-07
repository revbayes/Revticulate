#'Locates objects from revenv in a string and replaces that part of the string with a string reperesentation of the object that can be read in rb
#'
#'@param string String containing names of revenv objects surrounded by '#' and '#'
#'@return The origional string coerced into a format that will read the revenv objects into rb
#'
#'
#'@export
repRevObjects <- function(string){

  if(stringr::str_count(string, "#") < 2){return(string)}

  objects <- RevR::detectRevObjects(string)

  Rclass <-  class(get(RevR::detectRevObjects(string), envir = revenv))


  cs <- unique(data.frame(objects, Rclass))

  revd <- c()

  for(i in 1:nrow(cs)){

    if(cs[i, 2] == "character"){
      string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#", RevR::getSS(cs[i, 1]))
    }
    if(cs[i, 2] == "numeric"){
      string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#", RevR::getVS(cs[i, 1]))
    }
    if(cs[i, 2] == "list"){
      string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#",  RevR::getMS(cs[i, 1]))
    }
    if(cs[i, 2] == "phylo"){
      string <- stringr::str_replace_all(string, "#" %+% cs[i, 1] %+% "#",  RevR::getTS(cs[i, 1]))
    }

  }

  return(string)
}

