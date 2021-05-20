#' Coerces string of RevBayes output into an equivalent R object.
#'
#' Coerces string of RevBayes output into an equivalent R object.
#' Uses the structure of the characters within the string to identify an appropiate R object type (a list, vector,
#' or even phylo tree) to coerce the string into.
#'
#' @param ... out
#'
#' @return out Coerce output of the RevBayes output string.
#'
#' @examples
#' coerceRev("[1, 2, 3, 4]")
#'
#'@import stringr
#'
#' @export
#'


coerceRev <- function(out){


  out <- stringr::str_remove_all(out, " ")

  if(stringr::str_c(out, collapse = "") == ""){
    return("")
  }


  spl_vec <- function(inp){
    out <- stringr::str_remove_all(inp, ",|\\[|\\]" )
    out <- stringr::str_split(out, " ")

    return(out)
  }
  count_brackets <- function(string){
    stringr::str_count(string, pattern = "]|\\[")
  }
  test_matrix <- function(output){

    b_vec <- count_brackets(output)

    if(length(b_vec) == 0 | length(b_vec) == 1 ){
      return(FALSE)
    }

    is_matrix <- stringr::str_detect(out , pattern = "] ,")

    if(length(unique(is_matrix[1:length(is_matrix)-1])) == 1 & is_matrix[length(is_matrix)] == FALSE){
      if(unique(is_matrix[1:length(is_matrix)-1]) == TRUE){
        return(TRUE)
      }
    }

    if(length(unique(b_vec[c(1, length(b_vec))])) == 1 & b_vec[1] == 3){

      if(length(b_vec) == 2){return(TRUE)}

      if(unique(b_vec[-c(1, length(b_vec))]) == 2 | unique(b_vec[-c(1, length(b_vec))]) == 0){
        return(TRUE)
      }

      else{return(FALSE)}
    }
    else{
      return(FALSE)
    }
  }
  test_phylo <- function(out){
    if(any(stringr::str_count(out, "\\[&index=")) > 0){return(TRUE)}
    else{return(FALSE)}
  }

  coerce_phylo <- function(out){
    out = grep(out, pattern = "\\[&index=", value = T)
    out = ape::read.tree(text = out)

    return(out)
  }


  #######Coerce matrices
  if(sum(stringr::str_count(out, "\\[")) == 1 & sum(stringr::str_count(out, "\\]")) == 1){
    out <- stringr::str_flatten(out)
    out <- stringr::str_remove_all(out, "\\]|\\[")
    out <- stringr::str_split(out, ",")
    out <- unlist(out)

    ###
    testForNumbers <- grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", out)
    ###

    if(all(testForNumbers)){
      out <- as.numeric(out)
    }
    return(out)
  }


  if(stringr::str_detect(out[1], "\\[\\[") & stringr::str_detect(out[length(out)], "\\]\\]")){
    out <- stringr::str_flatten(out)
    out <- stringi::stri_split_boundaries(out)
    out <- gsub("\\[|\\]", "", out)
    out <- stringi::stri_split_boundaries(out)
    out <- unlist(out)

    if(stringr::str_sub(out[1],1, 3) == "c(\""){
      out[1] <- stringr::str_sub(out[1],4, stringr::str_length(out[1]))
    }

    if(out[length(out)] == ")")
      out <- out[-c(length(out))]

    num_detect <- function(String){
      detect <- function(char){
        char <- unlist((stringr::str_split(char, "")))
        numlist <- which(!is.na(as.numeric(char)))
        return(numlist)
      }
      return(suppressWarnings(detect(String)))
    }

    for(i in 1:length(out)){
      last_num <- max(num_detect(out[i]))
      first_num <- min(num_detect(out[i]))
      full_length <- stringr::str_length(out[i])
      out[i] <- stringr::str_sub(out[i], first_num, last_num)
    }
    out <- as.list(out)
    out <- stringr::str_split(out, ",")
    for(i in 1:length(out)){
      out[[i]] <- as.numeric(out[[i]])
    }
    return(out)
  }

  #coerce simple vectors, strings, and numerics, as well as single line phylogenies
  if(length(out) == 1){


    if(test_phylo(out) == TRUE){
      out <- coerce_phylo(out)
      return(out)
    }

    if(count_brackets(out) == 2 | count_brackets(out) == 0 ){
      coerce <- "vector"
    }

    if(coerce == "vector"){
      out <- spl_vec(out)
      out <- out[[1]]
      out <- out[which(out != "")]

      if(anyNA(as.numeric(out)) == FALSE){
        out <- as.numeric(out)
      }

      if(stringr::str_squish(out)[1] == "FALSE"){return(1 == 2)}
      if(stringr::str_squish(out)[1] == "TRUE"){return(1 == 1)}
    }

    return(out)
  }

  #coerce trees
  if(test_phylo(out) == TRUE){
    out <- grep("\\[&index=", out, value = T)
    out <- coerce_phylo(out)
    return(out)
  }

  if(test_matrix(out) == TRUE){coerce = "array"}

  if(coerce == "array"){

    out <- spl_vec(out)

    for(i in c(1:length(out))){
      out[[i]] <- out[[i]][which(out[[i]] != "")]
    }

    tvec <- c()

    for(i in c(1:length(out))){

      if(anyNA(as.numeric(out[[i]])) == FALSE){append(tvec, FALSE)}
      else{append(tvec, TRUE)}

      if(all(tvec) == TRUE){
        for(i in c(1:length(out))){
          out[[i]] <- as.numeric(out[[i]])
        }
      }
    }


    return(out)
  }

}


