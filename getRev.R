
library(tidyverse)

#Path to rb.exe

RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"



CallRev <- function(..., coerce = TRUE, path = RevPath){
  
  args_ <- c(...)
  
  tf <- tempfile(pattern = "file", tmpdir = paste(getwd(), "/", sep = ""), fileext = ".rev")
  tf <- gsub(pattern = "\\\\", "//", tf)
  
  fopen <- file(tf)
  writeLines(args_, fopen, sep = "\n")
  close(fopen)
  
  out <- system2(path, args = c(tf), stdout = T)
  out <- out[-c(1:13, length(out)-1, length(out))]
  
  if(file.exists(tf)){
    file.remove(tf)
  }
  
  if(coerce == FALSE){
    return(out)
  }
  
  spl_vec <- function(inp){
    out <- stringr::str_remove_all(inp, ",|\\[|\\]" )
    out <- strsplit(out, " ")
    
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
  
  
 
  
  #coerce simple vectors, strings, and numerics, as well as single line phylogenies
  if(length(out) == 1){
    
    unlink(tf)
    
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
    }
    
    return(out)
  }
  
  #coerce trees
   if(test_phylo(out) == TRUE){
      unlink(tf)
      out <- grep("\\[&index=", out, value = T)
      out <- coerce_phylo(out)
      return(out)
    }
    
  #coerce matrices 
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
  
  #make sure tf is gone
  
  unlink(tf)
  
return(out)}






l1 <- "kappaAG ~ dnLognormal(0, 1)"
l2 <- "kappaCT ~ dnLognormal(0, 1)"
l3 <- "pi ~ dnDirichlet( v(1, 1, 1, 1) )"
l4 <- "Q := fnTrN(kappaAG, kappaCT, pi)"
l5 <- 'Q'



CallRev('Q := fnJC(4)', "Q") 
CallRev("2*26^7/4")
CallRev("posteriorPredictiveProbability(v(2), 2)")
CallRev('"jon"')
CallRev("simTree(16)") %>% plot()
CallRev('readTrees("C:/Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/data/Eucladida_MAP.tre")')

CallRev('diagonalMatrix(4)', coerce = "array")
