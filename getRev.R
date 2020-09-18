
#Path to rb.exe

RevPath <- "C://Users/Caleb/Documents/WrightLab/RevBayes_Win_v1.0.13/RevBayes_Win_v1.0.13/rb.exe"




CallRev <- function(..., coerce = FALSE, path = RevPath){
 
  args_ <- c(...)
  
  tf <- tempfile(pattern = "file", tmpdir = paste(getwd(), "/", sep = ""), fileext = ".rev")
  tf <- gsub(pattern = "\\\\", "//", tf)
  
  fopen <- file(tf)
  writeLines(args_, fopen, sep = "\n")
  close(fopen)
  
  out <- system2(path, args = c(tf), stdout = T)
  out <- out[-c(1:13, length(out)-1, length(out))]
  
  if(coerce == "vector"){
    out <- stringr::str_remove_all(out, ",|\\[|\\]" )
    out <- strsplit(out, " ")
    out <- out[[1]][which(out[[1]] != "")]
  }
  
  if(coerce == "numeric"){
    out <- stringr::str_remove_all(out, ",|\\[|\\]" )
    out <- strsplit(out, " ")
    out <- as.numeric(out[[1]][which(out[[1]] != "")])
  }
  
  if(coerce == "phylo"){
    out <- ape::read.tree(text = out)
  }
  
  unlink(tf)
  
return(out)}

