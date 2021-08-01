#'Wrapper for callRev(). Runs previous code in revEnv$allCode to allow the user to
#'refer to objects that have been defined in rb but not in the revEnv.
#'
#'@param input Code snippet to be ran in rb.exe.
#'
#'@param viewCode If true, Rev code input and output will be displayed in the viewing pane.
#'
#'@param coerce If true, the output from RevBayes will be coerced into R format with coerceRev()
#'
#'@param timeout Determines how long the system2() call should wait before timing out (seconds). Default is 5.
#'
#'@export
#'

doRev <- function(input, viewCode = FALSE, coerce = FALSE, timeout = 5){
  #revEnv$allCode <- readLines(revEnv$revHistory, warn = F)

  input <- paste0(input, "\n")

  if(!all((str_count(input, c("\\(", "\\{", "\\[")) == str_count(input, c("\\)", "\\}", "\\]"))))){
    stop("RevBayes command must have an equal number of open and closing braces!")
  }

    if(length(getRevHistory()) == 0)
       now <- str_squish(callRev(input, coerce = F, viewCode = viewCode, timeout = timeout))
    else{
      first <- callRev(getRevHistory(), coerce = F, timeout = timeout)
      last <- callRev(unlist(c(getRevHistory(), input)), coerce = F, viewCode = viewCode, timeout = timeout)
      now <- last[suppressWarnings(last != first)]
      now <- str_squish(now)
    }

    if (any(str_detect(now, pattern = "Error:|error|Missing Variable:"))) {
       if(coerce == TRUE)
         return(warning(str_squish(now)))
      else
        return(now)
    }

    #if(length(now) == 0)
    #  now <- ""

    if(length(now) > 1)
      now <- now[which(now != "")]

    cat(input, file = Sys.getenv("RevHistory"), append = TRUE, sep = "\n\n")

    if(coerce)
      return(coerceRev(now))

    return(now)








}
