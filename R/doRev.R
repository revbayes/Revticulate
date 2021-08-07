#'Wrapper for callRev(). Runs previous code in the .Revhistory file to allow user-created Rev variables to persist between interactions.
#'
#'@param input Code snippet to run in the RevBayes executable
#'
#'@param viewCode If TRUE, Rev code input and output will be displayed in the viewing pane.
#'
#'@param coerce If TRUE, the output from RevBayes will be coerced into R format with coerceRev()
#'
#'@param timeout Determines how long the system2() call should wait before timing out (seconds). Default is 5.
#'
#'@return now: type varies. If coerce = TRUE, coerceRev() will attempt to convert RevBayes output into an equivalent R object. Else, return type is character.
#'
#'@export
#'

doRev <- function(input, viewCode = FALSE, coerce = FALSE, timeout = 5){

  if(length(input) != 1)
    stop("Input length must equal one.")

  if(!all((str_count(input, c("\\(", "\\{", "\\[")) == str_count(input, c("\\)", "\\}", "\\]"))))){
    stop("RevBayes command must have an equal number of open and closing braces!")
  }

  allCode <- getRevHistory()

  try({
    first <- callRev(getRevHistory(), coerce = FALSE, timeout = timeout)
    cat(input, file = Sys.getenv("RevHistory"), append = TRUE, sep = "\n")
    last <- callRev(getRevHistory(), coerce = FALSE, viewCode = viewCode, timeout = timeout)
  }, silent = TRUE)
  if(length(first) != 0)
    now <- last[-c(1:length(first))]
  else now <- last

  if(length(now) == 0)
    now <- ""

  if (any(str_detect(now, pattern = "Error:|error|Missing Variable:"))) {
    cat(allCode, file = Sys.getenv("RevHistory"), append = FALSE, sep = "\n")
    if(coerce){
      warning(stringr::str_squish(now))
      return("")
    }
  }

  if(coerce){
    now <- coerceRev(now)
    return(now)
  }

  now <- stringr::str_squish(now)

  if(length(now) > 1)
    now <- now[which(now != "")]


  return(now)
}
