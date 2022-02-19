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
#'@examples
#' \dontrun{
#' doRev("simTree(16)", coerce=FALSE)
#' doRev("a <- 10")
#' doRev("a * 100")
#'}
#'
#'@export
#'

doRev <- function(input, coerce = TRUE, evaluate = TRUE, viewCode = FALSE, timeout = 5){

  input <- unlist(input)

  if(!evaluate){
    cat(input, file = Sys.getenv("revHistory"), append = TRUE, sep = "\n")
    return("")
  }

  if(length(input) != 1)
    stop("Input length must equal one.")

  if(!all((str_count(input, c("\\(", "\\{", "\\[")) == str_count(input, c("\\)", "\\}", "\\]"))))){
    stop("RevBayes command must have an equal number of open and closing braces!")
  }

  allCode <- getRevHistory()

  try({

    first <- callRev(getRevHistory(), coerce = FALSE, timeout = timeout)
    cat(input, file = Sys.getenv("revHistory"), append = TRUE, sep = "\n")
    last <- callRev(getRevHistory(), coerce = FALSE, viewCode = viewCode, timeout = timeout)

  }, silent = TRUE)
  if(length(first) != 0)
    now <- last[-c(1:length(first))]
  else now <- last

  if(length(now) == 0)
    now <- ""

  if (any(str_detect(now, pattern = "Error:|error|Missing Variable:"))) {
    cat(allCode, file = Sys.getenv("revHistory"), append = FALSE, sep = "\n")
    if(coerce){
      cat(stringr::str_squish(now), file = stderr(), sep = "\n")
      return(invisible())
    }
  }

  if(coerce){
    now <- coerceRev(now)
    return(now)
  }

  if(length(now) > 1){
    if(str_squish(now[1]) == ""){
      now <- now[2:length(now)]
    }
  }

  return(now)
}
