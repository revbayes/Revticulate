#'Get variable definitions from RevEnv
#'
#'Returns vector of lines in .Revhistory containing variable definitions
#'
#'@param varName Only returns lines where specific variable names are defined. If NULL, all lines are returned
#'
#' @examples
#' \dontrun{
#' getRevVars()
#' }
#' \dontrun{
#' getRevVars("var1")
#' }
#'
#' @return varList: character. Vector of lines from .Revhistory containing assignment variables.
#'
#' @return singleVar: character. Vector of lines from .Revhistory containing an assignment variable for a specific variable name.
#'
#'@export
#'
getRevVars <- function(varName = NULL){
    varList <- grep(" <- | = | := | ~ ", readLines(Sys.getenv("RevHistory")), value = TRUE)
    if(is.null(varName))
      return(varList)
    else{
      singleVar <- grep("^" %+% varName %+% " ", varList, value = TRUE)
      return(singleVar)
    }
}
