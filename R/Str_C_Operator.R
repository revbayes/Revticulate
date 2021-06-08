#'Rapid string parsing
#'
#'Method for quicker pasting of Strings.
#'
#'@rdname stringpaste
#'@title Operator for concatenation.
#'@param a The first String
#'@param b The second String
#'
#'@examples
#' \dontrun{
#' \%+\%(a, b) append string b to string a
#' }
#'@export
"%+%" <- function(a, b) stringr::str_c(a, b, sep = "")
