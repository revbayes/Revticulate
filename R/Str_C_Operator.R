#'Rapid string parsing
#'
#'Method for quicker pasting of Strings.
#'
#'@rdname stringpaste
#'@usage a \%+\% b
#'@title Operator for concatenation.
#'@param a The first String
#'@param b The second String
#'
#'@export
"%+%" <- function(a, b) stringr::str_c(a, b, sep = "")
