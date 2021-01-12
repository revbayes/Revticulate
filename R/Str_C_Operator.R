#'Wrapper for stringr::str_c()
#'
#'Wrapper for stringr::str_c() used for quicker pasting of Strings
#'
#'@name \%+\%
#'@usage \%+\%(a, b)
#'@title Operator for concatenation
#'@param a The first String
#'@param b The second String
#'
#'@export
"%+%" <- function(a, b) stringr::str_c(a, b, sep = "")
