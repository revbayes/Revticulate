#'Operator to streamline pasting. Uses stringr::str_c for speed
#'@param a The first string to concatenate
#'@param b The second string to concatenate
`%+%` <- function(a, b) stringr::str_c(a, b, sep = "")
