#'Established an interactive rev session using CallRev. To break, type "quit()"
#'
#' @param viewCode If TRUE, uses stringr::str_view_all function to view raw input and output sent to rb
#'
#' @export
RepRev <- function(viewCode = F){

  while(TRUE){
    ginput <- readline(prompt = ">>>")

    if(ginput == "quit()"){break}

    print((RevR::CallRev(ginput, viewCode = viewCode)))

  }
}
