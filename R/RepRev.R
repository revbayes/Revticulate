#'Established an interactive rev session using CallRev. To break, type "quit()"
#'
#' @param viewCode If TRUE, uses stringr::str_view_all function to view raw input and output sent to rb
#'
#' @export
RepRev <- function(viewCode = F){
defs <- c()

while(TRUE){
        defs <- unique(defs)
        ginput <- readline(prompt = ">>>")
        RevDefine(ginput)

        if(ginput == "quit()"){break}
        if(stringr::str_detect(ginput, " <- | = | := | ~ ") == TRUE){
               defs <- append(defs, ginput %+% "\n")
             }

        out <- CallRev(defs, ginput, viewCode = viewCode)
        print(out)
    }
}

