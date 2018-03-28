#' Create parm widget form 
#' @description  Allows users to enter param widgets with html tools all in one call
#' @param byRow alter widget display (T/F)
#' @example myDiv(h1("Start"), textInput(x), numericInput(y), dateInput(theDate), h1("End"))
#'          submit()
#' @export

##  This function uses the rcw.* functions from rcloud.web. 
## These functions have been copied to rcloud.params.js but will need to be updated 
## when the package is transfered.


paramDiv <- function(..., byRow = FALSE){

    listNames <- as.character(match.call())[-1]  # remove call 
  
  if(!missing(byRow))
    listNames <- listNames[-length(listNames)] # remove byRow arg

  myDiv <- vector(length = length(listNames))

  if(byRow){
    rcloud.html.out(div(id = "param"))
    for(i in seq_len(length(listNames)))
      rcw.append("#param", eval(parse(text = listNames[i])))
    
  } else{
    for(i in seq_len(length(listNames)))
      myDiv[i] <- as.character(div(id = paste0("param", i)))
    
    rcloud.html.out(div(HTML(myDiv)))
    
    for(i in seq_len(length(listNames)))
      rcw.set(paste0("#param", i), eval(parse(text = listNames[i])))
  }
}
