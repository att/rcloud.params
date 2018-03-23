## Form
## Allows users to enter param widgets with html tools all in one call
## eg. theDate <- "2018-03-20"
##     myDiv(h1("Start"), textInput(x), numericInput(y), dateInput(theDate), h1("End"))
##     submit()

paramDiv <- function(..., byRow = FALSE){
  
  listNames <- as.character(match.call())[-1]
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