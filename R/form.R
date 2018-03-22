## Form
## Allows users to enter param widgets with html tools all in one call
## eg. theDate <- "2018-03-20"
##     myDiv(h1("Start"), textInput(x), numericInput(y), dateInput(theDate), h1("End"))
##     submit()

myDiv <- function(...){
  
  listNames <- as.character(match.call())[-1]
  
  myDiv <- vector(length = length(listNames))
  for(i in seq_len(length(listNames))){
    myDiv[i] <- as.character(div(id = paste0("param", i)))
  }
  
  # Create the dic with an id for each html object
  rcloud.html.out(
    div(HTML(myDiv))
  )
  
  for(i in seq_len(length(listNames))){
    rcw.set(paste0("#param", i), eval(parse(text = listNames[i])))
  }
  
  
}