# rcloud.params
Notebook parameters for RCloud

This RCloud package allows notebook variables to be exposed as parameters,
both through query parameters and through UI elements.

Say that we want a variable `x` to be read from a query parameter or defaulted to 1.

Simply load the library, set the variable, and call `mytags$input` on it, and then call `submit`
to generate the submit button:

```{r}
library(rcloud.params)
v <- 1

h3("Using an input tag and changing the type")
mytags$input(v, type = "number")
mytags$input(y, type = "text", value = "Tesing")
mytags$input(myDate, type = "date")
mytags$input(b, type = "range")
mytags$input(boxCheck, type = "checkbox")

h3("Using select tag for a dropdown")
mytags$select(h, lapply(names(airquality), tags$option)) 
mytags$select(x, multiple = "multiple", lapply(names(mtcars), tags$option))
submit()
```

rcloud.params will generate a labelled text input for the variable, display the default value,
and display a submit button and wait for it to be clicked.

If a value is specified as a query parameter, it will populate the variable instead of the
default value.

If there is no default, the text input will be empty and must be filled in before the submit
will be successful:


The variable will created during `submit`. A non-defaulted value can also be specified as
`NA` or assigned using the widget.


Multiple variables can be assigned using one submit() either by individually listing
or using the paramDiv() function to diaply with inline html

```{r}
library(rcloud.params)
library(htmltools)

paramDiv(h1("Start"), 
    textInput(x), 
    numericInput(y), 
    dateInput(theDate), 
    h1("End"), 
    byRow = TRUE
    )
submit()
```

