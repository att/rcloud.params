# rcloud.params
Notebook parameters for RCloud

This RCloud package allows notebook variables to be exposed as parameters,
both through query parameters and through UI elements. Useful for re-rendering a notebook and easily changing key inputs.

1. Use `param_set` to assign multiple variables. Each variable is given a list with information about how the parameter should be disaplyed in a notebook. Once paramters are set call `submit` to generate a submit button. Once selected query string will be update and variables assigned in the notebook with the correct class (eg. date input to "Date")

```{r}
library(rcloud.params)
x <- 10
param_set(
    x = list(label = "Minimum",  min = 1, input = "numeric"),
    title_mango = list(label = "The title", value = "Mango", input = "text"), 
    cars_cols = list(label = "Choose", input = "select",  multiple = "multiple", 
                     choices =names(mtcars)),
    my_date = list(label = "Date", input = "date"),
    the_range = list(label = "Range", input= "slider", value = 10, min = 1, max = 100),
    check_option = list(label = "Yes?", input = "checkbox")
)
submit()
 
```

Widget will generate a labelled input for the variable.
Widget is populated in 3 possible ways:

1. Display the default value provided in function call
2. Display value if variable already exists in notebook env 
3. Display value if specified as a query parameter


If there is no default, the input will be empty and must be filled in before the submit
will be successful:

The variable will created during `submit`. A non-defaulted value can also be specified as
`NA` or assigned using the widget.

Paramaterized widgets can be written inline with html tools objects, or wrapped inside a re-write of the div function. (This may change as rcloud.params div uses rloud.web caps `rcw.append` and `rcw.prepend`)

```{r}
library(rcloud.params)
x <- 10
div(
  h1("Report parameters"),
  param_set(x = list(input = "numeric", label = "Assign value for x"),
  y = list(input = "numeric", label = "Assign value for y")),
  h2("End")
  )
submit(function() { plot(c(x:y))}) 
```


