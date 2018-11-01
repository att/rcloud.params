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

# UI R Functions

* `rcloud.ui.set`
* `rcloud.ui.plot`
* `rcloud.ui.append`
* `rcloud.ui.prepend`

# Reactive Controls Examples

## Text Input
```{r}
paramDiv(h1('Text input'))

textVar <- "Some example message"

paramDiv(
h3("Text with default value from variable"),
textParam('textVar', label = "Text value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#textVar-result', var_value) })),
div(id="textVar-result")
)

paramDiv(
h3("Text with no variable"),
textParam('textNoVar', label = "Text value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#textNoVar-result', var_value) })),
div(id="textNoVar-result")
)

paramDiv(
h3("Text with default value specified using htmltools tag"),
textParam('textValueFromHtmltools', value = 'Value from attribute', label = "Text value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#textValueFromHtmltools-result', var_value) })),
div(id="textValueFromHtmltools-result"))  
```


## Numeric Input
```{r}
paramDiv(h1('Numeric input'))

numericNotebookVar <- 10

paramDiv(
h3("Numeric Parameter with default value from variable"),
numericParam('numericNotebookVar', min = -19, label = "Notebook variable value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericNotebookVar-result', c(1:var_value)) })),
div(id="numericNotebookVar-result")
)

paramDiv(
h3("Numeric Parameter with no variable"),
numericParam('numericNoVar', min = -19, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericNoVar-result', c(1:var_value)) })),
div(id="numericNoVar-result")
)

paramDiv(
h3("Numeric Parameter with default value specified using htmltools tag"),
numericParam('numericValueFromHtmltools', value = 12, min = -19, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericValueFromHtmltools-result', c(1:var_value)) })),
div(id="numericValueFromHtmltools-result")) 
```

## Slider Input
```{r}
paramDiv(h1('Numeric slider input'))

numericSliderNotebookVar <- 10

paramDiv(
h3("Numeric with default value from variable"),
numericSliderParam('numericSliderNotebookVar', min = -19, max = 25, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericSliderNotebookVar-result', c(1:var_value)) })),
div(id="numericSliderNotebookVar-result")
)

paramDiv(
h3("Numeric with no variable"),
numericSliderParam('numericSliderNoVar', min = -19, max = 25, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericSliderNoVar-result', c(1:var_value)) })),
div(id="numericSliderNoVar-result")
)

paramDiv(
h3("Numeric with default value specified using htmltools tag"),
numericSliderParam('numericSliderValueFromHtmltools', value = 12, min = -19, max = 25, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericSliderValueFromHtmltools-result', c(1:var_value)) })),
div(id="numericSliderValueFromHtmltools-result"))
```


## Single Select
```{r}
paramDiv(h1('Single select input'))

selectVar <- 2

paramDiv(
h3("Select with default value from variable"),
selectParam('selectVar', label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#selectVar-result', var_value) })),
div(id="selectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam('selectNoVar', label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#selectNoVar-result', var_value) })),
div(id="selectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam('selectValueFromHtmltools', value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#selectValueFromHtmltools-result', var_value) })),
div(id="selectValueFromHtmltools-result")) 
```

## Multi-Select
```{r}
paramDiv(h1('Miltiple select input'))

multiSelectVar <- c(2,3)

paramDiv(
h3("Select with default value from variable"),
selectParam('multiSelectVar', label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#multiSelectVar-result', var_value) })),
div(id="multiSelectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam('multiSelectNoVar', label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#multiSelectNoVar-result', var_value) })),
div(id="multiSelectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam('multiSelectValueFromHtmltools', value = 3, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#multiSelectValueFromHtmltools-result', var_value) })),
div(id="multiSelectValueFromHtmltools-result")) 
```

## Checkbox
```{r}
paramDiv(h1('Checkbox input'))

checkboxVar <- TRUE

paramDiv(
h3("Checkbox with default value from variable"),
checkboxParam('checkboxVar', label = "Selected?", 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#multiSelectVar-result', var_value) })),
div(id="multiSelectVar-result")
)

paramDiv(
h3("Checkbox with no variable"),
checkboxParam('checkboxNoVar', label = "Selected?",
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#checkboxNoVar-result', var_value) })),
div(id="checkboxNoVar-result")
)

paramDiv(
h3("Checkbox with default value specified using htmltools tag"),
checkboxParam('checkboxValueFromHtmltools', checked = TRUE, label = "Selected?", 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#checkboxValueFromHtmltools-result', var_value) })),
div(id="checkboxValueFromHtmltools-result")) 
```

## Date Input

```{r}
paramDiv(h1('Date input'))

dateVar <- Sys.Date()

paramDiv(
h3("Date with default value from variable"),
dateParam('dateVar', label = "Date value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#dateVar-result', var_value) })),
div(id="dateVar-result")
)

paramDiv(
h3("Date with no variable"),
dateParam('dateNoVar', label = "Date value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#dateNoVar-result', var_value) })),
div(id="dateNoVar-result")
)

paramDiv(
h3("Date with default value specified using htmltools tag"),
dateParam('dateValueFromHtmltools', value = '2017-08-30', label = "Date value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#dateValueFromHtmltools-result', var_value) })),
div(id="dateValueFromHtmltools-result")) 
```

## Radio Buttons

```{r}
paramDiv(h1('Radio button input'))

choiceVar <- 2

paramDiv(
h3("Choice with default value from variable"),
choiceParam('choiceVar', label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#choiceVar-result', var_value) })),
div(id="choiceVar-result")
)

paramDiv(
h3("Choice with no variable"),
choiceParam('choiceNoVar', label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#choiceNoVar-result', var_value) })),
div(id="choiceNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
choiceParam('choiceValueFromHtmltools', value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#choiceValueFromHtmltools-result', var_value) })),
div(id="choiceValueFromHtmltools-result"))
```

## Action Buttons

```{r}
paramDiv(h1('Action buttons'))

paramDiv(
h3("Execute two actions on click"),
buttonParam(value = "Execute",
    callbacks = list('click' = list(
        function(var_name, var_value, ...) { rcloud.ui.set('#action-result', var_name) }, 
        function(var_name, var_value, ...) { rcloud.ui.set('#action-result2', c(1:10)) }))
        
        ),
div(id="action-result"),
div(id="action-result2")
)
```
