# rcloud.params
Notebook parameters for RCloud

This RCloud package allows notebook variables to be exposed as parameters,
both through query parameters and through UI elements. Useful for re-rendering a notebook and easily changing key inputs.

# Frontend Control Functions

## HTML R Functions

Functions allowing for modifying HTML elements in RCloud UI.

* `rcloud.ui.set` - sets specified HTML contents
* `rcloud.ui.plot` - renders plot to a specified HTML
* `rcloud.ui.append` - appends content to specified HTML element
* `rcloud.ui.prepend` - prepends content to specified HTML element
* `compact` - a helper function ensuring that shiny.tag is not displayed in an iframe

# Notebook Execution R Functions

Functions controlling the execution of notebook cells.

* `rcloud.run.cell` - runs a specified cell 
* `rcloud.run.cells` - runs a specified cells
* `rcloud.run.cells.from` - runs cells starting from the given cell
* `rcloud.stop.execution` - gracefully (by allowing current cell execution to complete) stops execution of notebook cells 


# Reactive Controls Examples

## `param` function

`param` function allows for exposing notebook variables easily using default controls associated with R classes:

```{r}
z <- 10
text_param <- "Default text"
chck1 <- FALSE
my_date <- Sys.Date()
compact(div(
    param(z, min = -19, label = "Z value", 
        callbacks = list('change' = function(var_name, var_value, ...) {
            rcloud.ui.plot('#result', function() { plot(c(1:var_value)) })
            })),
    div(id="result"),
    param(chck1, label = 'Yes?', 
        callbacks = list('change' = function(name, val, ...) { 
            rcloud.ui.set('#chck-1-result', val)
            })), 
    div(id = 'chck-1-result'),
    param(my_date, label = 'Date',
        callbacks = list('change' = function(name, val, ...) { 
            rcloud.ui.set('#date-result', paste0(name, ' - ', typeof(val), '-', val))
            })), 
    div(id = 'date-result'),
    param(text_param, label = "Text value",
        callbacks = list('change' = function(var_name, var_value, ...) {
            rcloud.ui.set('#text-result', paste0(var_name, ':', var_value))
            })),
    div(id="text-result"),
    submitParam(name='submit1')
    )
)

```

## Text Input
```{r}
paramDiv(h1('Text input'))

textVar <- "Some example message"

paramDiv(
h3("Text with default value from variable"),
textParam(textVar, label = "Text value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#textVar-result', var_value) })),
div(id="textVar-result")
)

paramDiv(
h3("Text with no variable"),
textParam(textNoVar, label = "Text value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#textNoVar-result', var_value) })),
div(id="textNoVar-result")
)

paramDiv(
h3("Text with default value specified using htmltools tag"),
textParam(textValueFromHtmltools, value = 'Value from attribute', label = "Text value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#textValueFromHtmltools-result', var_value) })),
div(id="textValueFromHtmltools-result")) 

```


## Numeric Input
```{r}
paramDiv(h1('Numeric input'))

numericNotebookVar <- 10

paramDiv(
h3("Numeric Parameter with default value from variable"),
numericParam(numericNotebookVar, min = -19, label = "Notebook variable value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericNotebookVar-result', c(1:var_value)) })),
div(id="numericNotebookVar-result")
)

paramDiv(
h3("Numeric Parameter with no variable"),
numericParam(numericNoVar, min = -19, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericNoVar-result', c(1:var_value)) })),
div(id="numericNoVar-result")
)

paramDiv(
h3("Numeric Parameter with default value specified using htmltools tag"),
numericParam(numericValueFromHtmltools, value = 12, min = -19, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericValueFromHtmltools-result', c(1:var_value)) })),
div(id="numericValueFromHtmltools-result"))
```

## Slider Input
```{r}
paramDiv(h1('Numeric slider input'))

numericSliderNotebookVar <- 10

paramDiv(
h3("Numeric with default value from variable"),
numericSliderParam(numericSliderNotebookVar, min = -19, max = 25, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericSliderNotebookVar-result', c(1:var_value)) })),
div(id="numericSliderNotebookVar-result")
)

paramDiv(
h3("Numeric with no variable"),
numericSliderParam(numericSliderNoVar, min = -19, max = 25, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericSliderNoVar-result', c(1:var_value)) })),
div(id="numericSliderNoVar-result")
)

paramDiv(
h3("Numeric with default value specified using htmltools tag"),
numericSliderParam(numericSliderValueFromHtmltools, value = 12, min = -19, max = 25, label = "Numeric value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#numericSliderValueFromHtmltools-result', c(1:var_value)) })),
div(id="numericSliderValueFromHtmltools-result"))
```


## Single Select
```{r}
paramDiv(h1('Single select input'))

selectVar <- 2

paramDiv(
h3("Select with default value from variable"),
selectParam(selectVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#selectVar-result', var_value) })),
div(id="selectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam(selectNoVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#selectNoVar-result', var_value) })),
div(id="selectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam(selectValueFromHtmltools, value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
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
selectParam(multiSelectVar, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#multiSelectVar-result', var_value) })),
div(id="multiSelectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam(multiSelectNoVar, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#multiSelectNoVar-result', var_value) })),
div(id="multiSelectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam(multiSelectValueFromHtmltools, value = 3, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
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
checkboxParam(checkboxVar, label = "Selected?", 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#multiSelectVar-result', var_value) })),
div(id="multiSelectVar-result")
)

paramDiv(
h3("Checkbox with no variable"),
checkboxParam(checkboxNoVar, label = "Selected?",
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#checkboxNoVar-result', var_value) })),
div(id="checkboxNoVar-result")
)

paramDiv(
h3("Checkbox with default value specified using htmltools tag"),
checkboxParam(checkboxValueFromHtmltools, checked = TRUE, label = "Selected?", 
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
dateParam(dateVar, label = "Date value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#dateVar-result', var_value) })),
div(id="dateVar-result")
)

paramDiv(
h3("Date with no variable"),
dateParam(dateNoVar, label = "Date value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#dateNoVar-result', var_value) })),
div(id="dateNoVar-result")
)

paramDiv(
h3("Date with default value specified using htmltools tag"),
dateParam(dateValueFromHtmltools, value = '2017-08-30', label = "Date value", callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#dateValueFromHtmltools-result', var_value) })),
div(id="dateValueFromHtmltools-result"))
```

## Radio Buttons

```{r}
paramDiv(h1('Radio button input'))

choiceVar <- 2

paramDiv(
h3("Choice with default value from variable"),
choiceParam(choiceVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { rcloud.ui.set('#choiceVar-result', var_value) })),
div(id="choiceVar-result")
)

paramDiv(
h3("Choice with no variable"),
choiceParam(choiceNoVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    callbacks = list('change' = function(var_name, var_value, ...) { 
    rcloud.ui.set('#choiceNoVar-result', var_value) })),
div(id="choiceNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
choiceParam(choiceValueFromHtmltools, value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
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


# Reactve Form Example

## Simple Example

### Cell 1

```{r}
paramDiv(h1('Reactive form'))

fromVar <- 10

paramSet(
numericParam(fromVar, min = 0, label = "From value"),
numericParam(toVar, min = 0, label = "To value"),
submitParam(),
    on_submit = function(var_name, var_value, ...) {
      rcloud.run.cells.from(2)  
    }
)
```

### Cell 2

```{r}
plot(c(fromVar:toVar)) 
```

## Wait for Form on Demand

### Cell 1
```{r}
paramDiv(h1('Reactive form'))

fromVar <- 10

paramSet(wait_if_invalid=FALSE, name='my-form',
numericParam(fromVar, min = 0, label = "From value"),
numericParam(toVar, min = 0, label = "To value"),
submitParam(),
    on_submit = function(var_name, var_value, ...) {
      my_function(fromVar, toVar)
    }
)
```

### Cell 2
```{r}
# Some content used by form callback function

my_function <- function(from, to) {
    rcloud.ui.plot('#result-div', function() { plot(c(from:to))})
    rcloud.run.cell(4)
}

paramDiv(id='result-div') 
```

### Cell 3

```{r}
waitForForm('my-form') 
```
### Cell 4

```{r}
plot(c(fromVar:toVar)) 
```




# Blocking Form Example

The following code displays  a parameters form which blocks notebook execution until values for all parameters are provided.

```{r}
z <- 10
text_param = "Default text"
synchronousParamSet(div(
    numericParam(z, min = -19, label = "Z value"),
    selectParam(choice1, 'Select value', 
        choices = list('1' = "first", '2' = "second")), 
    selectParam(choice2, 'Select value', multiple='multiple', 
        choices = list('1' = "first", '2' = "second")),
    numericSliderParam(the_range, min = 1, max = 100, label = "Select value"),
    checkboxParam(chck1, 'Yes?'),
    dateParam(my_date, 'Date'),
    textParam(text_param, label = "Text value"),
    submitParam(name='submit1')
    ), on_submit = function(form_name, values, ...) {
     print(values)   
    }
)

z
choice1
choice2
the_range
chck1
my_date
text_param

```

> Note, in case of a blocking form, any registered reactive callback functions on specific controls are disabled.