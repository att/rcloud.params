# rcloud.params
Notebook parameters for RCloud

This RCloud package allows notebook variables to be exposed as parameters,
both through query parameters and through UI elements. Useful for re-rendering a notebook and easily changing key inputs.


# Installation

* Install rcloud.params package
* load `rcloud.params` in notebook cell or enable `rcloud.params` extension via Settings panel (or add it to `rcloud.alluser.addons` property in conf/rcloud.conf settings file)


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
textVar <- "Default text"
booleanVar <- FALSE
dateVar <- Sys.Date()
compact(div(
    param(z, min = -19, label = "Z value", 
        on.change = function(var.name, var.value, ...) {
            rcloud.ui.plot('#result', function() { plot(c(1:var.value)) })
            }),
    div(id="result"),
    param(booleanVar, label = 'Yes?', 
        on.change = function(name, val, ...) { 
            rcloud.ui.set('#chck-1-result', val)
            }), 
    div(id = 'chck-1-result'),
    param(dateVar, label = 'Date',
        on.change = function(name, val, ...) { 
            rcloud.ui.set('#date-result', paste0(name, ' - ', typeof(val), '-', val))
            }), 
    div(id = 'date-result'),
    param(textVar, label = "Text value",
        on.change = function(var.name, var.value, ...) {
            rcloud.ui.set('#text-result', paste0(var.name, ':', var.value))
            }),
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
textParam(textVar, label = "Text value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#textVar-result', var.value) }),
div(id="textVar-result")
)

paramDiv(
h3("Text with no variable"),
textParam(textNoVar, label = "Text value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#textNoVar-result', var.value) }),
div(id="textNoVar-result")
)

paramDiv(
h3("Text with default value specified using htmltools tag"),
textParam(textValueFromHtmltools, value = 'Value from attribute', label = "Text value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#textValueFromHtmltools-result', var.value) }),
div(id="textValueFromHtmltools-result")) 

```


## Numeric Input
```{r}

paramDiv(h1('Numeric input'))

numericNotebookVar <- 10

paramDiv(
h3("Numeric Parameter with default value from variable"),
numericParam(numericNotebookVar, min = -19, label = "Notebook variable value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#numericNotebookVar-result', c(1:var.value)) }),
div(id="numericNotebookVar-result")
)

paramDiv(
h3("Numeric Parameter with no variable"),
numericParam(numericNoVar, min = -19, label = "Numeric value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#numericNoVar-result', c(1:var.value)) }),
div(id="numericNoVar-result")
)

paramDiv(
h3("Numeric Parameter with default value specified using htmltools tag"),
numericParam(numericValueFromHtmltools, value = 12, min = -19, label = "Numeric value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#numericValueFromHtmltools-result', c(1:var.value)) }),
div(id="numericValueFromHtmltools-result"))
```

## Slider Input
```{r}

paramDiv(h1('Numeric slider input'))

numericSliderNotebookVar <- 10

paramDiv(
h3("Numeric with default value from variable"),
numericSliderParam(numericSliderNotebookVar, min = -19, max = 25, label = "Numeric value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#numericSliderNotebookVar-result', c(1:var.value)) }),
div(id="numericSliderNotebookVar-result")
)

paramDiv(
h3("Numeric with no variable"),
numericSliderParam(numericSliderNoVar, min = -19, max = 25, label = "Numeric value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#numericSliderNoVar-result', c(1:var.value)) }),
div(id="numericSliderNoVar-result")
)

paramDiv(
h3("Numeric with default value specified using htmltools tag"),
numericSliderParam(numericSliderValueFromHtmltools, value = 12, min = -19, max = 25, label = "Numeric value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#numericSliderValueFromHtmltools-result', c(1:var.value)) }),
div(id="numericSliderValueFromHtmltools-result"))
```


## Single Select
```{r}

paramDiv(h1('Single select input'))

selectVar <- 2

paramDiv(
h3("Select with default value from variable"),
selectParam(selectVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { rcloud.ui.set('#selectVar-result', var.value) }),
div(id="selectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam(selectNoVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#selectNoVar-result', var.value) }),
div(id="selectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam(selectValueFromHtmltools, value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#selectValueFromHtmltools-result', var.value) }),
div(id="selectValueFromHtmltools-result"))
```

## Multi-Select
```{r}

paramDiv(h1('Miltiple select input'))

multiSelectVar <- c(2,3)

paramDiv(
h3("Select with default value from variable"),
selectParam(multiSelectVar, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { rcloud.ui.set('#multiSelectVar-result', var.value) }),
div(id="multiSelectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam(multiSelectNoVar, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#multiSelectNoVar-result', var.value) }),
div(id="multiSelectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam(multiSelectValueFromHtmltools, value = 3, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#multiSelectValueFromHtmltools-result', var.value) }),
div(id="multiSelectValueFromHtmltools-result"))
```

## Checkbox
```{r}

paramDiv(h1('Checkbox input'))

logicalVar <- TRUE

paramDiv(
h3("Checkbox with default value from variable"),
logicalParam(logicalVar, label = "Selected?", 
    on.change = function(var.name, var.value, ...) { rcloud.ui.set('#logicalVar-result', var.value) }),
div(id="logicalVar-result")
)

paramDiv(
h3("Checkbox with no variable"),
logicalParam(logicalNoVar, label = "Selected?",
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#logicalNoVar-result', var.value) }),
div(id="logicalNoVar-result")
)

paramDiv(
h3("Checkbox with default value specified using htmltools tag"),
logicalParam(logicalValueFromHtmltools, checked = TRUE, label = "Selected?", 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#logicalValueFromHtmltools-result', var.value) }),
div(id="logicalValueFromHtmltools-result"))
```

## Date Input

```{r}

paramDiv(h1('Date input'))

dateVar <- Sys.Date()

paramDiv(
h3("Date with default value from variable"),
dateParam(dateVar, label = "Date value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#dateVar-result', var.value) }),
div(id="dateVar-result")
)

paramDiv(
h3("Date with no variable"),
dateParam(dateNoVar, label = "Date value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#dateNoVar-result', var.value) }),
div(id="dateNoVar-result")
)

paramDiv(
h3("Date with default value specified using htmltools tag"),
dateParam(dateValueFromHtmltools, value = '2017-08-30', label = "Date value", on.change = function(var.name, var.value, ...) { rcloud.ui.set('#dateValueFromHtmltools-result', var.value) }),
div(id="dateValueFromHtmltools-result"))
```

## Radio Buttons

```{r}

paramDiv(h1('Radio button input'))

choiceVar <- 2

paramDiv(
h3("Choice with default value from variable"),
choiceParam(choiceVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { rcloud.ui.set('#choiceVar-result', var.value) }),
div(id="choiceVar-result")
)

paramDiv(
h3("Choice with no variable"),
choiceParam(choiceNoVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#choiceNoVar-result', var.value) }),
div(id="choiceNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
choiceParam(choiceValueFromHtmltools, value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcloud.ui.set('#choiceValueFromHtmltools-result', var.value) }),
div(id="choiceValueFromHtmltools-result"))
```

## Action Buttons

```{r}

paramDiv(h1('Action buttons'))

paramDiv(
h3("Execute two actions on click"),
buttonParam(value = "Execute",
    on.click = list(
        function(var.name, var.value, ...) { 
            rcloud.ui.set('#action-result', var.name) 
            }, 
        function(var.name, var.value, ...) { 
            rcloud.ui.set('#action-result2', c(1:10)) }
        )
        ),
div(id="action-result"),
div(id="action-result2")
)
```


# Reactve Form Example

## Simple Example

```{r}

z <- 10
textVar = "Default text"
paramSet(div(
    numericParam(z, min = -19, label = "Z value"),
    selectParam(select, 'Select value', 
        choices = list('1' = "first", '2' = "second")), 
    choiceParam(choice, 'Select value', multiple='multiple', 
        choices = list('1' = "first", '2' = "second")),
    numericSliderParam(rangeVar, min = 1, max = 100, label = "Select value"),
    logicalParam(chck, 'Yes?', required = FALSE),
    dateParam(dateVar, 'Date'),
    textParam(textVar, label = "Text value"),
    submitParam(name='submit1')
    ), on.submit = function(form.name, values, ...) {
     rcloud.ui.set('#result-div', values)   
    }
)

paramDiv(id='result-div')
```

## Execute a Specific Cell

### Cell 1

```{r}

paramDiv(h1('Reactive form'))

fromVar <- 10

paramSet(
numericParam(fromVar, min = 0, label = "From value"),
numericParam(toVar, min = 0, label = "To value"),
submitParam(),
    on.submit = function(var.name, var.value, ...) {
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

paramSet(wait.if.invalid=FALSE, name='my-form',
numericParam(fromVar, min = 0, label = "From value"),
numericParam(toVar, min = 0, label = "To value"),
submitParam(),
    on.submit = function(var.name, var.value, ...) {
      myFunction(fromVar, toVar)
    }
)
```

### Cell 2
```{r}
# Some content used by form callback function

myFunction <- function(from, to) {
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

The following code displays a parameters form which blocks notebook execution until values for all parameters are provided.

> Note, in case of a blocking form, any registered reactive callback functions on specific controls are disabled.

```{r}

z <- 10
textVar = "Default text"
synchronousParamSet(div(
    numericParam(z, min = -19, label = "Z value"),
    selectParam(select, 'Select value', 
        choices = list('1' = "first", '2' = "second")), 
    choiceParam(choice, 'Select value', multiple='multiple', 
        choices = list('1' = "first", '2' = "second")),
    numericSliderParam(rangeVar, min = 1, max = 100, label = "Select value"),
    logicalParam(chck, 'Yes?'),
    dateParam(dateVar, 'Date'),
    textParam(textVar, label = "Text value"),
    submitParam(name='submit1')
    ), on.submit = function(form.name, values, ...) {
     print(values)   
    }
)

z
select
choice
rangeVar
chck
dateVar
textVar

```
