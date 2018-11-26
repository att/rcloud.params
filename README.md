# rcloud.params
Notebook parameters for RCloud

This RCloud package allows notebook variables to be exposed as parameters,
both through query parameters and through UI elements. Useful for re-rendering a notebook and easily changing key inputs.


# Installation

* Install rcloud.params package
* load `rcloud.params` in notebook cell or enable `rcloud.params` extension via Settings panel (or add it to `rcloud.alluser.addons` property in conf/rcloud.conf settings file)


# Frontend Control Functions

## HTML R Functions

* `compact` - a helper function ensuring that shiny.tag is not displayed in an iframe


# Reactive Controls Examples

## `param` function

`param` function allows for exposing notebook variables easily using default controls associated with R classes:

```{r}

numVar <- 10
textVar <- "Default text"
booleanVar <- FALSE
dateVar <- Sys.Date()
compact(div(
    param(numVar, min = -19, label = "Z value", 
        on.change = function(var.name, var.value, ...) {
            rcw.plot('#result', function() { plot(c(1:var.value)) })
            }),
    div(id="result"),
    param(booleanVar, label = 'Yes?', 
        on.change = function(name, val, ...) { 
            rcw.set('#chck-1-result', val)
            }), 
    div(id = 'chck-1-result'),
    param(dateVar, label = 'Date',
        on.change = function(name, val, ...) { 
            rcw.set('#date-result', paste0(name, ' - ', typeof(val), '-', val))
            }), 
    div(id = 'date-result'),
    param(textVar, label = "Text value",
        on.change = function(var.name, var.value, ...) {
            rcw.set('#text-result', paste0(var.name, ':', var.value))
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
textParam(textVar, label = "Text value", on.change = function(var.name, var.value, ...) { rcw.set('#textVar-result', var.value) }),
div(id="textVar-result")
)

paramDiv(
h3("Text with no variable"),
textParam(textNoVar, label = "Text value", on.change = function(var.name, var.value, ...) { rcw.set('#textNoVar-result', var.value) }),
div(id="textNoVar-result")
)

paramDiv(
h3("Text with default value specified using htmltools tag"),
textParam(textValueFromHtmltools, value = 'Value from attribute', label = "Text value", on.change = function(var.name, var.value, ...) { rcw.set('#textValueFromHtmltools-result', var.value) }),
div(id="textValueFromHtmltools-result")) 

```


## Numeric Input
```{r}

paramDiv(h1('Numeric input'))

numericNotebookVar <- 10

paramDiv(
h3("Numeric Parameter with default value from variable"),
numericParam(numericNotebookVar, min = -19, label = "Notebook variable value", 
on.change = function(var.name, var.value, ...) { rcw.set('#numericNotebookVar-result', c(1:var.value)) }),
div(id="numericNotebookVar-result")
)

paramDiv(
h3("Numeric Parameter with no variable"),
numericParam(numericNoVar, min = -19, label = "Numeric value", 
on.change = function(var.name, var.value, ...) { rcw.set('#numericNoVar-result', c(1:var.value)) }),
div(id="numericNoVar-result")
)

paramDiv(
h3("Numeric Parameter with default value specified using htmltools tag"),
numericParam(numericValueFromHtmltools, value = 12, min = -19, label = "Numeric value", 
on.change = function(var.name, var.value, ...) { rcw.set('#numericValueFromHtmltools-result', c(1:var.value)) }),
div(id="numericValueFromHtmltools-result"))
```

## Slider Input

```{r}

paramDiv(h1('Numeric slider input'))

numericSliderNotebookVar <- 10

paramDiv(
h3("Numeric with default value from variable"),
numericSliderParam(numericSliderNotebookVar, min = -19, max = 25, label = "Numeric value", 
on.change = function(var.name, var.value, ...) { rcw.set('#numericSliderNotebookVar-result', c(1:var.value)) }),
div(id="numericSliderNotebookVar-result")
)

paramDiv(
h3("Numeric with default value specified using htmltools tag"),
numericSliderParam(numericSliderValueFromHtmltools, value = 12, min = -19, max = 25, label = "Numeric value", 
on.change = function(var.name, var.value, ...) { rcw.set('#numericSliderValueFromHtmltools-result', c(1:var.value)) }),
div(id="numericSliderValueFromHtmltools-result"))
```

> Note, for slider and checkbox the parameter must have a defined value (via variable or query parameter) (see API Details section). Namely, the following WON'T work:

```{r}

paramDiv(
h3("Numeric with no variable"),
numericSliderParam(numericSliderNoVar, min = -19, max = 25, label = "Numeric value", 
on.change = function(var.name, var.value, ...) { rcw.set('#numericSliderNoVar-result', c(1:var.value)) }),
div(id="numericSliderNoVar-result")
)
```

## Single Select
```{r}

paramDiv(h1('Single select input'))

selectVar <- 2

paramDiv(
h3("Select with default value from variable"),
selectParam(selectVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { rcw.set('#selectVar-result', var.value) }),
div(id="selectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam(selectNoVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#selectNoVar-result', var.value) }),
div(id="selectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam(selectValueFromHtmltools, value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#selectValueFromHtmltools-result', var.value) }),
div(id="selectValueFromHtmltools-result"))
```

## Multi-Select
```{r}

paramDiv(h1('Miltiple select input'))

multiSelectVar <- c(2,3)

paramDiv(
h3("Select with default value from variable"),
selectParam(multiSelectVar, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { rcw.set('#multiSelectVar-result', var.value) }),
div(id="multiSelectVar-result")
)

paramDiv(
h3("Select with no variable"),
selectParam(multiSelectNoVar, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#multiSelectNoVar-result', var.value) }),
div(id="multiSelectNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
selectParam(multiSelectValueFromHtmltools, value = 3, label = "Select value", multiple = NA, choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#multiSelectValueFromHtmltools-result', var.value) }),
div(id="multiSelectValueFromHtmltools-result"))
```

## Checkbox
```{r}

paramDiv(h1('Checkbox input'))

logicalVar <- TRUE

paramDiv(
h3("Checkbox with default value from variable"),
logicalParam(logicalVar, label = "Selected?", 
    on.change = function(var.name, var.value, ...) { rcw.set('#logicalVar-result', var.value) }),
div(id="logicalVar-result")
)

paramDiv(
h3("Checkbox with default value specified using htmltools tag"),
logicalParam(logicalValueFromHtmltools, checked = TRUE, label = "Selected?", 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#logicalValueFromHtmltools-result', var.value) }),
div(id="logicalValueFromHtmltools-result"))
```

> Note, for slider and checkbox the parameter must have a defined value (via variable or query parameter) (see API Details section). Namely, the following WON'T work:

```{r}

paramDiv(
h3("Checkbox with no variable"),
logicalParam(logicalNoVar, label = "Selected?",
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#logicalNoVar-result', var.value) }),
div(id="logicalNoVar-result")
)

```

## Date Input

```{r}

paramDiv(h1('Date input'))

dateVar <- Sys.Date()

paramDiv(
h3("Date with default value from variable"),
dateParam(dateVar, label = "Date value", on.change = function(var.name, var.value, ...) { rcw.set('#dateVar-result', var.value) }),
div(id="dateVar-result")
)

paramDiv(
h3("Date with no variable"),
dateParam(dateNoVar, label = "Date value", on.change = function(var.name, var.value, ...) { rcw.set('#dateNoVar-result', var.value) }),
div(id="dateNoVar-result")
)

paramDiv(
h3("Date with default value specified using htmltools tag"),
dateParam(dateValueFromHtmltools, value = '2017-08-30', label = "Date value", on.change = function(var.name, var.value, ...) { rcw.set('#dateValueFromHtmltools-result', var.value) }),
div(id="dateValueFromHtmltools-result"))
```

## Radio Buttons

```{r}

paramDiv(h1('Radio button input'))

choiceVar <- 2

paramDiv(
h3("Choice with default value from variable"),
choiceParam(choiceVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { rcw.set('#choiceVar-result', var.value) }),
div(id="choiceVar-result")
)

paramDiv(
h3("Choice with no variable"),
choiceParam(choiceNoVar, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#choiceNoVar-result', var.value) }),
div(id="choiceNoVar-result")
)

paramDiv(
h3("Select with default value specified using htmltools tag"),
choiceParam(choiceValueFromHtmltools, value = 3, label = "Select value", choices = list('1' = "first", '2' = "second", '3' = "third"), 
    on.change = function(var.name, var.value, ...) { 
    rcw.set('#choiceValueFromHtmltools-result', var.value) }),
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
            rcw.set('#action-result', var.name) 
            }, 
        function(var.name, var.value, ...) { 
            rcw.set('#action-result2', c(1:10)) }
        )
        ),
div(id="action-result"),
div(id="action-result2")
)
```


# Reactve Forms Examples

## Simple Example

```{r}

numVar <- 10
textVar <- "Default text"
rangeVar <- 13
logicalVar <- FALSE
paramSet(div(
    numericParam(numVar, min = -19, label = "Z value"),
    selectParam(select, 'Select value', 
        choices = list('1' = "first", '2' = "second")), 
    choiceParam(choice, 'Select value', multiple='multiple', 
        choices = list('1' = "first", '2' = "second")),
    numericSliderParam(rangeVar, min = 1, max = 100, label = "Select value"),
    logicalParam(logicalVar, 'Yes?', required = FALSE),
    dateParam(dateVar, 'Date'),
    textParam(textVar, label = "Text value"),
    submitParam(name='submit1')
    ), on.submit = function(form.name, values, ...) {
     rcw.set.crs('#result-div', values)   
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
    rcw.plot('#result-div', function() { plot(c(from:to))})
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

## Reactive Parameter Sets

### Register Callback Function with Parameter Set

`rcloud.params` supports registering a common callback function to all parameters defined in a single parameter set.

#### Cell 1
```{r}
paramDiv(h1('Reactive parameter set'))

fromVar <- 10
toVar <- 3

paramSet(
numericParam(fromVar, min = 11, label = "From value"),
numericParam(toVar, min = 0, label = "To value"),
on.change = function(var_name, var_value, ...) {
      rcloud.run.cells.from(2)  
    }
)

```

#### Cell 2
```{r}
plot(c(fromVar:toVar))

```

## Reactive Parameter Sets - Missing Variable

### Register Callback Function with Parameter Set

If a `on.submit` callback function is not defined for a paramSet and the form is invalid (due to missing values) an error will be produced.

#### Cell 1
```{r}
paramDiv(h1('Reactive parameter set'))

fromVar <- 10
# The following value is missing:
#toVar <- 3

paramSet(
numericParam(fromVar, min = 11, label = "From value"),
numericParam(toVar, min = 0, label = "To value"),
on.change = function(var_name, var_value, ...) {
      rcloud.run.cells.from(2)  
    }
)

```

#### Cell 2
```{r}
plot(c(fromVar:toVar))

```

### Register Callbacks Dynamically

`rcloud.params` package allows for dynamic registration of callbacks to existing parameters. In the following example a callback function is registered for a single parameter (`toVar`) from a parameter set.

> Note, to remove callbacks of a given type registered with a parameter use `removeCallbacks` 

#### Cell 1
```{r}
paramDiv(h1('Reactive parameter set'))

fromVar <- 10
toVar <- 3

paramSet(
numericParam(fromVar, min = 11, label = "From value"),
numericParam(toVar, min = 0, label = "To value")
)

myCallback <- function(var_name, var_value, ...) {
      rcloud.run.cells.from(2)  
    }


addCallback(toVar,  'change',  myCallback)

```

#### Cell 2
```{r}
plot(c(fromVar:toVar))

```

## Blocking Form Example

The following code displays a parameters form which blocks notebook execution until values for all parameters are provided.

> Note, in case of a blocking form, any registered reactive callback functions on specific controls are disabled untill form is successfully submitted.

```{r}

z <- 10
textVar <- "Default text"
chck <- FALSE
rangeVar <- 33
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

## Mini Mode

### Reactive Form

```{r}
library(rcloud.web)
library(rcloud.params)

rcw.result(
    run = function(...) {
        numVar <<- 10
        textVar <<- "Default text"
        rangeVar <<- 13
        logicalVar <<- FALSE
        params <- paramSet(
                numericParam(numVar, min = -19, label = "Z value"),
                selectParam(select, 'Select value', 
                    choices = list('1' = "first", '2' = "second")), 
                choiceParam(choice, 'Select value', multiple='multiple', 
                    choices = list('1' = "first", '2' = "second")),
                numericSliderParam(rangeVar, min = 1, max = 100, label = "Select value"),
                logicalParam(logicalVar, 'Yes?', required = FALSE),
                dateParam(dateVar, 'Date'),
                textParam(textVar, label = "Text value"),
                submitParam(name='submit1')
                , on.submit = function(form.name, values, ...) {
                 rcw.set('#result-div', values)   
                }
            )
        rcw.set.paramSet('#form', params)
    },
    body = as.character(div(div(id='form'), div(id='result-div')))
    ) 
```

### Blocking Form

```{r}
library(rcloud.web)
library(rcloud.params)

rcw.result(
    run = function(...) {
        numVar <<- 10
        textVar <<- "Default text"
        chck <<- FALSE
        rangeVar <<- 33
        params <- synchronousParamSet(div(
            numericParam(numVar, min = -19, label = "Z value"),
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
             rcw.set('#result-div', as.character(values))   
            }
        )
        rcw.set.paramSet('#form', params)
    },
    body = as.character(div(div(id='form'), div(id='result-div')))
    ) 

```


# API Details

## Undefined Value Handling

If value for a parameter is undefined (i.e. the corresponding variable is not initialized, parameter value is not specified with url parameter nor a value is provided as htmltools parameter), the following rules apply:
* `rcloud.params` will NOT initialize the variable
* Checkbox and Slider will fail to display, as their initial state represents a specific value which involves implicit mapping of empty value (`NULL`) to a specific value (e.g. a middle of range in case of slider) which leads to invalid state


## Debugging

## Logging

Debug messages logging can be enabled for `rcloud.params` package by invoking `rcloud.params.debug.on()` and disabled with `rcloud.params.debug.off()`.

> The debug messages are printed to JavaScript console.
