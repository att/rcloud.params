# rcloud.params
Notebook parameters for RCloud

This RCloud package allows notebook variables to be exposed as parameters,
both through query parameters and through UI elements.

Say that we want a variable `x` to be read from a query parameter or defaulted to 1.

Simply load the library, set the variable, and call `param` on it, and then call `submit`
to generate the submit button:

```{r}
library(rcloud.params)
x <- 1
param(x)
submit()
```

rcloud.params will generate a labelled text input for the variable, display the default value,
and display a submit button and wait for it to be clicked.

If a value is specified as a query parameter, it will populate the variable instead of the
default value.

If there is no default, the text input will be empty and must be filled in before the submit
will be successful:

```{r}
library(rcloud.params)
param(y)
submit()
```

The variable will created during `submit`. A non-defaulted value can also be specified as
`NA`.


