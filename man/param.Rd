% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcloud.params.R
\name{param}
\alias{param}
\title{Creates parameter for a variable}
\usage{
param(object, ...)
}
\arguments{
\item{object}{variable which should be exposed as a parameter}

\item{...}{extra parameters to specific implementations of `*Param` functions}
}
\value{
parameter shiny.tag
}
\description{
A simple way of exposing a variable as notebook parameter. Creates a parameter based on the variable type being passed in.
}
\details{
Just a basic set of controls are supported, to create radio buttons, numeric sliders and select inputs please use specific `*Param` functions.
}
