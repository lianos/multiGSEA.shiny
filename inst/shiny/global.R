library(multiGSEA.shiny)
library(DT)

## Loading "standard" Libraries ------------------------------------------------
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(dtplyr)

# theme_set(theme_bw())

## By default shiny limits upload size to 5 MB, let's change this to 30MB
## (which is kind of big, no?)
options(shiny.maxRequestSize=30*1024^2)

