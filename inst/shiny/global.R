# ui.R requires miniUI and shinys to be `library`-d, so putting this here.
# unfortunately pollutes workspace of user if this app isn't run as a
# standalone shiny app
library(shinyjs)
library(miniUI)

if (!interactive()) {
  # If the shiny server is launching this app directly, then I need the library
  # calls.
  #
  # Otherwise, this same global.R file is also launched via the call from
  # multiGSEA.shiny::explore(), and I don't want these library calls to pollute
  # a users workspace.
  library(multiGSEA.shiny)
  library(DT)

  ## Loading "standard" Libraries ------------------------------------------------
  library(shiny)
  library(shinydashboard)
  library(data.table)
  library(dplyr)
  library(dtplyr)
}

## By default shiny limits upload size to 5 MB, let's change this to 30MB
## (which is kind of big, no?)
# options(shiny.maxRequestSize=30*1024^2)
options(shiny.maxRequestSize=Inf)

