#' A geneset selector for a ReactiveGeneSetDb
#'
#' This module is unique becauase if you pass `gdb = NULL` it returns a shim
#' of iteself that will never trigger reactivity.
#'
#' @export
#' @importFrom shiny observeEvent
#' @param gdb A [reactiveGeneSetDb()] module, or `NULL`
#' @return A list of geneset info and membersihp
reactiveGeneSetSelect <- function(input, output, session, gdb = NULL, ...) {
  assert_multi_class(gdb, c("reactive", "GeneSetDb"), null.ok = TRUE)

  state <- reactiveValues(
    collection = NULL,
    name = NULL)

  if (is.null(gdb)) {
    out <- list(
      collection = reactive(NULL),
      name = reactive(NULL),
      membership = reactive(tibble(feature_id = character())))
    return(out)
  }

  rgdb <- callModule(reactiveGeneSetDb, "gdb", gdb, ...)

  observe({
    req(initialized(rgdb))
    gsets <- rgdb$geneSets()
    collections <- unique(gsets$collection)
    choices <- sapply(collections, function(coll) {
      gnames <- filter(gsets, collection == coll)$name
      key <- paste(coll, gnames, sep = ";;")
      names(key) <- gnames
      if (length(key) == 1L) list(key) else key
    }, simplify = FALSE)
    choices <- c(list("---"), choices)
    updateSelectizeInput(session, "geneset", choices = choices,
                         selected = NULL, server = TRUE)
  })

  observeEvent(input$geneset, {
    if (unselected(input$geneset)) {
      collection <- ""
      name <- ""
    } else {
      info <- strsplit(input$geneset, ";;")[[1]]
      collection <- info[1L]
      name <- info[2L]
    }
    if (!isTRUE(state$collection == collection)) {
      state$collection <- collection
    }
    if (!isTRUE(state$name == name)) {
      state$name <- name
    }
  })

  gscoll <- reactive(state$collection)
  gsname <- reactive(state$name)

  membership <- reactive({
    req(initialized(rgdb))
    gdb. <- rgdb$gdb()
    coll <- gscoll()
    name <- gsname()
    if (!unselected(coll) && !unselected(name)) {
      out <- as_tibble(multiGSEA::geneSet(gdb., collection = coll, name = name))
      out <- select(out, feature_id)
    } else {
      out <- tibble(feature_id = character())
    }
    out
  })

  list(
    collection = gscoll,
    name = gsname,
    membership = membership)
}

#' @noRd
#' @export
#' @importFrom shiny column fluidRow icon selectizeInput
#' @importFrom shinyWidgets dropdown
reactiveGeneSetSelectUI <- function(id, label = NULL, dropdown_width = "350px",
                                    ...) {
  ns <- NS(id)
  fluidRow(
    column(
      9,
      selectizeInput(ns("geneset"), label = label, choices = NULL,
                     multiple = FALSE)),
    column(
      3,
      dropdown(
        inputId = ns("opts"),
        icon = icon("sliders"),
        status = "primary",
        with = dropdown_width,
        tags$div(
          id = ns("genesetdbconfig"),
          tags$h4("Gene Set Selection"),
          reactiveGeneSetDbFilterUI(ns("gdb")))))
  )
}
