
#' A ReactiveGeneSetDb for use in the shiiny world.
#'
#' This can be instantiated from a "static" or "reactive" GeneSetDb object.
#' It allows users to customize which genesets are active by:
#'
#' 1. Filtering out entire collections; and
#' 2. Filtering genesets based on min and max (gene) size.
#'
#' This should be in mutliGSEA.shiny, but is developed here for now to make
#' it easier to develop within the FacieAnalysis shiny world.
#'
#' @export
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shiny updateSliderInput renderUI
#'
#' @param gdb A static or reactive GeneSetDb object
#' @param min.gs.size,max.gs.size the default minimum and maximum geneset size
#'   set in the UI when `gdb` is first loaded or changes (when reactive)
reactiveGeneSetDb <- function(input, output, session, gdb,
                              min.gs.size = 2L, max.gs.size = Inf,
                              default_collections = NULL, ...) {
  assert_character(default_collections, null.ok = TRUE)
  min.gs.size <- assert_number(round(min.gs.size), lower = 2L, null.ok = FALSE)
  max.gs.size <- assert_number(round(max.gs.size), lower = 2L, null.ok = TRUE)

  state <- reactiveValues(
    gdb = NULL,
    min.gs.size = min.gs.size,
    max.gs.size = max.gs.size)

  rgdb <- reactive({
    gdb. <- if (is(gdb, "reactive")) gdb() else gdb
    assert_class(gdb., "GeneSetDb")
    gdb.
  })

  observe({
    gs.range <- input$size
    if (state$min.gs.size != gs.range[1L]) state$min.gs.size <- gs.range[1L]
    if (state$min.gs.size != gs.range[2L]) state$max.gs.size <- gs.range[2L]
  })

  rmin.gs.size <- reactive(state$min.gs.size)
  rmax.gs.size <- reactive(state$max.gs.size)

  observeEvent(rgdb(), {
    gdb. <- req(rgdb())
    req(is(gdb., "GeneSetDb"))

    # ........................................................ collection picker
    gsets <- geneSets(gdb.)
    min.n <- min(gsets$N)
    max.n <- max(gsets$N)
    colls.all <- unique(gsets$collection)
    colls.selected <- intersect(colls.all, default_collections)

    if (length(colls.selected) == 0L) {
      colls.selected <- colls.all
    }

    gs.count <- table(gsets$collection)
    names(colls.all) <- sprintf("%s [%d total gene sets]", colls.all,
                                gs.count[colls.all])
    updatePickerInput(
      session,
      "collections",
      choices = colls.all,
      selected = colls.selected)

    # .............................................................. size slider
    if (!is.null(min.gs.size)) {
      min.val <- max(min.gs.size, min.n)
    } else {
      min.val <- min.n
    }
    if (!is.null(max.gs.size)) {
      max.val <- min(max.gs.size, max.n)
    } else {
      max.val <- max.n
    }
    state$min.gs.size <- min.val
    state$max.gs.size <- max.val
    updateSliderInput(session, "size", min = min.n, max = max.n,
                      value = c(min.n, max.n))
  })

  genesets <- reactive({
    selected.colls <- input$collections
    gdb. <- req(rgdb())
    req(is(gdb., "GeneSetDb"))
    gsets <- geneSets(gdb.) %>%
      filter(collection %in% selected.colls,
             N >= rmin.gs.size(), N <= rmax.gs.size())
    gsets
  })

  output$gscount <- renderUI({
    tags$span(nrow(genesets()))
  })

  vals <- list(
    gdb = rgdb,
    geneSets = genesets,
    min.gs.size = rmin.gs.size,
    max.gs.size = rmax.gs.size,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("ReactiveGeneSetDb")
  vals
}

#' UI components to filter a GeneSetDb by collection and set size
#'
#' @export
#' @importFrom shiny NS tagList sliderInput uiOutput
#' @importFrom shinyWidgets pickerInput
reactiveGeneSetDbFilterUI <- function(id, min = 2, max = 100L, ...) {
  ns <- NS(id)

  tagList(
    tags$p(
      tags$span("Gene Sets Selected:",
                style = "font-weight: bold; color: #FF7F00"),
      uiOutput(ns("gscount"), inline = TRUE)),
    sliderInput(ns("size"), "Set Size", min = min, max = max,
                value = c(min, max)),
    pickerInput(
      ns("collections"),
      "Collections",
      choices = NULL,
      multiple = TRUE,
      options = list(
        `selected-text-format`= "count",
        `count-selected-text` = "{0} collections chosen"
      )))
}

#' Returns the filtered unreactive GeneSetDb from the reactiveGeneSetDb module.
#'
#' The user can choose to include subsets of collections, as well as
#' genesets of a certain size.
#'
#' This can only be run within a reactive context.
#'
#' @noRd
#' @export
GeneSetDb.ReactiveGeneSetDb <- function(x, ...) {
  gdb <- x$gdb()
  gsets.all <- geneSets(gdb)
  gsets.selected <- x$geneSets()

  keep <- is.element(
    paste(gsets.all$collection, gsets.all$name),
    paste(gsets.selected$collection, gsets.selected$name))

  if (!all(keep)) {
    gdb <- gdb[keep]
  }

  gdb
}
