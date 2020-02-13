#' Creates a GeneSetDb from a user-specfied gene set definition table.
#'
#' This module provides an upload button that allows a user to upload a
#' table of gene set definitions in [mutiGSEA::GeneSetDb()] format. Minimal
#' validation checks are implemented.
#'
#' @export
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom shiny need observe observeEvent reactive reactiveValues validate
#' @importFrom shinyjs toggleState
#' @return A list of reactive components. `$gdb()` will be a GeneSetDb when
#'   the user uploades a valid gene set definition file. Otherwise it will be
#'   `NULL`.
userDefinedGeneSetDb <- function(input, output, session, ...) {

  empty.def <- data.frame(
    collection = character(), name = character(), feature_id = character(),
    stringsAsFactors = FALSE)

  state <- reactiveValues(
    dat = empty.def)

  observeEvent(input$upload, {
    type <- input$upload$type
    path <- input$upload$datapath
    ext <- file_ext(path)

    validate(
      need(ext %in% c("csv", "xlsx"), "Only csv or xlsx files are supported")
    )

    if (ext == "csv") {
      dat <- try(read.csv(path, stringsAsFactors = FALSE), silent = TRUE)
    } else {
      dat <- try(read_excel(path), silent = TRUE)
    }

    validate(
      need(is.data.frame(dat), "Error parsing geneset definition file")
    )

    req.cols <- c("collection", "name", "feature_id")
    missed <- setdiff(req.cols, colnames(dat))
    validate(
      need(
        length(missed) == 0L,
        sprintf("Missing columns: %s", paste(missed, collapse = ","))))

    dat[["feature_id"]] <- as.character(dat[["feature_id"]])
    state$dat <- dat %>%
      mutate(collection = as.character(collection),
             name = as.character(name),
             feature_id = as.character(feature_id))
  })

  gdb <- reactive({
    dat. <- state$dat
    out <- if (is.null(dat.) || nrow(dat.) == 0) NULL else GeneSetDb(dat.)
    out
  })

  observe({
    toggleState("rm", condition = is(gdb(), "GeneSetDb"))
  })

  observeEvent(input$rm, {
    state$dat <- empty.def
  })

  vals <- list(
    gdb = gdb,
    .ns = session$ns)
  class(vals) <- "ReactiveGeneSetDb"
  vals
}

#' @noRd
#' @export
#' @importFrom shiny actionButton fileInput NS tags tagList
userDefinedGeneSetDbUI <- function(id, ..., debug = FALSE) {
  ns <- NS(id)
  inline.style <- "display: inline-block; vertical-align:top;"

  tagList(
    tags$div(
      style = inline.style,
      fileInput(
        ns("upload"), "Additional Gene Set Definition File",
        multiple = FALSE,
        accept = c(
          # --- CSV stuff ---
          "text/csv", ".csv",
          "text/comma-separated-values,text/plain",
          # --- xlsx stuff (not .xls) ---
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx"))),
    tags$div(
      style = paste(inline.style, "margin-top: 1.8em"),
      actionButton(ns("rm"), "Reset")))

}
