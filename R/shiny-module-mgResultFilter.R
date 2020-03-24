#' Shiny module to pick GSEA method and fdr params used for display
#'
#' @export
#' @rdname mgResultFilter
#' @importFrom shiny NS tagList fluidRow column selectInput numericInput div
#' @importFrom shiny downloadButton
mgResultFilterUI <- function(id, mg=NULL) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        5,
        selectInput(ns("gseaMethod"), "GSEA Method", "")),
      column(
        3,
        numericInput(ns("gseaReportFDR"), "FDR Cutoff", min = 0, max = 1,
                     value = 0.2, step = 0.05)),
      column(
        4,
        tags$div(
          style="padding-top: 1.7em;",
          downloadButton(ns("gseaDownloadStats"), 'Download'))))
  )
}

#' @export
#' @rdname mgResultFilter
#' @importFrom shiny observeEvent req updateSelectInput downloadHandler
#' @importFrom shiny isolate reactive
mgResultFilter <- function(input, output, session, mgc) {

  # When the MultiGSEAResult changes, we want to update different aspects of
  # the application
  observeEvent(mgc(), {
    req(mgc())
    updateSelectInput(session, "gseaMethod",
                      choices=mgc()$methods,
                      selected=mgc()$methods[1L])
  })

  output$gseaDownloadStats <- downloadHandler(
    filename=function() {
      sprintf('multiGSEA-gsea-statistics-%s.csv', isolate(input$gseaMethod))
    },
    content=function(file) {
      write.csv(result(mgc()$mg, isolate(input$gseaMethod)), file,
                row.names=FALSE)
    }
  )

  reactive({
    list(method=reactive(input$gseaMethod), fdr=reactive(input$gseaReportFDR))
  })
}
