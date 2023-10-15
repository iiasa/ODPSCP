#' Export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Export_ui <- function(id){
  ns <- NS(id)

  tabItem(
    tabName = "Export",
    fluidPage(
      box(
        title = "Export the protocol",
        closable = FALSE,
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        "All protocol entries can be exported in a Range of different formats
        fur further use. Currently supported are the compressed 'rData', 'csv' and
        'yaml' format.
        "
      ),
      br(),
      p(
        # Output format
        shinyWidgets::radioGroupButtons(
          inputId = ns("downloadFormat"),
          label = "Output Format",
          individual = TRUE,
          status = "info",
          choices = c("rData", "csv", "yaml"),
          selected = "rData"
        ),
        br(),
        # Button
        downloadButton(ns("downloadData"), "Download the protocol")
      )
    )
  )
}

#' Export Server Functions
#'
#' @noRd
mod_Export_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Get output format
    oftype <- reactive(input$downloadFormat)

    # Collate all inputs
    all_inputs <- reactive({
      out <- list()
      for(i in 1:length(names(input))){
        out[[names(input)[i]]] <- input[[names(input)[i]]]
      }
      out
    })

    # Make a download handler ----
    output$downloadData <- downloadHandler(
      filename = function() {
        # Compose output file
        paste0(
          format(Sys.Date(), "%Y_%m_%d"),
          "__ODPSCP__",
          oftype
        )
      },
      content = function(file) {
        if(oftype == "rData"){
          save(all_inputs, file = opfile)
        } else if(oftype == "csv"){
          write.csv(all_inputs, opfile, row.names = FALSE)
        } else if(oftype == "yaml"){
          yaml::write_yaml(all_inputs,file = opfile )
        }
      }
    )

  })
}

## To be copied in the UI
# mod_Export_ui("Export_1")

## To be copied in the server
# mod_Export_server("Export_1")
