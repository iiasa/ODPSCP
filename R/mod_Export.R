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
      bs4Dash::tabsetPanel(
        id = ns("tabcard"),type = "pills",
        tabPanel(
          title = "Export protocol",
          icon = icon("save",lib = "glyphicon"),
          box(
            title = "Output format",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            "All protocol entries can be exported in a Range of different formats
            for further use. Currently supported are the compressed 'rData', 'csv' and
            'yaml' format.
            ",
            # Output format
            shinyWidgets::radioGroupButtons(
              inputId = ns("downloadFormat"),
              label = "Output Format",
              individual = TRUE,
              status = "info",
              choices = c('rData', 'csv', 'yaml'),
              selected = 'rData'
            ),
            br(),
            # Conditional info
            conditionalPanel(
              condition = "input.downloadFormat == 'rData'",
              ns = ns,
              p("Protocol will be exported as a list in a R data object. RData objects
              can only be opened through R and the resulting file can be loaded
              again via load(file) in R.")
            ),
            conditionalPanel(
              condition = "input.downloadFormat == 'csv'",
              ns = ns,
              p("Protocol will be exported as a comma-separated file (csv) that can
              be loaded in any conventional software that allows the modification
              of text or spreadsheets. The protocol can also be reloaded in R via
              read.csv(file).")
            ),
            conditionalPanel(
              condition = "input.downloadFormat == 'yaml'",
              ns = ns,
              p("Protocol will be exported in the YAML Ain't Markup Language, a
              human-readable data serialization language that is common among
              many programming languages. YAML files can be read and edited with
              any text editor and loaded as lists into R via yaml::read_yaml(file).")
            ),
            br(),
            # Button
            downloadButton(ns("downloadData"), "Download the protocol")
          )
        ),
        tabPanel(
          title = "Render protocol",
          icon = icon("list-alt",lib = "glyphicon"),
          box(
            title = "Protocol",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE
          ),
          shiny::textOutput(
            outputId = ns("protocolmarkdown")
            )
        ) # End Tab panel
       ) # End Tabset panel
    )
  )
}

#' Export Server Functions
#'
#' @noRd
mod_Export_server <- function(id, results){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Get output format
    oftype <- reactive({input$downloadFormat})

    # Make a download handler ----
    output$downloadData <- downloadHandler(
      filename = function() {
        # Compose output file
        paste0(
          format(Sys.Date(), "%Y_%m_%d"),
          "__ODPSCP.",
          oftype()
        )
      },
      content = function(file) {
        # Create outputs from results
        protocol <- list()
        exportVals <- names(rvtl(results))
        for(i in 1:length(exportVals)){
          protocol[[exportVals[i]]] <- results[[exportVals[i]]]
        }

        if(oftype() == "rData"){
          save(protocol, file = file)
          # load("../../../Downloads/test.rData")
        } else if(oftype() == "csv"){
          # TODO: Write proper wrapper for csv in wide format
          write.csv(list2DF(protocol), file = file)
          # read.csv("../../../Downloads/test.csv")
        } else if(oftype() == "yaml"){
          yaml::write_yaml(protocol, file = file)
          # yaml::read_yaml("../../../Downloads/test.yaml")
        }
      }
    )

    # Render the whole protocol
    # output$protocolmarkdown <- renderUI(
    #   {includeMarkdown(knitr::knit("protocol_preview.Rmd", quiet = T))}
    #   )

  })
}

## To be copied in the UI
# mod_Export_ui("Export_1")

## To be copied in the server
# mod_Export_server("Export_1")
