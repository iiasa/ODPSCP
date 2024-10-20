#' Export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils write.csv
#' @importFrom bs4Dash insertTab tabItem tabItems
#' @importFrom shiny actionButton tabsetPanel column
mod_Export_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "Export",
    shiny::fluidPage(
      bs4Dash::tabsetPanel(
        id = ns("tabcard"),type = "pills",
        shiny::tabPanel(
          title = "Export protocol",
          icon = shiny::icon("save",lib = "glyphicon"),
          bs4Dash::box(
            title = "Output format",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            shiny::p(
              "All protocol entries can be exported in a range of different formats
              for further use such as appending them to a manuscript.
              It should be noted that only 'csv' and 'yaml' are
              machine-readable formats and can be imported again by ODPSCP.
              "
            ),
            shiny::br(),
            # Output format
            shinyWidgets::radioGroupButtons(
              inputId = ns("downloadFormat"),
              label = "Output Format",
              individual = TRUE,
              justified = TRUE,
              size = "lg",
              status = "info",
              choices = c('docx', 'csv', 'yaml'),
              selected = 'csv',
              checkIcon = list(
                yes = shiny::icon("circle-down"),
                no = NULL
              )
            ),
            shiny::br(),
            # Conditional info
            shiny::conditionalPanel(
              condition = "input.downloadFormat == 'docx'",
              ns = ns,
              shiny::p("Protocol will be rendered as docx Word document, which
                       can be easily modified as well. Suitabile for manuscript
                       appendices or personal records.")
            ),
            shiny::conditionalPanel(
              condition = "input.downloadFormat == 'pdf'",
              ns = ns,
              shiny::p("Protocol will be rendered as document and then converted
                       to a PDF format. This is the most convenient format for
                       appending the protocol to any publications for example.")
            ),
            shiny::conditionalPanel(
              condition = "input.downloadFormat == 'rData'",
              ns = ns,
              shiny::p("Protocol will be exported as a list in a R data object. RData objects
              can only be opened through R and the resulting file can be loaded
              again via load(file) in R.")
            ),
            shiny::conditionalPanel(
              condition = "input.downloadFormat == 'csv'",
              ns = ns,
              shiny::p("Protocol will be exported as a comma-separated file (csv) that can
              be loaded in any conventional software that allows the modification
              of text or spreadsheets. The protocol can also be reloaded in R via
              read.csv(file).")
            ),
            shiny::conditionalPanel(
              condition = "input.downloadFormat == 'yaml'",
              ns = ns,
              shiny::p("Protocol will be exported in the YAML Ain't Markup Language, a
              human-readable data serialization language that is common among
              many programming languages. YAML files can be read and edited with
              any text editor and loaded as lists into R via yaml::read_yaml(file).")
            ),
            shiny::br(),
            shiny::hr(),
            shiny::div(id = ns("missing"),style = "color: red",
                       shiny::textOutput(outputId = ns("missingtext"))),
            shiny::br(),
            # Button
            shiny::p(
              shiny::downloadButton(ns("downloadData"), "Download the selection option",
                                    class = "btn-primary"),
              shiny::downloadButton(ns("downloadEverything"), "Download everything",
                                    icon = shiny::icon("file-zipper"),
                                    class = "btn-secondary")
            )
          )
        ),
        shiny::tabPanel(
          title = "Render protocol table",
          icon = shiny::icon("list-alt",lib = "glyphicon"),
          bs4Dash::box(
            title = "Protocol",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            DT::DTOutput(outputId = ns("results_table"))
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
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Format the results table to a data.frame
    output$results_table <- DT::renderDT({
      DT::datatable(format_protocol(results, format = "data.frame", studyregiondummy=TRUE) |>
                      dplyr::select(-element),
                    rownames = FALSE,
                    filter = "top", selection = "none",
                    style = "auto",
                    editable = FALSE)
    })

    # --- #
    # Check for mandatory outputs and highlight them in text
    # shiny::observe(results, {
    #   miss <- check_protocol_mandatory(results)
    #   output$missingtext <- shiny::renderText({
    #     paste0("No entry found for mandatory fields:", miss)
    #   })
    #   print("test")
    #   shinyjs::toggle("downloadData")
    # })

    # Get output format
    oftype <- shiny::reactive({input$downloadFormat})

    # Make a download handler ----
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        # Compose output file
        paste0(
          "ODPSCP__",
          format(Sys.Date(), "%Y_%m_%d"),
          ".",
          oftype()
        )
      },
      content = function(file) {
        if(oftype() == "rData"){
          # Create outputs from results
          protocol <- format_protocol(results, format = "data.frame")
          save(protocol, file = file)
          # load("../../../Downloads/test.rData")
        } else if(oftype() == "csv"){
          # Create outputs from results
          protocol <- format_protocol(results, format = "data.frame")
          readr::write_csv(protocol, file = file)
          # read.csv("../../../Downloads/test.csv")
        } else if(oftype() == "yaml"){
          # Create outputs from results
          protocol <- format_protocol(results, format = "list")
          yaml::write_yaml(protocol, file = file)
          # yaml::read_yaml("../../../Downloads/test.yaml")
        } else if(oftype() == "docx"){
          # Create document from results, everything handled by function
          protocol <- format_protocol(results, format = "list")
          # saveRDS(rvtl(results), "test.rds")
          protocol_to_document(protocol, file = file, format = "docx")
        } else if(oftype() == "pdf"){
          # Create document from results
          protocol <- format_protocol(results, format = "list")
          protocol_to_document(protocol, file = file, format = "pdf")
        }
      }
    )

    # Download everything button
    output$downloadEverything <- shiny::downloadHandler(
      filename = function() {
        # Compose output file
        paste0(
          "ODPSCP__",
          format(Sys.Date(), "%Y_%m_%d"),
          ".zip"
        )
      },
      content = function(file) {
        # Create outputs from results
        shiny::showNotification("Preparing zipped outputs which can take a little while...",
                                duration = 3, type = "message")
        # First csv
        protocol <- format_protocol(results, format = "data.frame")
        ofname1 <- file.path(tempdir(), "ODPSCP_protocol.csv")
        readr::write_csv(protocol, file = ofname1)

        # Create document from results, everything handled by function
        protocol <- format_protocol(results, format = "list")
        ofname2 <- file.path(tempdir(), "ODPSCP_protocol.docx")
        protocol_to_document(protocol, file = ofname2,
                             format = "docx")

        # Create yaml from results
        protocol <- format_protocol(results, format = "list")
        ofname3 <- file.path(tempdir(), "ODPSCP_protocol.yaml")
        yaml::write_yaml(protocol, file = ofname3)

        # Zip everything together
        zip::zip(file,
                 files = c(ofname1, ofname2, ofname3),
                 mode = "cherry-pick")
      }
    )

  })
}

## To be copied in the UI
# mod_Export_ui("Export_1")

## To be copied in the server
# mod_Export_server("Export_1")
