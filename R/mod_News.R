#' News UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash insertTab tabItem tabItems
#' @importFrom shiny actionButton tabsetPanel column
mod_News_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "News",
       fluidPage(
         fluidRow(
           column(width = 2),
           column(width = 12,
                  bs4Dash::tabsetPanel(
                    id = ns("Tabset"),
                    type = "pills",
                    vertical = FALSE,
                    selected = "News",
                    # News panel
                    shiny::tabPanel(
                      title = "News",
                      br(),
                      box(
                        title = "News and protocol updates",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsed = FALSE,
                        width = 12,
                        includeMarkdown("NEWS.md")
                      )
                    ),
                    # Rendered protocol
                    shiny::tabPanel(
                      title = "ODPSCP protocol",
                      br(),
                      box(
                        title = "The protocol template",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsed = FALSE,
                        width = 12,
                        shiny::htmlOutput(ns("current_protocol"))
                      )
                    ),
                    shiny::tabPanel(
                      title = "R Session Info",
                      br(),
                      box(
                        title = "R Session Info",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsed = FALSE,
                        width = 12,
                        verbatimTextOutput(ns("Rsession"))
                      )
                    )
                  )
           )
         )
       ) # End fluidpage
  ) # End of tabItem
}

#' News Server Functions
#'
#' @noRd
mod_News_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Print out the current Rsession
    output$Rsession <- renderPrint(
      print(utils::sessionInfo())
    )

    # Get the protocol for print
    ppath <- system.file("01_protocol.yaml",
                         package = "ODPSCP",
                         mustWork = TRUE)
    output$current_protocol <- renderPrint(
      print( readLines(ppath) )
    )

  })
}

## To be copied in the UI
# mod_News_ui("News_1")

## To be copied in the server
# mod_News_server("News_1")
