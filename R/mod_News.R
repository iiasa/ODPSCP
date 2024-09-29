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
       shiny::fluidPage(
         shiny::fluidRow(
           shiny::column(width = 2),
           shiny::column(width = 12,
                  bs4Dash::tabsetPanel(
                    id = ns("Tabset"),
                    type = "pills",
                    vertical = FALSE,
                    selected = "News",
                    # News panel
                    shiny::tabPanel(
                      title = "News",
                      shiny::br(),
                      bs4Dash::box(
                        title = "News and protocol updates",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsed = FALSE,
                        width = 12,
                        shiny::includeMarkdown("NEWS.md")
                      )
                    ),
                    # Rendered protocol
                    shiny::tabPanel(
                      title = "ODPSCP protocol",
                      shiny::br(),
                      bs4Dash::box(
                        title = "The protocol template",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsed = FALSE,
                        width = 12,
                        shiny::verbatimTextOutput(ns("current_protocol"))
                      )
                    )
                    # shiny::tabPanel(
                    #   title = "R Session Info",
                    #   shiny::br(),
                    #   bs4Dash::box(
                    #     title = "R Session Info",
                    #     status = "primary",
                    #     solidHeader = TRUE,
                    #     collapsed = FALSE,
                    #     width = 12,
                    #     shiny::verbatimTextOutput(ns("Rsession"))
                    #   )
                    # )
                  ) # End of tabset
           )
         )
       ) # End fluidpage
  ) # End of tabItem
}

#' News Server Functions
#'
#' @noRd
mod_News_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Print out the current Rsession
    # output$Rsession <- shiny::renderPrint(
    #   print(utils::sessionInfo())
    # )

    # Get the protocol for print
    ppath <- system.file("01_protocol.yaml",
                         package = "ODPSCP",
                         mustWork = TRUE)
    output$current_protocol <- shiny::renderPrint(
     readLines(ppath)
    )

  })
}

## To be copied in the UI
# mod_News_ui("News_1")

## To be copied in the server
# mod_News_server("News_1")
