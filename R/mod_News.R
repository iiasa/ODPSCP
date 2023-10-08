#' News UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_News_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "News",
          fluidRow(
            column(4,
                   includeMarkdown("NEWS.md")
            )
          )
  ) # End of tabItem
}

#' News Server Functions
#'
#' @noRd
mod_News_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_News_ui("News_1")

## To be copied in the server
# mod_News_server("News_1")
