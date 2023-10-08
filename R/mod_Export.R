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
    tabName = "Export"
  )
  # https://shiny.posit.co/r/gallery/widgets/file-download/
}

#' Export Server Functions
#'
#' @noRd
mod_Export_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Export_ui("Export_1")

## To be copied in the server
# mod_Export_server("Export_1")
