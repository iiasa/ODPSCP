#' Design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Design_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "Design")
}

#' Design Server Functions
#'
#' @noRd
mod_Design_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Design_ui("Design_1")

## To be copied in the server
# mod_Design_server("Design_1")
