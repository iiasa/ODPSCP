#' Planning UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Planning_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "Planning")
}

#' Planning Server Functions
#'
#' @noRd
mod_Planning_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Planning_ui("Planning_1")

## To be copied in the server
# mod_Planning_server("Planning_1")
