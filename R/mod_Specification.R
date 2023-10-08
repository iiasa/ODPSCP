#' Specification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Specification_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "Specification")
}

#' Specification Server Functions
#'
#' @noRd
mod_Specification_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Specification_ui("Specification_1")

## To be copied in the server
# mod_Specification_server("Specification_1")
