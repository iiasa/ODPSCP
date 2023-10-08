#' Context UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Context_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "Context")
}

#' Context Server Functions
#'
#' @noRd
mod_Context_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Context_ui("Context_1")

## To be copied in the server
# mod_Context_server("Context_1")
