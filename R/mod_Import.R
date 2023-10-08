#' Import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Import_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "Import")
  # https://shiny.posit.co/r/gallery/widgets/file-upload/
}

#' Import Server Functions
#'
#' @noRd
mod_Import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Import_ui("Import_1")

## To be copied in the server
# mod_Import_server("Import_1")
