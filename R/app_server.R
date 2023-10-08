#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import waiter
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # fake reload at start
  observeEvent(input$reload, {
    session$reload()
  })

  # title page --------------------------------------------------------------
  observeEvent(input$start_new_protocol, {
    updateTabItems(session, "sidebarmenu", selected = "Overview")
  })

  # Adding module server code
  mod_Home_server("Home_1")
  mod_Overview_server("Overview_1")
  mod_Design_server("Design_1")
  mod_Specification_server("Specification_1")
  mod_Context_server("Context_1")
  mod_Planning_server("Planning_1")

  # News
  mod_News_server("News_1")
  # Import/Export
  mod_Import_server("Import_1")
  mod_Export_server("Export_1")
}

