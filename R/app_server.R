#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import waiter
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  sever::sever()

  # fake reload at start
  observeEvent(input$reload, {
    session$reload()
  })

  # Define overall protocol results
  # to modify a global variable use <<- instead of <- or =
  results <- reactiveValues()

  # title page --------------------------------------------------------------
  observeEvent(input$start_new_protocol, {
    updateTabItems(session,inputId = "sidebarmenu", selected = "Overview")
  })

  # Adding module server code
  mod_Home_server("Home_1", results)
  mod_Overview_server("Overview_1", results)
  mod_Design_server("Design_1", results)
  mod_Specification_server("Specification_1", results)
  mod_Context_server("Context_1", results)
  mod_Prioritization_server("Prioritization_1", results)

  # News
  mod_News_server("News_1")
  # Import/Export
  mod_Import_server("Import_1")
  mod_Export_server("Export_1", results)

}

