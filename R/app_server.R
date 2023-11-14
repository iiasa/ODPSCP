#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  sever::sever()

  # fake reload at start
  shiny::observeEvent(input$reload, {
    session$reload()
  })

  # Define overall protocol results
  # to modify a global variable use <<- instead of <- or =
  results <- shiny::reactiveValues()

  # Bottom page buttons -------------------------------------------------------
  shiny::observeEvent(input$start_new_protocol, {
    bs4Dash::updateTabItems(session,
                            inputId = "sidebarmenu", selected = "Overview")
  })

  # Final observer events for continue buttons
  shiny::observeEvent(input$go_home, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Home")
  })
  shiny::observeEvent(input$go_overview, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Overview")
  })
  shiny::observeEvent(input$go_design, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Design")
  })
  shiny::observeEvent(input$go_specification, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Specification")
  })
  shiny::observeEvent(input$go_context, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Context")
  })
  shiny::observeEvent(input$go_prioritization, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Prioritization")
  })
  shiny::observeEvent(input$go_export, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Export")
  })
  # ---------------------------------------------------------------------------

  # title page ----------------------------------------------------------------
  # Adding module server code
  mod_Home_server("Home_1", results, session)
  mod_Overview_server("Overview_1", results, session)
  mod_Design_server("Design_1", results, session)
  mod_Specification_server("Specification_1", results, session)
  mod_Context_server("Context_1", results, session)
  mod_Prioritization_server("Prioritization_1", results, session)

  # News
  mod_News_server("News_1")
  # Import/Export
  mod_Import_server("Import_1")
  mod_Export_server("Export_1", results)

  # Automatically stop a Shiny app when closing the browser tab
  # session$onSessionEnded(stopApp)
}

