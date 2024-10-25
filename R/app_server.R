#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  sever::sever()

  # --- #
  # Add bookmark button to top
  # NOTE: For URL see also https://stackoverflow.com/questions/58396680/how-to-extract-the-url-from-the-shiny-bookmark-button-and-create-my-own-action-b
  # shiny::enableBookmarking(store = "url")
  shiny::observeEvent(input$bookmark, {
    # session$doBookmark()
    # Use manuall bookmarking instead owing to the complexity
    shiny::showNotification("Save the current protocol as csv or yaml. Then import later...",
                            duration = 5, closeButton = TRUE, type = "message")
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Export")
  })

  # Save bookmark time too
  shiny::onBookmark(function(state) {
    savedTime <- as.character(Sys.time())
    cat("Last saved at", savedTime, "\n")
    # state is a mutable reference object, and we can add arbitrary values to
    # it.
    state$values$time <- savedTime
  })

  # On restore
  shiny::onRestore(function(state) {
    cat("Restoring from state bookmarked at", state$values$time, "\n")
  })
  # --- #

  # Enable shinylogs
  # shinylogs::read_rds_logs("logs")
  shinylogs::track_usage(what = c("session", "input", "output", "error"),
                         storage_mode = shinylogs::store_rds(path = "logs/")
                         )

  # fake reload at start
  shiny::observeEvent(input$reload, {
    session$reload()
  })

  # Define overall protocol results
  # to modify a global variable use <<- instead of <- or =
  results <- shiny::reactiveValues()

  # Bottom page buttons -------------------------------------------------------
  # Javascript to be rendered with shinyjs
  jscode <- "function() {document.body.scrollTop = 0;}"
  shiny::observeEvent(input$start_new_protocol, {
    bs4Dash::updateTabItems(session,
                            inputId = "sidebarmenu", selected = "Overview")
    # Jump to top
    shinyjs::runjs(jscode)
  })

  # Final observer events for continue buttons
  shiny::observeEvent(input$go_home, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Home")
    # Jump to top
    shinyjs::runjs(jscode)
  })
  shiny::observeEvent(input$go_overview, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Overview")
    # Jump to top
    shinyjs::runjs(jscode)
  })
  shiny::observeEvent(input$go_design, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Design")
    # Jump to top
    shinyjs::runjs(jscode)
  })
  shiny::observeEvent(input$go_specification, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Specification")
    # Jump to top
    shinyjs::runjs(jscode)
  })
  shiny::observeEvent(input$go_context, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Context")
    # Jump to top
    shinyjs::runjs(jscode)
  })
  shiny::observeEvent(input$go_prioritization, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Prioritization")
    # Jump to top
    shinyjs::runjs(jscode)
  })
  shiny::observeEvent(input$go_export, {
    bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Export")
    # Jump to top
    shinyjs::runjs(jscode)
  })

  # Initiate watching flags
  # gargoyle::init("import_overview", "import_design",
  #                "import_specification",
  #                "import_context",
  #                "import_prioritization", session = session)

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
  mod_Glossary_server("Glossary_1")
  # Import/Export
  mod_Import_server("Import_1", results, session)
  mod_Export_server("Export_1", results)

  # Automatically stop a Shiny app when closing the browser tab
  # NOTE: Disabled this as it seems to stop the instance across browsers
  # session$onSessionEnded(stopApp)
}

