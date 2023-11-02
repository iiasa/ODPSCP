#' Import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash insertTab tabItem tabItems
#' @importFrom shiny actionButton tabsetPanel column
mod_Import_ui <- function(id){
  ns <- NS(id)

  # TODO:
  # Import existing Marxan / Zonation / Prioritizr configuration files?
  bs4Dash::tabItem(tabName = "Import",
          # https://shiny.posit.co/r/gallery/widgets/file-upload/
          shiny::fluidPage(
            bs4Dash::box(
              title = "Not yet implemented",
              closable = FALSE,
              width = 12,status = "danger",
              solidHeader = TRUE,
              collapsible = FALSE,
              "To be added..."
            )
            # fileInput(
            #   inputId = ns("file1"),
            #   width = "100%",
            #   label = "Load your database",
            #   accept = c(
            #     "text/csv",
            #     "text/comma-separated-values",
            #     "text/tab-separated-values",
            #     "text/plain",
            #     ".csv",
            #     ".tsv", "xlsx"
            #   )
            # ),
            # helpText("Default max. file size is 100MB"),
            # box(
            #   title = tagList(shiny::icon("upload"), "Source"),
            #   solidHeader = FALSE,
            #   status = "success",
            #   maximizable = F,
            #   closable = F,
            #   width = 12,
            #   shinyWidgets::radioGroupButtons(
            #     inputId = ns("Id004"),
            #     choices = c("Example Data" = 1, "Import Data" = 2, "BrAPI" = 3),
            #     status = "success",
            #     selected = 1
            #   ),
            #   shiny::conditionalPanel(
            #     condition = "input.Id004==1",
            #     h6("Use example data"),
            #     ns = ns
            #   ),
            #   shiny::conditionalPanel(
            #     condition = "input.Id004==2",
            #     h6("Import external data preferably csv/txt files."),
            #     ns = ns
            #   )
            # )
          ) # End fluid page
  )

}

#' Import Server Functions
#'
#' @noRd
mod_Import_server <- function(id){
 shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_Import_ui("Import_1")

## To be copied in the server
# mod_Import_server("Import_1")
