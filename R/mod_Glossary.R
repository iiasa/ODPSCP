#' Glossary UI Function
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
mod_Glossary_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "Glossary",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 12,
                        bs4Dash::box(
                          title = "Glossary",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsed = FALSE,
                          width = 12,
                          DT::DTOutput(outputId = ns('glossary_table'))
                        )
                      )
          )
    ) # End fluidpage
  ) # End of tabItem
}

#' Glossary Server Functions
#'
#' @noRd
mod_Glossary_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Get the protocol for print
    ppath <- system.file("glossary_table.csv",
                         package = "ODPSCP",
                         mustWork = TRUE)
    output$glossary_table <- DT::renderDataTable(
      utils::read.csv(ppath, sep = ",",header = TRUE) |>
        DT::datatable(filter = "none", rownames = FALSE,
                      editable = FALSE)
    )
  })
}

## To be copied in the UI
# mod_Glossary_ui("Glossary_1")

## To be copied in the server
# mod_Glossary_server("Glossary_1")
