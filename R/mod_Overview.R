#' Overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Overview_ui <- function(id){
  ns <- NS(id)

  # cg species --------------------------------------------------------------
  tabItem(
    tabName = "Overview",

    fluidPage(
      box(
        title = "Overview over the study",
        closable = FALSE,
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        "Let's start with a new reporting protocol. In this first step we describe
        all the properties of the conducted planning study. The entries below
        intend to both uniquely identify the study, provide necessary information
        on the availability of code or data and broadly categorizes any and all studies
        based on the listed properties.
        "
      ),

      shinyWidgets::checkboxGroupButtons(
        inputId = "Id058",
        label = "Label",
        choices = c("A",
                    "B", "C", "D"),
        justified = TRUE,
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"))
      ),
      uiOutput("Overview_UI")
    )
  )

}

#' Overview Server Functions
#'
#' @noRd
mod_Overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### Dynamic rendering of UI Elements ####
    protocol <- load_protocol()$overview # Get all overview UI elements
    # output$Overview_UI = render_protocol("Overview", protocol)

  })
}

## To be copied in the UI
# mod_Overview_ui("Overview_1")

## To be copied in the server
# mod_Overview_server("Overview_1")
