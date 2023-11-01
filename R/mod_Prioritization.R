#' Prioritization UI Function
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
mod_Prioritization_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(tabName = "Prioritization",
          fluidPage(
            fluidRow(
              column(width = 12,
                     bs4Dash::box(
                       title = 'Prioritization',
                       closable = FALSE,
                       width = 12,
                       solidHeader = FALSE,
                       status = "primary",
                       collapsible = FALSE,
                       p("With prioritization we usually refer to the process of taking
                         the various datasets and parameters defined earlier and
                         identifying 'solutions' that might be spatial, spatial-temporal
                         or non-spatial in nature. This section elaborates on the
                         process of prioritization and specifically the algorithms used
                         to generate such solutions for the planning problem.")
                     )
                )
              ),
              hr(),
              fluidRow(
                column(width = 2),
                column(width = 12,
                       # Software description
                       bs4Dash::box(
                         title = 'Used Software',
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         collapsible = TRUE,
                         p("There are multiple existing types of software that
                           allow users to integrate various features, constraints
                           and targets in a single prioritization. The most
                           commonly used SCP software solutions are listed below."),
                         shinyWidgets::pickerInput(
                           inputId = ns("software"),
                           label = "Used software",
                           choices = c("","Zonation","Marxan",
                                       "prioritizr","prioriactions",
                                       "RestOptr","oppr",
                                       "CAPTAIN","ROOT",
                                       "Custom","Other"),
                         options = list(
                           style = "btn-info")
                       ),
                       #TODO: Show some explanation / reference for each method
                       # upon selection.
                       conditionalPanel(
                         condition = "input.software == 'Custom' || input.software == 'Other'",
                         ns = ns,
                         textAreaInput(inputId = ns("othersoftware"), label = "Other",
                                       placeholder = 'Describe the custom or other algorithm.',
                                       height = "45px", width = "100%", resize = "none")
                       ),
                       # Version number
                       br(),
                       conditionalPanel(
                         condition = "input.software != ''",
                         ns = ns,
                         textAreaInput(inputId = ns("versionnr"), label = "Provide a version number",
                                     placeholder = 'Enter a version nr for the used algorithm',
                                     height = "45px", width = "100%", resize = "none")
                       ),
                     br(),
                     bs4Dash::box(
                       title = "Key parameters",
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "secondary",
                       collapsible = FALSE,
                       br(),
                       textAreaInput(inputId = ns("parameters"), label = "Specific parameters used in the prioritization",
                                     placeholder = 'Records any specific parameters related to the prioritization that would be useful to know.',
                                     height = "60px", width = "100%", resize = "vertical")
                       ),
                     br(),
                     # Identification of final priorities
                     bs4Dash::box(
                       title = "Identification of final priorities",
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "secondary",
                       collapsible = FALSE,
                       p("Not always is there a single solution to the prioritization process or
                         where only single prioritizations run. Here we record how the
                         final priorities (those reported in the study) were obtained."),
                       shinyWidgets::pickerInput(
                         inputId = ns("identsolution"),
                         label = "Identification of solutions",
                         choices = c("","Budgets reached or costs exceeded",
                                     "Targets achieved",
                                     "External indicator",
                                     "Other")
                       ),
                       conditionalPanel(
                         condition = "input.identsolution == 'Other'",
                         ns = ns,
                         textAreaInput(inputId = ns("otheridentification"), label = "",
                                       placeholder = 'Explain how final solutions were obtained',
                                       height = "45px", width = "100%", resize = "vertical")
                       )
                      )
                     )
              )
            ), # Fluidrow
            fluidRow(
              column(width = 2),
              column(width = 12,
                     bs4Dash::box(
                       title = 'Performance evaluation',
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = TRUE,
                       p("A performance evaluation determines the value or overall
                         benefits of a given prioritization output in terms of
                         chosen representative indicators or values."),
                       DT::DTOutput(outputId = ns("perfidenticators")),
                       actionButton(inputId = ns("add_indicator"), label = "Add a new indicator", icon = icon("plus")),
                       actionButton(inputId = ns("remove_indicator"), label = "Remove last indicator", icon = icon("minus")),
                       pre("(Doubleclick on an added row to change the input values)"),
                       br(),
                       br(),
                       # Any other performance evaluation conducted?
                       bs4Dash::box(
                         title = "Other performance evaluation",
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "secondary",
                         collapsible = FALSE,
                         br(),
                         textAreaInput(inputId = ns("otherperformance"),
                                       label = "Was there any other form of evaluation of the prioritization?",
                                       placeholder = 'If applicable, elaborate',
                                       height = "60px", width = "100%", resize = "vertical")
                       )
                     )
              ) # Column end
            ) # Fluid row
          ) # Fluidpage
        )
}

#' Planning Server Functions
#'
#' @importFrom shiny observe observeEvent
#' @noRd
mod_Prioritization_server <- function(id, results){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ids <- get_protocol_ids(group = "prioritization")
    observe({
      for(id in ids){
        if(id == "perfidenticators"){
          results[[id]] <- data.frame(indicators()) |> asplit(MARGIN = 1)
        } else {
          results[[id]] <- input[[id]]
        }
      }
    })

    # List for performance indicators
    indicators <- reactiveVal(
      data.frame(name = character(0),
                 description = character(0),
                 unit = character(0),
                 reference = character(0))
    )

    # Events for author table
    observeEvent(input$add_indicator, {
      new_data <- indicators() |> dplyr::add_row(
        data.frame(name = "EDIT ME", description = "EDIT ME",
                   unit = "EDIT ME", reference = "EDIT ME")
      )
      indicators(new_data)
    })
    observeEvent(input$remove_indicator, {
      new_data <- indicators() |> dplyr::slice(-dplyr::n())
      indicators(new_data)
    })

    #output the datatable based on the dataframe (and make it editable)
    output$perfidenticators <- DT::renderDT({
      DT::datatable(indicators(),rownames = FALSE,
                    filter = "none", selection = "none",
                    style = "auto",
                    editable = TRUE)
    })

    observeEvent(input$perfidenticators_cell_edit, {
      info <- input$perfidenticators_cell_edit
      modified_data <- indicators()
      modified_data[info$row, info$col+1] <- info$value
      indicators(modified_data)
    })

  })
}

## To be copied in the UI
# mod_Prioritization_ui("Prioritization_1")

## To be copied in the server
# mod_Prioritization_server("Prioritization_1")
