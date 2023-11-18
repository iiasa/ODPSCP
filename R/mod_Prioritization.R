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
          shiny::fluidPage(
            shiny::fluidRow(
              shiny::column(width = 12,
                     bs4Dash::box(
                       title = 'Prioritization',
                       closable = FALSE,
                       width = 12,
                       solidHeader = FALSE,
                       status = "primary",
                       collapsible = FALSE,
                       shiny::p("With prioritization we usually refer to the process of taking
                         the various datasets and parameters defined earlier and
                         identifying 'solutions' that might be spatial, spatial-temporal
                         or non-spatial in nature. This section elaborates on the
                         process of prioritization and specifically the algorithms used
                         to generate such solutions for the planning problem.")
                     )
                )
              ),
              shiny::hr(),
              shiny::fluidRow(
                shiny::column(width = 2),
                shiny::column(width = 12,
                       # Software description
                       bs4Dash::box(
                         title = 'Used Software',
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         collapsible = TRUE,
                         shiny::p("There are multiple existing types of software that
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
                       shiny::conditionalPanel(
                         condition = "input.software == 'Custom' || input.software == 'Other'",
                         ns = ns,
                         shiny::textAreaInput(inputId = ns("othersoftware"), label = "Other",
                                       placeholder = 'Describe the custom or other algorithm.',
                                       height = "45px", width = "100%", resize = "none")
                       ),
                       # Version number
                       shiny::br(),
                       shiny::conditionalPanel(
                         condition = "input.software != ''",
                         ns = ns,
                         shiny::textAreaInput(inputId = ns("versionnr"), label = "Provide a version number",
                                     placeholder = 'Enter a version nr for the used algorithm',
                                     height = "45px", width = "100%", resize = "none")
                       ),
                     # Objective functions
                     shiny::br(),
                     bs4Dash::box(
                       title = "Benefit functions",
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "secondary",
                       collapsible = FALSE,
                       shiny::p("In many optimizations benefits can accrue in varying ways, for example
                                through maximizing the targets achieved. If known or specific to the study,
                                provide information on benefit function used."),
                       shiny::p("Benefit functions in prioritizations are for example those minimize marginal
                                losses from cell removal, minimize an average shortfall or maximize the number
                                of targets (a constraint) achieved.

                                Reference: Arponen, A., Heikkinen, R. K., Thomas, C. D., & Moilanen, A. (2005). The value of biodiversity in reserve selection: representation, species weighting, and benefit functions. Conservation Biology, 19(6), 2009-2014."),
                       shiny::textAreaInput(inputId = ns("benefitfunctions"), label = "What is being optimized and how?",
                                            placeholder = 'If known, please provide further detail.',
                                            height = "60px", width = "100%", resize = "vertical")
                     ),
                     shiny::br(),
                     bs4Dash::box(
                       title = "Key parameters",
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "secondary",
                       collapsible = FALSE,
                       shiny::br(),
                       shiny::textAreaInput(inputId = ns("parameters"), label = "Specific parameters used in the prioritization",
                                     placeholder = 'Records any specific parameters related to the prioritization that would be useful to know.',
                                     height = "60px", width = "100%", resize = "vertical")
                       ),
                     shiny::br(),
                     # Identification of final priorities
                     bs4Dash::box(
                       title = "Identification of final priorities",
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "secondary",
                       collapsible = FALSE,
                       shiny::p("Not always is there a single solution to the prioritization process or
                         where only single prioritizations run.",
                         "For example, the selection frequency across multiple iterations
                         or prioritization runs could be used to identify the set of
                         final 'priority' areas.",
                         "Here we record how the final priorities (those reported in the study) were obtained."),
                       shinyWidgets::pickerInput(
                         inputId = ns("identsolution"),
                         label = "Identification of solutions",
                         choices = c("","Budgets reached or costs exceeded",
                                     "Targets achieved",
                                     "Selection frequency",
                                     "External indicator",
                                     "Overlays",
                                     "Other")
                       ),
                       shiny::conditionalPanel(
                         condition = "input.identsolution == 'Other'",
                         ns = ns,
                         shiny::textAreaInput(inputId = ns("otheridentification"), label = "",
                                       placeholder = 'Explain how final solutions were obtained',
                                       height = "45px", width = "100%", resize = "vertical")
                       )
                      )
                     )
              )
            ), # Fluidrow
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(width = 12,
                     bs4Dash::box(
                       title = 'Performance evaluation',
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = TRUE,
                       shiny::p("A performance evaluation determines the value or overall
                         benefits of a given prioritization output in terms of
                         chosen representative indicators or values.",
                         "An example would be for example summarizing the average amount of
                         a threatened species range covered, or overlap with other spatial layers."),
                       DT::DTOutput(outputId = ns("perfidenticators")),
                       shiny::actionButton(inputId = ns("add_indicator"), label = "Add a new indicator",
                                           icon = shiny::icon("plus")),
                       shiny::actionButton(inputId = ns("remove_indicator"), label = "Remove last indicator",
                                           icon = shiny::icon("minus")),
                       shiny::pre("(Doubleclick on an added row to change the input values)"),
                       shiny::br(),
                       shiny::br(),
                       # Any other performance evaluation conducted?
                       bs4Dash::box(
                         title = "Other performance evaluation",
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "secondary",
                         collapsible = FALSE,
                         shiny::br(),
                         shiny::textAreaInput(inputId = ns("otherperformance"),
                                       label = "Was there any other form of evaluation of the prioritization?",
                                       placeholder = 'If applicable, elaborate',
                                       height = "60px", width = "100%", resize = "vertical")
                       )
                     )
              ) # Column end
            ), # Fluid row

            # End of page button row
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(width = 8,
                            # Add backward button
                            shinyWidgets::actionBttn(
                              inputId = "go_context",
                              label = "Back to Context",
                              style = "simple",
                              color = "primary",
                              size = "sm",
                              block = FALSE,
                              icon = shiny::icon("arrow-left")
                            ),
                            # Add forward button
                            shinyWidgets::actionBttn(
                              inputId = "go_export",
                              label = "Export the protocol",
                              style = "simple",
                              color = "success",
                              size = "sm",
                              block = FALSE,
                              icon = shiny::icon("file-export")
                            )
              )
            ) # Fluidrow button end
          ) # Fluidpage
        )
}

#' Planning Server Functions
#'
#' @importFrom shiny observe observeEvent
#' @noRd
mod_Prioritization_server <- function(id, results, parentsession){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    ids <- get_protocol_ids(group = "prioritization")
    shiny::observe({
      for(id in ids){
        if(id == "perfidenticators"){
          results[[id]] <- data.frame(indicators()) |> asplit(MARGIN = 1)
        } else {
          results[[id]] <- input[[id]]
        }
      }
    })

    # List for performance indicators
    indicators <- shiny::reactiveVal(
      data.frame(name = character(0),
                 description = character(0),
                 unit = character(0),
                 reference = character(0))
    )

    # Events for author table
    shiny::observeEvent(input$add_indicator, {
      new_data <- indicators() |> dplyr::add_row(
        data.frame(name = "EDIT ME", description = "EDIT ME",
                   unit = "EDIT ME", reference = "EDIT ME")
      )
      indicators(new_data)
    })
    shiny::observeEvent(input$remove_indicator, {
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

    shiny::observeEvent(input$perfidenticators_cell_edit, {
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
