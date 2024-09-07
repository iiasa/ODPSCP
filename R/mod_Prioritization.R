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
                           and targets in a single prioritization. Some make use of mathematical
                           optimization, others of heuristics or multi-criteria ranking approaches.
                           Some of the most commonly used SCP software solutions are listed below.
                           If the software for this study is not found in the list below,
                                  please select 'Other'."),
                         shinyWidgets::pickerInput(
                           inputId = ns("software"),
                           label = "Used software",
                           choices = c("","Zonation","Marxan",
                                       "prioritizr","prioriactions",
                                       "RestOptr","oppr", "ConsNet",
                                       "CAPTAIN","ROOT", "C-PLAN",
                                       "Custom","Other"),
                           options = list(
                             style = "btn-info")
                         ),
                         # Small
                         shiny::textOutput(outputId = ns("software_help"),inline = TRUE),
                         shiny::br(),
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
                         shiny::p("Enter a version number of the used software.
                                  Also provide any other information related to software
                                  (for example if a specific solver was used for mathematical programming such as Gurobi or Symphony)."),
                         shiny::textAreaInput(inputId = ns("versionnr"),
                                              label = "Provide a version number and other information related to the software.",
                                     placeholder = 'Enter free text here.',
                                     height = "45px", width = "100%", resize = "none")
                       ),
                     # Objective functions
                     shiny::br(),
                     bs4Dash::box(
                       title = "(Optional) Outcome identification",
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "secondary",
                       collapsible = FALSE,
                       shiny::p("In most prioritization exercises the way outcomes are identified is
                       usually determined by the used algorithm approach (e.g. heuristic, optimization) and
                       the question that is being asked for the decision variable."),
                       shiny::p("In this field we record - if known - how the software makes decisions,
                                e.g. what is being optimized or ranked and how. A typical example is the use
                                of minimum set problems (in Marxan and others) to identify those areas where
                                all feature targets are reached while minimizing a cost (area, opportunity cost, ...)"),
                       shiny::strong("References:"),
                       shiny::br("Arponen, A., Heikkinen, R. K., Thomas, C. D., & Moilanen, A. (2005). The value of biodiversity in reserve selection: representation, species weighting, and benefit functions. Conservation Biology, 19(6), 2009-2014."),
                       shiny::br("Schuster, R., Hanson, J. O., Strimas-Mackey, M., & Bennett, J. R. (2020). Exact integer linear programming solvers outperform simulated annealing for solving conservation planning problems. PeerJ, 8, e9258."),
                       shiny::textAreaInput(inputId = ns("outcomefunctions"), label = "How are prioritization outcomes identified?",
                                            placeholder = 'If known, please provide further detail.',
                                            height = "60px", width = "100%", resize = "vertical")
                     ),
                     shiny::br(),
                     bs4Dash::box(
                       title = "(Optional) Key parameters",
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
                         "Besides factors directly included in the prioritization,
                         it is also common to consider external or auxillary datasets,
                         selection frequency across multiple iterations, or
                         prioritization runs to identify a set of final 'priority' areas
                         or actions.",
                         "Here we record how the final priorities (those reported in the study) were obtained."),
                       shinyWidgets::pickerInput(
                         inputId = ns("identsolution"),
                         label = "Identification of priorities",
                         choices = c("","Single solution",
                                     "Selection frequency",
                                     "Overlays",
                                     "External indicator",
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
                         "An example would be summarizing the average amount of
                         a threatened species range covered, or overlap with other spatial layers."),
                       shiny::br(),
                       DT::DTOutput(outputId = ns("evalperformance")),
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

    # Get full protocol
    protocol <- load_protocol()$prioritization # Get all overview UI elements

    ids <- get_protocol_ids(group = "prioritization")
    shiny::observe({
      for(id in ids){
        if(id == "evalperformance"){
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
                 unit = character(0))
    )

    # --- #
    # Render some explanatory help for the software box
    shiny::observeEvent(input$software, {
      if(input$software!= ""){
        tt <- switch(input$software,
          "Zonation" = "Zonation is a spatial prioritization software that can be used to identify priority areas to support conservation planning, land use planning, ecological impact avoidance and other similar tasks. https://zonationteam.github.io/Zonation5/",
          "Marxan" = "Marxan is a suite of tools designed to help decision makers find good solutions to conservation planning problems. https://marxansolutions.org/",
          "prioritizr" = "The prioritizr R package uses mixed integer linear programming (MILP) techniques to provide a flexible interface for building and solving conservation planning problems. https://prioritizr.net/",
          "prioriactions" = "The prioriactions package allows you to create and solve conservation planning problems that involve multiple threats and actions using techniques of integer linear programming (ILP). https://prioriactions.github.io/prioriactions/",
          "RestOptr" = "The restoptr R package provides a flexible framework for ecological restoration planning using using mathematical optimization and landscape ecology theory. https://dimitri-justeau.github.io/restoptr/",
          "oppr" = "The oppr R package is decision support tool for prioritizing conservation projects in terms of funding and monitoring. https://prioritizr.github.io/oppr/",
          "ConsNet" = "ConsNet is a comprehensive software package for decision support for the design of conservation area networks to represent biodiversity features while incorporating a wide range of spatial and other criteria. https://doi.org/10.1111/j.1600-0587.2008.05721.x",
          "CAPTAIN" = "Software for conservation prioritization using reinforcement learning with the capacity to use most type of available data and resources. https://www.captain-project.net/",
          "ROOT" = "ROOT is tool designed to facilitate multi-objective landscape planning. It uses an optimization approach to help identify opportunities to maximize cobenefits and to identify compromise solutions where required. https://natcap.github.io/ROOT/",
          "C-PLAN" = "C-Plan is a conservation decision support software that links with GIS to map options for achieving explicit conservation targets. https://github.com/mattwatts/cplan",
          "Custom" = "This option is for planning studies that created their own customized software or code to achieve their solutions.",
          "Other" = "Any other software solution not listed among the options here (explain in fields below)."
        )
        output$software_help <- shiny::renderText(tt)
      } else {
        output$software_help <- shiny::renderText("")
      }
    })
    # --- #

    # Events for author table
    shiny::observeEvent(input$add_indicator, {
      new_data <- indicators() |> dplyr::add_row(
        data.frame(name = "EDIT ME", description = "EDIT ME",
                   unit = "EDIT ME")
      )
      indicators(new_data)
    })
    shiny::observeEvent(input$remove_indicator, {
      new_data <- indicators() |> dplyr::slice(-dplyr::n())
      indicators(new_data)
    })

    #output the datatable based on the dataframe (and make it editable)
    output$evalperformance <- DT::renderDT({
      DT::datatable(indicators(),rownames = FALSE,
                    filter = "none", selection = "none",
                    style = "auto",
                    editable = TRUE)
    })

    shiny::observeEvent(input$evalperformance_cell_edit, {
      info <- input$evalperformance_cell_edit
      modified_data <- indicators()
      modified_data[info$row, info$col+1] <- info$value
      indicators(modified_data)
    })

    # --- #
    # Add Tooltips for each element
    shiny::observeEvent(parentsession$input$help_switch,{
      # Enable tooltips if set
      add_protocol_help(parentsession, protocol, type = "popover")
    })
    # --- #
  })
}

## To be copied in the UI
# mod_Prioritization_ui("Prioritization_1")

## To be copied in the server
# mod_Prioritization_server("Prioritization_1")
