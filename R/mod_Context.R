#' Context UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Context_ui <- function(id){
  ns <- NS(id)

  tabItem(tabName = "Context",
          fluidPage(
            fluidRow(
              column(width = 12,
                     box(
                       title = "Important contextual information with regards to the planning.",
                       closable = FALSE,
                       width = 12,
                       status = "primary",
                       solidHeader = FALSE,
                       collapsible = FALSE,
                       "Besides input datasets and parameters,
                       there are usually quite a few other important choices
                       made by analysts with regards to what the planning should
                       incorporate or consider. Examples include the consideration
                       of connectivity or other constraints that limit the primary
                       objective of the planning."
                     ),
                   hr()
              )
            ),
            #---#
            # Start with the context
           fluidRow(
             column(width = 2),
             column(width = 12,
                    # Purpose
                    box(
                      title = 'Selection criteria',
                      closable = FALSE,
                      width = 12,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsible = TRUE,
                      # icon = icon("info"),
                      # Decision variable
                      box(
                        title = "Decision variable",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shiny::selectizeInput(inputId = ns("decisiontype"),
                                       label = "What are the decision or output variables?",
                                       choices = c("","Binary allocation",
                                                   "Proportional share",
                                                   "Semi-continious share",
                                                   "Importance ranking"),
                                       multiple = FALSE,
                                       options = list(create = TRUE,
                                                      placeholder = "Choose from list, or type and click to add new option"))
                      ),
                      br(),
                      # Time context
                      box(
                        title = "Temporal considerations",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        p("For example, was the planning conducted in a way that
                          considers future states or conditions?"),
                        shiny::selectizeInput(inputId = ns("timedecision"),
                                              label = "Which temporal conditions does the planning consider?",
                                              choices = c("Past conditions",
                                                          "Current conditions",
                                                          "Future conditions",
                                                          "Fully dynamic"),
                                              multiple = TRUE,
                                              options = list(create = FALSE,
                                                             placeholder = "Choose single or multiple from list"))
                      ),
                      br(),
                      # Connectivity
                      box(
                        title = "Aspects of connectivity considered?",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shinyWidgets::awesomeRadio(
                          inputId = ns("checkconnectivity"),
                          label = "Was connectivity somehow considered in the planning?",
                          choices = c("No", "Yes"),
                          selected = "No",
                          inline = FALSE,
                          checkbox = TRUE
                        ),
                        conditionalPanel(
                          condition = "input.checkconnectivity == 'Yes'",
                          ns = ns,
                          br(),
                          # Options for connectivity
                          box(
                            title = "Connectivity planning",
                            closable = FALSE,
                            width = 12,
                            solidHeader = TRUE,
                            status = "secondary",
                            collapsible = FALSE,
                            shiny::selectizeInput(inputId = ns("connectivityplan"),
                                                  label = "How was connectivity considered?",
                                                  choices = c("","Boundary penalty",
                                                              "Neighbour constraint",
                                                              "Size constraint",
                                                              "Structural connectivity",
                                                              "Functional connectivity"),
                                                  multiple = TRUE,
                                                  options = list(create = TRUE,
                                                                 placeholder = "Choose from list, or type and click to add new option"))
                          ),
                          br(),
                          box(
                            title = "Other connectivity details",
                            closable = FALSE,
                            width = 12,
                            solidHeader = TRUE,
                            status = "secondary",
                            collapsible = FALSE,
                            p("Any other methodological detail on how connectivity was considered in the planning."),
                            textAreaInput(inputId = ns("otherconnectivity"), label = "Connectivity method",
                                          placeholder = 'Describe the methodology',
                                          height = "60px", width = "100%", resize = "none")
                          )
                          )
                        ),
                      br(),
                      # Any other constraints
                      box(
                        title = "Other constraints",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        p("Any other constraints used in the planning?"),
                        textAreaInput(inputId = ns("otherconstraints"), label = "Constraints",
                                      placeholder = 'If any, shortly describe how they are defined.',
                                      height = "60px", width = "100%", resize = "vertical")
                        )
                      )
                     ) # Column end
                   ), # Fluid row end
           # --- #
           # Further entries on features #
           fluidRow(
             column(width = 2),
             column(width = 12,
                    # Purpose
                    box(
                      title = 'Feature contexts',
                      closable = FALSE,
                      width = 12,
                      solidHeader = TRUE,
                      status = "primary",
                      collapsible = TRUE,
                      p("In many situations the features used in the planning
                        might also have their own parametrizations and options.
                        A common example is for instance the setting of targets
                        as a specific constraint."),
                      # Feature targets
                      box(
                        title = "Feature targets",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shiny::selectizeInput(inputId = ns("contexttargets"),
                                              label = "Were targets used for features?",
                                              choices = c("","None",
                                                          "Flat (e.g. 30 percent)",
                                                          "Log-linear",
                                                          "Favourable Reference value"),
                                              multiple = FALSE,
                                              options = list(create = TRUE,
                                                             placeholder = "Choose from list, or type and click to add new option"))
                      ),
                      br(),
                      # Feature weights
                      box(
                        title = "Feature weights",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        p("Were differential weights assigned to some features,
                          such as for example giving threatened species a higher
                          weight in the planning? If so, describe here:"),
                      shinyWidgets::awesomeRadio(
                        inputId = ns("checkfeatureweights"),
                        label = "Were weights modified for features?",
                        choices = c("No", "Yes"),
                        selected = "No",
                        inline = FALSE,
                        checkbox = TRUE
                      ),
                      br(),
                      conditionalPanel(
                        condition = "input.checkfeatureweights == 'Yes'",
                        ns = ns,
                        textAreaInput(inputId = ns("featureweightsdetails"), label = "Feature weights",
                                      placeholder = 'Describe how feature weights have been defined or set.',
                                      height = "60px", width = "100%", resize = "none")
                      )
                      )
                    )
                )
              ) # End Fluid row
            ) # End Fluidpage
          )
}

#' Context Server Functions
#'
#' @noRd
mod_Context_server <- function(id, results){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Get all parameters
    ids <- get_protocol_ids(group = "context")
    observe({
      for(id in ids){
        results[[id]] <- input[[id]]
      }
    })

  })
}

## To be copied in the UI
# mod_Context_ui("Context_1")

## To be copied in the server
# mod_Context_server("Context_1")
