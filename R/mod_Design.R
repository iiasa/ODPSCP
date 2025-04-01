#' Design UI Function
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
mod_Design_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "Design",
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(width = 12,
                 bs4Dash::box(
                   title = "What are the overall design criteria for the planning?",
                   closable = FALSE,
                   width = 12,
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   "Systematic conservation planning can be conducted in a range
                   of different ways. In the 'Design' section we record the principle
                   design elements of the study. These elements usually do not
                   consider methodological specifications of the planning, but
                   rather the conceptual understanding of the aims, purpose, and
                   any framework underlying a study."
                 ),
                 shiny::hr()
          )
        ),
        # --- #
        # Entries #
        shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(width = 12,
                 # Purpose
                 bs4Dash::box(
                   title = 'Aims and framing',
                   closable = FALSE,
                   width = 12,
                   solidHeader = TRUE,
                   status = "primary",
                   collapsible = TRUE,
                   # icon = icon("info"),
                   # Study aims
                   bs4Dash::box(
                     title = "Study aims",
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "secondary",
                     collapsible = FALSE,
                     shiny::div("A short 1-2 sentence description of what the study aimed
                         to achieve. "),
                     shiny::textAreaInput(inputId = ns("studyaim"), label = "",
                                   placeholder = 'Describe study aims...',
                                   height = "60px", width = "100%", resize = "vertical")
                   ),
                   shiny::br(),
                   bs4Dash::box(
                     title = '(Optional) Analytical Framework',
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "secondary",
                     collapsible = TRUE,
                     # icon = icon("info"),
                     # Study framework
                     shiny::p("Does the study follow an analytical framework, either explicitly
                       defined within the study or through a reference to previous
                       work? This could for example also be a specific planning protocol or established
                       approaches such as structure decision making or adaptive management."),
                     shiny::p("Example framework references:"),
                     shiny::p("Pressey, R. L., & Bottrill, M. C. (2009). Approaches to landscape-and seascape-scale conservation planning: convergence, contrasts and challenges. Oryx, 43(4), 464-475. DOI: https://doi.org/10.1017/S0030605309990500"),
                     shiny::p("Alvarez-Romero, J. G., Adams, V. M., Pressey, R. L., Douglas, M., Dale, A. P., Auge, A. A., ... & Perdrisat, I. (2015). Integrated cross-realm planning: A decision-makers' perspective. Biological Conservation, 191, 799-808. DOI: https://doi.org/10.1016/j.biocon.2015.07.003"),
                     shiny::p("Niemiec, R. M., Gruby, R., Quartuch, M., Cavaliere, C. T., Teel, T. L., Crooks, K., ... & Manfredo, M. (2021). Integrating social science into conservation planning. Biological Conservation, 262, 109298. DOI: https://doi.org/10.1016/j.biocon.2021.109298"),
                     shinyWidgets::pickerInput(
                       inputId = ns("studyframework"),
                       label = "Analytical Framework",
                       choices = c("None","Defined within study","Reference"),
                       selected = "Defined within study",
                       multiple = FALSE
                     ),
                     shiny::conditionalPanel(
                       condition = "input.studyframework == 'Reference'",
                       ns = ns,
                       shiny::textAreaInput(inputId = ns("frameworkreference"), label = "Reference",
                                     placeholder = 'Provide a reference for the framework used.',
                                     height = "45px", width = "100%", resize = "none")
                     )
                   ),
                   shiny::br(),
                   # Theory of Change
                   bs4Dash::box(
                     title = "(Optional) Theory of change",
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "secondary",
                     collapsible = FALSE,
                     shiny::p('Most SCP applications are applied rather than curiosity driven. However an applied
                              focus does not necessarily mean that the work will be implemented and the pathway to
                              impact is clear. A theory of change is a apriori process that maps the relationship between
                              a long-term goal of a planning objective and the necessary steps required to implement it.'),
                     shinyWidgets::awesomeRadio(
                       inputId = ns("theoryofchange"),
                       label = "Did the study use a theory of change concept?",
                       choices = c("No", "Yes"),
                       selected = "No",
                       inline = FALSE,
                       checkbox = TRUE
                     ),
                     shiny::br(),
                     shiny::conditionalPanel(
                       condition = "input.theoryofchange == 'Yes'",
                       ns = ns,
                       shiny::textAreaInput(inputId = ns("theoryofchange_text"), label = "",
                                            placeholder = 'Describe the theory of change used.',
                                            height = "45px", width = "100%", resize = "vertical")
                     )
                   )
                ), # End study aims box
                # Study purpose overall box
                bs4Dash::box(
                  title = 'Purpose of planning',
                  closable = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  # icon = icon("info"),
                  # Study purpose
                  bs4Dash::box(
                    title = "Study purpose",
                    closable = FALSE,
                    width = 12,
                    solidHeader = TRUE,
                    status = "secondary",
                    collapsible = FALSE,
                    shiny::p("As primary purpose we refer to the overall aim of a study
                      such as the identification of areas to be placed under
                      conservation management (e.g. Protected areas)"),
                    shiny::selectizeInput(inputId = ns("studypurpose"),
                                          label = "Identify or add primary purpose(s)",
                                          choices = c("","Area-based allocation",
                                                      "Management evaluation",
                                                      "Management improvement",
                                                      "Action-based planning",
                                                      "Monitoring and evaluation"
                                                      ),
                                          multiple = TRUE,
                                          options = list(create = TRUE,
                                                         placeholder = "Choose from list, or type and click to add a new option."))
                  ),
                  shiny::br(),
                  # Multiple objective
                  bs4Dash::box(
                    title = "Multiple objectives",
                    closable = FALSE,
                    width = 12,
                    solidHeader = TRUE,
                    status = "secondary",
                    collapsible = FALSE,
                    shiny::p("For a given purpose and objective function there can be often multiple, sometimes
                      competing objectives involved in the planning. For example,
                      if one would to identify management options that can consider
                      both species and carbon storage as features by altering their weights."),
                    shinyWidgets::awesomeRadio(
                      inputId = ns("checkmultobj"),
                      label = "Had the planning multiple objectives?",
                      choices = c("No", "Yes"),
                      selected = "No",
                      inline = FALSE,
                      checkbox = TRUE
                    ),
                    shiny::br(),
                    shiny::conditionalPanel(
                      condition = "input.checkmultobj == 'Yes'",
                      ns = ns,
                      shiny::textAreaInput(inputId = ns("multobj"), label = "Muliple objectives",
                                    placeholder = 'List them here',
                                    height = "45px", width = "100%", resize = "none")
                    )
                  ),
                  shiny::br(),
                  bs4Dash::box(
                    title = "Scenarios or planning variants",
                    closable = FALSE,
                    width = 12,
                    solidHeader = TRUE,
                    status = "secondary",
                    collapsible = FALSE,
                    shiny::p("Often the output of a planning exercise is not a single
                      prioritization, but multiple each with different assumptions,
                      parameters or input data. Examples include planning approaches
                      that account for various climate scenarios or assumptions regarding
                      constraints. Please record whether there multiple scenarios
                      or variants have been explored and how"),
                    shiny::br(),
                    shiny::helpText("What does not qualify here: 'Scenarios' that
                                    are essentially sensitivity checks or parameter
                                    calibrations. Only select Yes if multiple outputs are
                                    presented in the work."),
                    shinyWidgets::awesomeRadio(
                      inputId = ns("checkscenarios"),
                      label = "Are there multiple variants or scenarios explored in the planning?",
                      choices = c("No", "Yes"),
                      selected = "No",
                      inline = FALSE,
                      checkbox = TRUE
                    ),
                    shiny::br(),
                    shiny::conditionalPanel(
                      condition = "input.checkscenarios == 'Yes'",
                      ns = ns,
                      shiny::textAreaInput(inputId = ns("planningscenarios"), label = "Explain",
                                    placeholder = 'Describe the planning scenarios or variants.',
                                    height = "60px", width = "100%", resize = "vertical")
                    )
                  )
                ),
                # Study Engagement
                bs4Dash::box(
                  title = 'Engagement of stakeholders',
                  closable = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  # icon = icon("info"),
                  # Stakeholders
                  shiny::p("To facilitate sucessful implementation it can be considered
                    important to involve stakeholders in the design and execution
                    of the planning exercise. There are multiple ways of doing so
                    and the fields below record these details."),
                  shiny::br(),
                  bs4Dash::box(
                    title = "Stakeholder engaged",
                    closable = FALSE,
                    width = 12,
                    solidHeader = TRUE,
                    status = "secondary",
                    collapsible = FALSE,
                    shinyWidgets::awesomeRadio(
                      inputId = ns("checkstakeholders"),
                      label = "Were Stakeholders engaged in any way?",
                      choices = c("No", "Yes"),
                      selected = "No",
                      inline = FALSE,
                      checkbox = TRUE
                    ),
                    # Details on type of engagement
                    shiny::conditionalPanel(
                      condition = "input.checkstakeholders == 'Yes'",
                      ns = ns,
                      shiny::br(),
                      bs4Dash::box(
                        title = "Type of engagement",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        # Ask for the specific types
                        shinyWidgets::pickerInput(
                          inputId = ns("stakeholderint"),
                          label = "Types of stakeholder interactions:",
                          choices = c("Informed","Consulted","Dialogue","Involved",
                                      "Collaborated","Co-design"),
                          multiple = TRUE,
                          options = list(
                            title = "Engaged how?")
                        )
                      ),
                      shiny::br(),
                      bs4Dash::box(
                        title = "Stakeholders",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shiny::p("Select or add broad groups of who was engaged.
                                 Note that co-authors are generally not considered stakeholders
                                 unless co-authorship is given due to co-design of the
                                 whole planning study."),
                        shiny::selectizeInput(inputId = ns("stakeholdertype"),
                                              label = "Who was engaged?",
                                              choices = c("Policy makers (International)",
                                                          "Policy makers (National)",
                                                          "Domain experts (Individuals)",
                                                          "Scientists",
                                                          "NGO (International)",
                                                          "NGO (National)",
                                                          "Resource users",
                                                          "General Public",
                                                          "Property owners"),
                                              multiple = TRUE,
                                              options = list(create = TRUE,
                                                             placeholder = "Choose from list, or type and click to add new option.")
                                              )
                      ),
                      shiny::br(),
                      bs4Dash::box(
                        title = "Stakeholder engagement method",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shiny::textAreaInput(inputId = ns("stakeholdermethod"), label = "Method of engagement",
                                      placeholder = 'Describe when and how stakeholders were engaged in the course of the project',
                                      height = "80px", width = "100%", resize = "both")
                      )
                    )
                  )
            ), # Box engagement
            # Added with version 0.5 - Box on monitoring
            bs4Dash::box(
              title = 'Monitoring and evaluation',
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              # icon = icon("info"),
              # Stakeholders
              shiny::p("Monitoring and evaluation is a key part of systematic conservation planning. Here we ask
                       whether - following the implementation of the study - procedures are in place to monitor
                       its outcome and what steps have been undertaken to evaluate the effectiveness of the planning."),
              shiny::br(),
              bs4Dash::box(
                title = "Has a monitoring and evaluation plan be developed?",
                closable = FALSE,
                width = 12,
                solidHeader = TRUE,
                status = "secondary",
                collapsible = FALSE,
                shiny::br(),
                shiny::p("Such a plan could for example include the monitoring of the implementation and its effectiveness,
                         or the mentioning of a specific methodology to evaluate the planning objectives and targets."),
                shinyWidgets::awesomeRadio(
                  inputId = ns("checkmonitoringeval"),
                  label = "Has a monitoring and evaluation plan be developed?",
                  choices = c("No", "Yes"),
                  selected = "No",
                  inline = FALSE,
                  checkbox = TRUE
                ),
                shiny::br(),
                shiny::conditionalPanel(
                  condition = "input.checkmonitoringeval == 'Yes'",
                  ns = ns,
                  shiny::textAreaInput(inputId = ns("checkmonitoringeval_detail"), label = "Explain",
                                       placeholder = 'Describe what aspects of the project are monitored and how the expected results or goals are evaluated.',
                                       height = "60px", width = "100%", resize = "vertical")
                )
              ) # End of targetd box on monitoring and evaluation plan
            )
          )
        ),
        # End of page button row
        shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(width = 8,
             # Add backward button
             shinyWidgets::actionBttn(
               inputId = "go_overview",
               label = "Back to Overview",
               style = "simple",
               color = "primary",
               size = "sm",
               block = FALSE,
               icon = shiny::icon("arrow-left")
             ),
             # Add forward button
             shinyWidgets::actionBttn(
               inputId = "go_specification",
               label = "Continue with specification",
               style = "simple",
               color = "primary",
               size = "sm",
               block = FALSE,
               icon = shiny::icon("arrow-right")
             )
          )
        )
      ) # FluidPage
  ) # TabItem
}

#' Design Server Functions
#'
#' @importFrom shiny observe
#' @noRd
mod_Design_server <- function(id, results, parentsession){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Study design page --------------------------------------------------------------
    # Get protocol
    protocol <- load_protocol()$design # Get all overview UI elements

    # Load all parameters and add them to the reactive result container
    # Upon change
    ids <- get_protocol_ids(group = "design")
    observe({
      for(id in ids){
        results[[id]] <- input[[id]]
      }
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
# mod_Design_ui("Design_1")

## To be copied in the server
# mod_Design_server("Design_1")
