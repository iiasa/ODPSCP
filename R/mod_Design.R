#' Design UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Design_ui <- function(id){
  ns <- NS(id)

  tabItem(
    tabName = "Design",
      fluidPage(
        fluidRow(
          column(width = 12,
                 box(
                   title = "What are the overall design criteria for the planning?",
                   closable = FALSE,
                   width = 12,
                   status = "primary",
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   "Systematic conservation planning can be conducted in a range
                   of different ways. In this section we record the principle
                   design elements of the study. These elements usually do not
                   consider methodological specifications of the planning, but
                   rather the conceptual understanding of the aims, purpose, and
                   framework underlying a study."
                 ),
                 hr()
          )
        ),
        # --- #
        # Entries #
        fluidRow(
          column(width = 2),
          column(width = 12,
                 # Purpose
                 box(
                   title = 'Aims and framing',
                   closable = FALSE,
                   width = 12,
                   solidHeader = TRUE,
                   status = "primary",
                   collapsible = TRUE,
                   # icon = icon("info"),
                   # Study aims
                   box(
                     title = "Study aims",
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "secondary",
                     collapsible = FALSE,
                     textAreaInput(inputId = ns("studyaim"), label = "",
                                   placeholder = 'A short 1-2 sentence description of what this study aims to achieve.',
                                   height = "45px", width = "100%", resize = "vertical")
                   ),
                   br(),
                   box(
                     title = 'Analytical Framework',
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "secondary",
                     collapsible = TRUE,
                     # icon = icon("info"),
                     # Study framework
                     p("Does the study follow an analytical framework, either explicitly
                       defined within the study or through a reference to previous
                       work?"),
                     shinyWidgets::pickerInput(
                       inputId = ns("studyframework"),
                       label = "Analytical Framework",
                       choices = c("None","Defined within study","Reference"),
                       multiple = FALSE
                     ),
                     conditionalPanel(
                       condition = "input.studyframework == 'Reference'",
                       ns = ns,
                       textAreaInput(inputId = ns("frameworkreference"), label = "Reference",
                                     placeholder = 'Provide a reference for the framework used.',
                                     height = "45px", width = "100%", resize = "none")
                     )
                   ),
                   br(),
                   # Theory of Change
                   box(
                     title = "(Optional) Theory of change",
                     closable = FALSE,
                     width = 12,
                     solidHeader = TRUE,
                     status = "secondary",
                     collapsible = FALSE,
                     p('Most SCP applications are applied rather than curiosity driven.
                       Here we ask whether the pathway to impact and
                       influencing outcomes is clear.'),
                     textAreaInput(inputId = ns("theoryofchange"), label = "",
                                   placeholder = 'Describe the theory of change if there is any.',
                                   height = "45px", width = "100%", resize = "vertical")
                   )
                ), # End study aims box

                # Study purpose overall box
                box(
                  title = 'Purpose of planning',
                  closable = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  # icon = icon("info"),
                  # Study purpose
                  box(
                    title = "Study purpose",
                    closable = FALSE,
                    width = 12,
                    solidHeader = TRUE,
                    status = "secondary",
                    collapsible = FALSE,
                    shinyWidgets::pickerInput(
                      inputId = ns("studypurpose"),
                      label = "Identify a primary purpose",
                      choices = c("Area-based expansion", "Management improvement",
                                  "Action-based planning", "Monitoring and evaluation",
                                  "Land-use allocation",
                                  "Other"),
                      multiple = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.studypurpose == 'Other'",
                      ns = ns,
                      textAreaInput(inputId = ns("otherpurpose"), label = "Other purpose",
                                    placeholder = 'Describe the purpose',
                                    height = "45px", width = "100%", resize = "none")
                    )
                  ),
                  br(),
                  # Multiple objective
                  box(
                    title = "Multiple objectives",
                    closable = FALSE,
                    width = 12,
                    solidHeader = TRUE,
                    status = "secondary",
                    collapsible = FALSE,
                    p("For a given purpose there can be often multiple, sometimes
                      competing objectives involved in the planning. For example,
                      if one would to identify management options that can maximize
                      both species and carbon storage."),
                    shinyWidgets::awesomeRadio(
                      inputId = ns("checkmultobj"),
                      label = "Were multiple objectives considered?",
                      choices = c("No", "Yes"),
                      selected = "No",
                      inline = FALSE,
                      checkbox = TRUE
                    ),
                    br(),
                    conditionalPanel(
                      condition = "input.checkmultobj == 'Yes'",
                      ns = ns,
                      textAreaInput(inputId = ns("multobj"), label = "Muliple objectives",
                                    placeholder = 'List them here',
                                    height = "45px", width = "100%", resize = "none")
                    )
                  )
                ),
                br(),

                # Study Engagement
                box(
                  title = 'Engagement of stakeholders',
                  closable = FALSE,
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  # icon = icon("info"),
                  # Stakeholders
                  p("To facilitate sucessful implementation it can be considered
                    important to involve stakeholders in the design and execution
                    of the planning exercise. There are multiple ways of doing so
                    and the fields below record these details."),
                  br(),
                  box(
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
                    conditionalPanel(
                      condition = "input.checkstakeholders == 'Yes'",
                      ns = ns,
                      br(),
                      box(
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
                          choices = c("Informed","Consulted","Involved",
                                      "Collaborated","Co-design"),
                          multiple = TRUE,
                          options = list(
                            title = "Engaged how?")
                        )
                      ),
                      br(),
                      box(
                        title = "Stakeholders",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shinyWidgets::pickerInput(
                          inputId = ns("stakeholdertype"),
                          label = "Who was engaged?",
                          choices = c("Policy makers (International)",
                                      "Policy makers (National)",
                                      "Scientists",
                                      "NGOs",
                                      "General Public",
                                      "Youth"),
                          multiple = TRUE
                        )
                      ),
                      br(),
                      box(
                        title = "Stakeholder engagement method",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        textAreaInput(inputId = ns("stakeholdermethod"), label = "Method of engagement",
                                      placeholder = 'Describe when and how stakeholders were engaged in the course of the project',
                                      height = "80px", width = "100%", resize = "both")
                      )
                    )
                  )
            ) # Box engagement
          )
        )
      ) # FluidPage
  ) # TabItem
}

#' Design Server Functions
#'
#' @noRd
mod_Design_server <- function(id, results){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Study design page --------------------------------------------------------------

    # Load all parameters in overview and add them to the reactive result container
    # Upon change
    ids <- get_protocol_ids(group = "design")
    observe({
      for(id in ids){
        results[[id]] <- input[[id]]
      }
    })

  })
}

## To be copied in the UI
# mod_Design_ui("Design_1")

## To be copied in the server
# mod_Design_server("Design_1")
