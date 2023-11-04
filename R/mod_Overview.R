#' Overview UI Function
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
mod_Overview_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(
    tabName = "Overview",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(width = 12,
               bs4Dash::box(
            title = "Provide an overview of the conducted work",
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
          shiny::hr()
        )
      ),
      # --- #
      # Entries #
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 12,
          # Study name group
          bs4Dash::box(
            title = "Study information",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsed = FALSE,
            collapsible = TRUE,
            # icon = icon("info"),
            shiny::textAreaInput(inputId = ns("studyname"), label = "Study name",
                          placeholder = 'What is the title of the conducted study?',
                          height = "45px", width = "100%", resize = "none"),
          # Authors
          bs4Dash::box(
            title = "Provide a list of authors:",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            status = "secondary",
            collapsible = TRUE,
            DT::DTOutput(outputId = ns("authors_table")),
            shiny::actionButton(inputId = ns("add_author"), label = "Add a new author row",
                                icon = shiny::icon("plus")),
            shiny::actionButton(inputId = ns("remove_author"), label = "Remove last author row",
                                icon = shiny::icon("minus")),
            shiny::pre("(Doubleclick on an added row to change the input values)"),
            shiny::br()
          ),
          # Corresponding author
          bs4Dash::box(
            title = "Corresponding Author",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            shiny::div("Who is the corresponding author?"),
            shiny::textAreaInput(inputId = ns("authoremail"), label = "",
                          placeholder = 'Email of the corresponding author',
                          height = "45px", width = "100%", resize = "none")
          ),
          # Link to study
          bs4Dash::box(
            title = "Link to study",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "gray",
            collapsible = FALSE,
            shiny::div("Has the study already been published? If so, pprove a reference"),
            shiny::textAreaInput(inputId = ns("studylink"), label = "",
                          placeholder = 'Link to the published study such as a DOI.',
                          height = "45px", width = "100%", resize = "none")
          )
          )
        ),
      ),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 12,
             # Next box with study location
             bs4Dash::box(
          title = "Overview of scale and extent of a study",
          closable = FALSE,
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsed = FALSE,
          collapsible = TRUE,
          # icon = icon("location"),
          shiny::p("Spatial planning can be conducted at a range of different spatial
                   and temporal scales, and realms. The fields below capture this
                   information."),
          # Scale
          bs4Dash::box(
            title = "Study scale",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            shiny::p("Local refers to a study at any given single site, National to planning
            at a country level, Regional for studies beyond single countries
            (e.g. bioregions), Continental for entire continents (e.g. Europe, Africa)
            and global for truly global studies."),
            shinyWidgets::pickerInput(
                inputId = ns("studyscale"),
                label = "At what scale was the study conducted?",
                choices = c("local", "national", "regional", "continental", "global"),
                options = list(
                  title = "Scale of the conducted study?")
              )
          ),
          # Study location
          bs4Dash::box(
            title = "Study location",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            shiny::div("Describe in a few sentences which location the study covers, for
                example the land- or seascape covered by study at local scale."),
            shiny::textAreaInput(inputId = ns("studylocation"), label = "Description",
                          placeholder = 'Qualitative description of the study location.',
                          height = "45px", width = "100%",resize = "vertical")
            ),
          # Study location
          bs4Dash::box(
            title = "Temporal coverage of the study",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            shiny::p("Define the temporal scale over which the planning applies.
              If outside the chosen scale, please provide details in the textbox."),
            shinyWidgets::sliderTextInput(
              inputId = ns("studytime"),
              label = "Choose a range:",
              choices = 1960:2100,
              grid = TRUE,
              selected = seq(2000,2020,1)
            ),
            shiny::br(),
            shiny::textAreaInput(inputId = ns("otherstudytime"), label = "(Optional) Custom coverage",
                          placeholder = 'Enter in case the range cannot be reflected with the slider.',
                          height = "45px", width = "100%",resize = "vertical")
          ),
          # Realm
          bs4Dash::box(
            title = "Study realm",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            shinyWidgets::checkboxGroupButtons(
              inputId = ns("studyrealm"),
              label = "In what realm(s) was the study conducted?",
              choices = c("Terrestrial (above ground)","Terrestrial (below ground)",
                          "Freshwater", "Marine", "Air"),
              size = "sm",
              justified = TRUE,
              checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
             )
            )
          )
        ) # Column end
      ),
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 12,
          # Data availability
          bs4Dash::box(
            title = "Data and code availability",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsed = FALSE,
            collapsible = TRUE,
            # icon = icon("magnifying-glass-chart"),
            shiny::p("This box records whether a study makes available the data - both for
            input and outputs - as well as the software code or analytical to
            reproduce the analysis."),
            # Link to input data
            bs4Dash::box(
                title = 'Are the used input data made available and if so where?',
                closable = FALSE,
                width = 12,
                solidHeader = TRUE,
                status = "gray",
                collapsible = FALSE,
                shinyWidgets::prettyToggle(
                  inputId = ns('inputavailability'),
                  label_on = "Yes",
                  icon_on = shiny::icon("check"),
                  status_on = "success",
                  status_off = "danger",
                  label_off = "No",
                  icon_off = shiny::icon("remove")
                ),
                shiny::textAreaInput(inputId = ns('inputdata'), label = "Input data",
                              placeholder = 'If applicable please enter a link to the data storage repository.',
                              height = "45px", width = "100%", resize = "none")

                ),
            shiny::br(),
            # Link to output data
            bs4Dash::box(
              title = 'Are the created outputs made openly available and if so where?',
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "gray",
              collapsible = FALSE,
              shinyWidgets::prettyToggle(
                inputId = ns('outputavailability'),
                label_on = "Yes",
                icon_on = shiny::icon("check"),
                status_on = "success",
                status_off = "danger",
                label_off = "No",
                icon_off = shiny::icon("remove")
              ),
              shiny::textAreaInput(inputId = ns('outputdata'), label = "Output data",
                            placeholder = 'If applicable please enter a link to the data storage repository.',
                            height = "45px", width = "100%", resize = "none")

            ),
            shiny::br(),
            # Link to code
            bs4Dash::box(
              title = 'Are the analytical steps to reproduce the results made available?',
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "gray",
              collapsible = FALSE,
              shinyWidgets::prettyToggle(
                inputId = ns('codeavailability'),
                label_on = "Yes",
                icon_on = shiny::icon("check"),
                status_on = "success",
                status_off = "danger",
                label_off = "No",
                icon_off = shiny::icon("remove")
              ),
              shiny::textAreaInput(inputId = ns('outputcode'), label = "Analysis code",
                            placeholder = 'If applicable please enter a link to the code storage repository.',
                            height = "45px", width = "100%", resize = "none")
            )
          ) # End of column
        ) # End of fluid row
      )

      # uiOutput("Overview_UI")

      # End of page button row
      # fluidRow(
      #   column(width = 3),
      #   column(width = 8,
      #          # Add reset button
      #          shinyWidgets::actionBttn(
      #            inputId = ns("reset"),
      #            label = "Clear all fields?",
      #            style = "simple",
      #            color = "danger",
      #            size = "md",
      #            block = FALSE,
      #            icon = icon("broom")
      #          ),
      #          # Add forward button
      #          shinyWidgets::actionBttn(
      #            inputId = ns("next_design"),
      #            label = "Continue with the design",
      #            style = "simple",
      #            color = "royal",
      #            size = "md",
      #            block = FALSE,
      #            icon = icon("arrow-right")
      #          )
      #   )
      # ) # End of fluid row for buttons
    ) # End of fluid page
  ) # End of tab

}

#' Overview Server Functions
#'
#' @importFrom shiny observe observeEvent
#' @noRd
mod_Overview_server <- function(id, results){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### Dynamic rendering of UI Elements ####
    # protocol <- load_protocol()$overview # Get all overview UI elements
    # output$Overview_UI = render_protocol("Overview", protocol)
    # -------------------------------------------

    # Study overview page --------------------------------------------------------------

    # Load all parameters in overview and add
    ids <- get_protocol_ids(group = "overview")
    shiny::observe({
      for(id in ids){
        if(id == "authors_table"){
          results[[id]] <- data.frame(authors()) |> asplit(MARGIN = 1)
        } else {
          results[[id]] <- input[[id]]
        }
      }
    })

    # Authors
    authors <- shiny::reactiveVal(
      data.frame(forename = character(0),
                 surename = character(0),
                 orcid = character(0))
    )

    # Events for author table
    shiny::observeEvent(input$add_author, {
      new_data <- authors() |> dplyr::add_row(
        data.frame(forename = "EDIT ME", surename = "EDIT ME", orcid = "EDIT ME")
      )
      authors(new_data)
    })
    shiny::observeEvent(input$remove_author, {
      new_data <- authors() |> dplyr::slice(-dplyr::n())
      authors(new_data)
    })

    #output the datatable based on the dataframe (and make it editable)
    output$authors_table <- DT::renderDT({
      DT::datatable(authors(),rownames = FALSE,
                    filter = "none", selection = "none",
                    style = "auto",
                    editable = TRUE)
    })

    shiny::observeEvent(input$authors_table_cell_edit, {
      info <- input$authors_table_cell_edit
      modified_data <- authors()
      modified_data[info$row, info$col+1] <- info$value
      authors(modified_data)
    })

    # ----- #
    # Events for hiding data input boxes
    shiny::observeEvent(input$inputavailability, {
      shinyjs::toggle("inputdata")
    })
    shiny::observeEvent(input$outputavailability, {
      shinyjs::toggle("outputdata")
    })
    shiny::observeEvent(input$codeavailability, {
      shinyjs::toggle("outputcode")
    })
    # ----- #

    # Bottom page buttons --------------------------------------------------------------
    # Final observe event to continue
    shiny::observeEvent(input$next_design, {
      bs4Dash::updateTabItems(session, inputId = ns("sidebarmenu"), selected = "Design")
    })

    # Clear all
    shiny::observeEvent(input$reset, {
      shinyWidgets::ask_confirmation(
        inputId = ns("confirm_reset"),
        text = tags$b(
          shiny::icon("broom"),
          "Do you really want to clear all entries?",
          style = "color: #FA5858;"
        ),
        btn_labels = c("Cancel", "Clear"),
        btn_colors = c("#00BFFF", "#FE2E2E"),
        html = TRUE
      )
    })
    # TODO
    clearall <- shiny::renderPrint(input$confirm_reset)
  })
}

## To be copied in the UI
# mod_Overview_ui("Overview_1")

## To be copied in the server
# mod_Overview_server("Overview_1")
