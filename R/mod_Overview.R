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

  tabItem(
    tabName = "Overview",
    fluidPage(
      fluidRow(
        column(width = 12,
          box(
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
          hr()
        )
      ),
      # --- #
      # Entries #
      fluidRow(
        column(width = 2),
        column(width = 12,
          # Study name group
          box(
            title = "Study information",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsed = FALSE,
            collapsible = TRUE,
            icon = icon("info"),
            textAreaInput(inputId = ns("studyname"), label = "Study name",
                          placeholder = 'What is the title of the conducted study?',
                          height = "45px", width = "100%", resize = "none"),
          # Authors
          box(
            title = "Provide a list of authors:",
            closable = FALSE,
            width = 12,
            solidHeader = T,
            status = "secondary",
            collapsible = TRUE,
            DT::DTOutput(outputId = ns("authors_table")),
            actionButton(inputId = ns("add_author"), label = "Add a new author row", icon = icon("plus")),
            actionButton(inputId = ns("remove_author"), label = "Remove last author row", icon = icon("minus")),
            br()
          ),
          # Corresponding author
          box(
            title = "Corresponding Author",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            textAreaInput(inputId = ns("authoremail"), label = "",
                          placeholder = 'Email of the corresponding author',
                          height = "45px", width = "100%", resize = "none")
          ),
          # Link to study
          box(
            title = "(optional) Link to study",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "gray",
            collapsible = FALSE,
            textAreaInput(inputId = ns("studylink"), label = "",
                          placeholder = 'Link to the published study such as DOI.',
                          height = "45px", width = "100%", resize = "none")
          )
          )
        ),
      ),
      fluidRow(
        column(width = 2),
        column(width = 12,
             # Next box with study location
        box(
          title = "Study location",
          closable = FALSE,
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsed = FALSE,
          collapsible = TRUE,
          icon = icon("location"),
          "Spatial planning can be conducted at a range of different scales and realms
          and this field aims to provide the various options.",
          pre(),
          # Scale
          box(
            title = "Study scale",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            "Local refers to a study at any given single site, National to planning
            at a country level, Regional for studies beyond single countries
            (e.g. bioregions), Continental for entire continents (e.g. Europe, Africa)
            and global for truly global studies.",
            pre(),
            shinyWidgets::pickerInput(
                inputId = ns("studyscale"),
                label = "At what scale was the study conducted?",
                choices = c("local", "national", "regional", "continental", "global"),
                options = list(
                  title = "Scale of the conducted study?")
              )
          ),
          # Study location
          box(
            title = "Study location",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            textAreaInput(inputId = ns("studylocation"), label = "",
                          placeholder = 'Qualitative description of the study location.',
                          height = "45px", width = "100%",resize = "vertical")
            ),
          # Realm
          box(
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
              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
             )
            )
          )
        ) # Column end
      ),
      fluidRow(
        column(width = 2),
        column(width = 12,
          # Data availability
          box(
            title = "Data and code availability",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsed = FALSE,
            collapsible = TRUE,
            icon = icon("magnifying-glass-chart"),
            "This box records whether a study makes available the data - both for
            input and outputs - as well as the software code or analytical to
            reproduce the analysis.",
            pre(),
            # Link to input data
              box(
                title = 'Are the used input data made available and if so where?',
                closable = FALSE,
                width = 12,
                solidHeader = TRUE,
                status = "gray",
                collapsible = FALSE,
                  # shinyWidgets::prettyToggle(
                  #   inputId = ns('inputavailability'),
                  #   label_on = "Yes",
                  #   icon_on = icon("check"),
                  #   status_on = "success",
                  #   status_off = "danger",
                  #   label_off = "No",
                  #   icon_off = icon("remove")
                  # ),
                  textAreaInput(inputId = ns('inputdata'), label = "",
                                placeholder = 'If applicable please enter a link to the data storage repository.',
                                height = "45px", width = "100%", resize = "none")

                ),
            pre(),
            # Link to output data
            box(
              title = 'Are the created outputs made openly available and if so where?',
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "gray",
              collapsible = FALSE,
              textAreaInput(inputId = ns('outputdata'), label = "",
                            placeholder = 'If applicable please enter a link to the data storage repository.',
                            height = "45px", width = "100%", resize = "none")

            ),
            pre(),
            # Link to code
            box(
              title = 'Are the analytical steps to reproduce the results made available?',
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "gray",
              collapsible = FALSE,
              textAreaInput(inputId = ns('outputcode'), label = "",
                            placeholder = 'If applicable please enter a link to the code storage repository.',
                            height = "45px", width = "100%", resize = "none")

            )
          ) # End of column
        ) # End of fluid row
      ),

      # uiOutput("Overview_UI")

      # End of page button row
      fluidRow(
        column(width = 3),
        column(width = 8,
               # Add reset button
               shinyWidgets::actionBttn(
                 inputId = ns("reset"),
                 label = "Clear all fields?",
                 style = "simple",
                 color = "danger",
                 size = "md",
                 block = FALSE,
                 icon = icon("broom")
               ),
               # Add forward button
               shinyWidgets::actionBttn(
                 inputId = ns("next_design"),
                 label = "Continue with the design",
                 style = "simple",
                 color = "royal",
                 size = "md",
                 block = FALSE,
                 icon = icon("arrow-right")
               )
        )
      ) # End of fluid row for buttons
    ) # End of fluid page
  ) # End of tab

}

#' Overview Server Functions
#'
#' @noRd
mod_Overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### Dynamic rendering of UI Elements ####
    # protocol <- load_protocol()$overview # Get all overview UI elements
    # output$Overview_UI = render_protocol("Overview", protocol)
    # -------------------------------------------

    # Study overview page --------------------------------------------------------------
    # Authors
    authors <- reactiveVal(
      data.frame(forename = character(0),
                 surename = character(0),
                 orcid = character(0))
    )

    # Events for author table
    observeEvent(input$add_author, {
      new_data <- authors() |> dplyr::add_row(
        data.frame(forename = "EDIT ME", surename = "EDIT ME", orcid = "EDIT ME")
      )
      authors(new_data)
    })
    observeEvent(input$remove_author, {
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

    observeEvent(input$authors_table_cell_edit, {
      info <- input$authors_table_cell_edit
      modified_data <- authors()
      modified_data[info$row, info$col+1] <- info$value
      authors(modified_data)
    })

    # Final observe event to continue
    observeEvent(input$next_design, {
      updateTabItems(session, "sidebarmenu", selected = "Design")
    })

    # Clear all
    observeEvent(input$reset, {
      shinyWidgets::ask_confirmation(
        inputId = ns("confirm_reset"),
        text = tags$b(
          icon("broom"),
          "Do you really want to clear all entries?",
          style = "color: #FA5858;"
        ),
        btn_labels = c("Cancel", "Clear"),
        btn_colors = c("#00BFFF", "#FE2E2E"),
        html = TRUE
      )
    })
    # TODO
    clearall <- renderPrint(input$confirm_reset)

  })
}

## To be copied in the UI
# mod_Overview_ui("Overview_1")

## To be copied in the server
# mod_Overview_server("Overview_1")
