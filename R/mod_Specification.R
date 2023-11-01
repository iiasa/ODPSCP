#' Specification UI Function
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
mod_Specification_ui <- function(id){
  ns <- NS(id)

  bs4Dash::tabItem(tabName = "Specification",
          fluidPage(
            fluidRow(
              column(width = 12,
                     bs4Dash::box(
                       title = "Specification",
                       closable = FALSE,
                       width = 12,
                       status = "primary",
                       solidHeader = FALSE,
                       collapsible = FALSE,
                       p("Under the protocol entry 'Specification' we list all
                         the elements, features and datasets that are being used
                         in the planning. Basic information on their broad categorization,
                         type and origin are recorded.")
                     )
              )
            ),
            hr(),
            # Row for planning units
             fluidRow(
                column(width = 2),
                column(width = 12,
                       # Planning units
                       bs4Dash::box(
                         title = 'Planning units and scale',
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         collapsible = TRUE,
                         p("The principal elements of a SCP application are generally
                           called 'Planning units'. They can be for example based on a
                           gridded Raster layer or any spatial organization such as
                           a polygon. There could also be no planning units for SCP
                           applications that are non-spatial."),
                         br(),
                         # Planning unit type
                         bs4Dash::box(
                           title = "Planning unit type",
                           closable = FALSE,
                           width = 12,
                           solidHeader = TRUE,
                           status = "secondary",
                           collapsible = FALSE,
                           shinyWidgets::pickerInput(
                             inputId = ns("pu_type"),
                             label = "Type of planning unit",
                             choices = c(
                               "None", "Gridded", "Vector (Point, Line, Polygon)",
                               "Other"
                             ),
                             multiple = FALSE
                           ),
                           conditionalPanel(
                             condition = "input.pu_type == 'Other'",
                             ns = ns,
                             textAreaInput(inputId = ns("othertypes"), label = "Other type",
                                           placeholder = 'Define here',
                                           height = "45px", width = "100%", resize = "none")
                           )
                          ),
                         br(),
                         bs4Dash::box(
                           title = "What was the spatial grain of planning?",
                           closable = FALSE,
                           width = 12,
                           solidHeader = TRUE,
                           status = "secondary",
                           collapsible = FALSE,
                           p("Briefly describe the spatial grain of planning if applicable, for
                             example whether a planning unit was homogeneous in size."),
                           textAreaInput(inputId = ns("pu_grainspace"), label = "",
                                         placeholder = 'Describe the grain of planning units if applicable.',
                                         height = "45px", width = "100%", resize = "none")
                         ),
                         br(),
                         bs4Dash::box(
                           title = "Planning unit costs",
                           closable = FALSE,
                           width = 12,
                           solidHeader = TRUE,
                           status = "secondary",
                           collapsible = FALSE,
                           shinyWidgets::pickerInput(
                             inputId = ns("pu_checkcosts"),
                             label = "Where there any costs of selecting a planning unit?",
                             choices = c("No", "Area only", "Yes"),
                             multiple = FALSE
                           ),
                           conditionalPanel(
                             condition = "input.pu_checkcosts == 'Yes'",
                             ns = ns,
                             textAreaInput(inputId = ns("pu_costs"), label = "Describe process of creating costs",
                                           placeholder = 'Provide some detail on how costs were defined or created.',
                                           height = "45px", width = "100%", resize = "none")
                           )
                         )
                    )
                ) # End column
              ), # End row
            fluidRow(
              column(width = 2),
              column(width = 12,
                     # Planning units
                     bs4Dash::box(
                       title = 'Zones and specific groups',
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = TRUE,
                       p("Not all planning units or planning objectives might
                         receive the same attention or are considered equally.
                         Here we record whether certain area or themes were
                         exclusively considered, included or excluded."),
                       br(),
                       # Ecosystem specifics
                       bs4Dash::box(
                         title = "Was there any ecosystem specificity?",
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "secondary",
                         collapsible = FALSE,
                         p("Planning can be conducted on all land or sea within a given
                           region, but it can also be specific to certain ecosystems
                           or land-use types, such as for example forests.",
                           strong("Check No if all available land or sea was considered.")),
                          shinyWidgets::awesomeRadio(
                            inputId = ns("checkecosystem"),
                            label = "Was there any ecosystem specificity?",
                            choices = c("No", "Yes"),
                            selected = "No",
                            inline = FALSE,
                            checkbox = TRUE
                          ),
                      br(),
                      conditionalPanel(
                        condition = "input.checkecosystem == 'Yes'",
                        ns = ns,
                        textAreaInput(inputId = ns("specificecosystem"), label = "Which ecosystems were considered?",
                                      placeholder = 'List them here',
                                      height = "45px", width = "100%", resize = "none")
                        )
                      ),
                      br(),
                      # Zones
                      bs4Dash::box(
                        title = "Where any zones used for the planning?",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        p("Planning can be structured by considering multiple
                          management or land-use objectives through zones.
                          Here we describe them if those are set."),
                        shinyWidgets::awesomeRadio(
                          inputId = ns("checkzones"),
                          label = "Were different management zones considered?",
                          choices = c("No", "Yes"),
                          selected = "No",
                          inline = FALSE,
                          checkbox = TRUE
                        ),
                        br(),
                        conditionalPanel(
                          condition = "input.checkzones == 'Yes'",
                          ns = ns,
                          textAreaInput(inputId = ns("specificzones"), label = "Zones used?",
                                        placeholder = 'Describe the zones used in the planning.',
                                        height = "45px", width = "100%", resize = "none")
                        )
                      ),
                      br(),
                      # Areas included or excluded
                      bs4Dash::box(
                        title = "Inclusion or exclusions?",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        p("Any areas or actions that were included or excluded by default?"),
                        shinyWidgets::pickerInput(
                          inputId = ns("inclusionexclusion"),
                          label = "Select:",
                          choices = c("None", "Areas or Action included", "Areas or Action excluded"),
                          selected = "None",
                          multiple = TRUE
                        ),
                        conditionalPanel(
                          condition = "input.inclusionexclusion != 'None'",
                          ns = ns,
                          textAreaInput(inputId = ns("defaultareas"), label = "Describe inclusions or exclusions",
                                        placeholder = 'Describe in wording the specific areas or actions considered...',
                                        height = "60px", width = "100%", resize = "vertical")
                        )
                      )
                     )
               ) # Column end
             ), # Fluid row end
             br(),
             # Entries #
             fluidRow(
               column(width = 2),
               column(width = 12,
                      # Features
                      bs4Dash::box(
                        title = 'Feature types and description',
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        collapsible = TRUE,
                        p("What types of features are included in the spatial planning?
                          As features we do traditionally describe all (spatial)
                          information that enters the prioritization with the aim of
                          accruing benefits. Examples include estimates of the distribution
                          of species or Nature Contributions to people (NCPs)."),
                        br(),
                        # Feature types
                        bs4Dash::box(
                          title = "Feature types",
                          closable = FALSE,
                          width = 12,
                          solidHeader = TRUE,
                          status = "secondary",
                          collapsible = FALSE,
                          shinyWidgets::pickerInput(
                              inputId = ns("featuretypes"),
                              label = "Select/deselect all options",
                              choices = c(
                                "Species (distributions)",
                                "Species (abundance)",
                                "Species (traits)",
                                "Ecosystems or Habitats",
                                "Land-cover and Land-use",
                                "Socio-economic",
                                "Political",
                                "Biophysical",
                                "Nature contribtions to people (supply)",
                                "Nature contribtions to people (demand)",
                                "Other"
                              ),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE
                            ),
                            br(),
                            # Any other types?
                            textAreaInput(inputId = ns("otherfeaturetype"), label = "Other types",
                                          placeholder = 'Any other broad feature types included? Describe',
                                          height = "45px", width = "100%", resize = "none")
                        ),
                        br(),
                        # Aggregated features
                        bs4Dash::box(
                          title = "Where any features types aggregated before use in the planning?",
                          closable = FALSE,
                          width = 12,
                          solidHeader = TRUE,
                          status = "secondary",
                          collapsible = FALSE,
                          p("An example is the use of Overall Species richness as
                            aggregated sum of species distributions in the planning"),
                          shinyWidgets::prettyToggle(
                            inputId = ns('checkaggregated'),
                            label_on = "Yes",
                            icon_on = icon("check"),
                            status_on = "success",
                            status_off = "danger",
                            label_off = "No",
                            icon_off = icon("remove")
                          ),
                          conditionalPanel(
                            condition = "input.checkaggregated",
                            ns = ns,
                            textAreaInput(inputId = ns('featureaggregated'), label = "Aggregation of feature types",
                                        placeholder = 'If applicable please describe which and how.',
                                        height = "45px", width = "100%", resize = "none")
                          )
                        ),
                        br(),
                        # List the features
                        bs4Dash::box(
                          title = "Provide a list of all features:",
                          closable = FALSE,
                          width = 12,
                          solidHeader = T,
                          status = "secondary",
                          collapsible = TRUE,
                          DT::DTOutput(outputId = ns("featurelist")),
                          actionButton(inputId = ns("add_feature"), label = "Add a new feature row", icon = icon("plus")),
                          actionButton(inputId = ns("remove_feature"), label = "Remove last feature row", icon = icon("minus")),
                          fileInput(inputId = ns('load_feature'),label = 'Alternatively upload a feature/group list:',
                                    accept = c('csv', 'comma-separated-values','.csv', 'tsv', '.tsv')),
                          p("(Doubleclick on an added row to change the input values)")
                        ),
                        br(),
                        # How were features created?
                        bs4Dash::box(
                          title = "How were features created?",
                          closable = FALSE,
                          width = 12,
                          solidHeader = TRUE,
                          status = "secondary",
                          collapsible = FALSE,
                          p('Commonly input features in the planning were created
                            through separate processes, such as through qualitative
                            data gathering or ecological modelling. Here we briefly
                            describe how input features were created.'),
                          textAreaInput(inputId = ns("featureorigin"), label = "",
                                           placeholder = 'Describe the origin of the input features',
                                           height = "60px", width = "100%", resize = "vertical")
                        )
                      )
                  ) # Fluid column end
                ), # Fluidrow end
            br()

          )
  ) # End tab
}

#' Specification Server Functions
#'
#' @importFrom shiny observe observeEvent
#' @noRd
mod_Specification_server <- function(id, results){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Load all parameters in specification and add
    ids <- get_protocol_ids(group = "specification")
    observe({
      for(id in ids){
        if(id == "feature_table"){
          results[[id]] <- data.frame(feature_table()) |> asplit(MARGIN = 1)
        } else {
          results[[id]] <- input[[id]]
        }
      }
    })

    # --- #
    # Define the features list
    feature_table <- shiny::reactiveVal(
      data.frame(name = character(0),
                 group = character(0))
    )

    # Events for author table
    observeEvent(input$add_feature, {
      new_data <- feature_table() |> dplyr::add_row(
        data.frame(name = "My species", group = "Species distribution")
      )
      feature_table(new_data)
    })
    observeEvent(input$remove_feature, {
      new_data <- feature_table() |> dplyr::slice(-dplyr::n())
      feature_table(new_data)
    })

    #output the datatable based on the dataframe (and make it editable)
    output$featurelist <- DT::renderDT({
      DT::datatable(feature_table(),rownames = FALSE,
                    filter = "none", selection = "none",
                    style = "auto",
                    editable = TRUE)
    })

    # Manual edit
    observeEvent(input$feature_table_cell_edit, {
      info <- input$feature_table_cell_edit
      modified_data <- feature_table()
      modified_data[info$row, info$col+1] <- info$value
      feature_table(modified_data)
    })

    # Load an external file
    loadedfeatures <- shiny::reactive({
      if(is.null(input$load_feature)){
        return(NULL)
      }
      data <- readr::read_csv(input$load_feature$datapath)
      # FIXME: Could do some graceful error checks here?
      feature_table(data)
    })
    # --- #

    # Events for hiding data input boxes
    observeEvent(input$featuretypes, {
      if('Other' %in% input$featuretypes){
        shinyjs::show("otherfeaturetype")
      }
    })

  })
}

## To be copied in the UI
# mod_Specification_ui("Specification_1")

## To be copied in the server
# mod_Specification_server("Specification_1")
