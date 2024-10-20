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
          shiny::fluidPage(
            shiny::fluidRow(
              shiny::column(width = 12,
                     bs4Dash::box(
                       title = "Specification",
                       closable = FALSE,
                       width = 12,
                       status = "primary",
                       solidHeader = FALSE,
                       collapsible = FALSE,
                       shiny::p("Under the protocol entry 'Specification' we list all
                         the elements, features and datasets that are being used
                         in the planning. Basic information on their broad categorization,
                         type and origin are recorded.
                         It records what type of information is included in the planning,
                         not how individual information sources are used (for that see context).")
                     )
              )
            ),
            shiny::hr(),
            # Row for planning units
             shiny::fluidRow(
                shiny::column(width = 2),
                shiny::column(width = 12,
                       # Planning units
                       bs4Dash::box(
                         title = 'Planning units and scale',
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "primary",
                         collapsible = TRUE,
                         shiny::p("The principal elements of a SCP application are generally
                           called 'Planning units' (but see Glossary). They can be for example based on a
                           gridded Raster layer or any spatial organization such as
                           a polygon. A regular polygon has equal sides and angles."),
                         shiny::br(),
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
                               "Gridded",
                               "Point", "Line",
                               "Regular Polygon (e.g. hexagon)",
                               "Irregular Polygon (e.g. hydrological basin)",
                               "Other"
                             ),
                             multiple = FALSE
                           ),
                           shiny::conditionalPanel(
                             condition = "input.pu_type == 'Other'",
                             ns = ns,
                             shiny::textAreaInput(inputId = ns("othertypes"), label = "Other type",
                                           placeholder = 'Define here',
                                           height = "45px", width = "100%", resize = "none")
                           )
                          ),
                         shiny::br(),
                         bs4Dash::box(
                           title = "What was the spatial grain of planning?",
                           closable = FALSE,
                           width = 12,
                           solidHeader = TRUE,
                           status = "secondary",
                           collapsible = FALSE,
                           shiny::numericInput(
                             inputId = ns("pu_grain"),
                             label = "Planning unit area (for example 100)",
                             value = NA,
                           ),
                           shiny::selectizeInput(inputId = ns("pu_grainunit"),
                                                 label = "Unit of the spatial grain",
                                                 choices = c("",
                                                             "m2","km2", "ha",
                                                             "ft2", "yd2","mi2",
                                                             "acre"
                                                             ),
                                                 multiple = FALSE,
                                                 options = list(create = TRUE,
                                                                placeholder = "Choose from list, or type and click to add a new option.")),
                           shiny::br(),
                           shiny::p("Fill the text box below if the grain of the
                                    planning unit cannot be easily determined.
                                    For example if planning units are agricultural field sizes."),
                           shiny::textAreaInput(inputId = ns("pu_grainother"),
                                                label = "Any other description with regards to spatial grain of PU.",
                                                placeholder = 'Leave empty if not applicable.',
                                                height = "45px", width = "100%", resize = "none")
                         ),
                         shiny::br(),
                         bs4Dash::box(
                           title = "Planning unit costs or penalities",
                           closable = FALSE,
                           width = 12,
                           solidHeader = TRUE,
                           status = "secondary",
                           collapsible = FALSE,
                           shiny::p("The decision where to allocate conservation efforts can to a large degree be determined
                                    by economic, biophysical or socio-economic constraints. One way of including those in planning
                                    studies is to treat them as a cost or penality, thus penalizing the selection of any outcomes
                                    with too high costs. Typical are for example the costs of land acquistion in area-based planning."),
                           shinyWidgets::pickerInput(
                             inputId = ns("pu_checkcosts"),
                             label = "Where there any costs or penalities for selecting a planning unit? Select one or multiple.",
                             choices = c(
                               c("Area only",
                                 "Biophysical proxy",
                                 "Socioeconomic proxy",
                                 "Implementation cost",
                                 "Acquisition cost",
                                 "Opportunity cost",
                                 "Management cost",
                                 "Logistics cost",
                                 "Sampling effort",
                                 "Other")
                             ),
                             options = list(`actions-box` = TRUE),
                             multiple = TRUE
                           ),
                           shiny::br(),
                           # Any other cost description?
                           shiny::textAreaInput(inputId = ns("pu_costs"), label = "Describe process of creating costs",
                                         placeholder = 'Provide some detail on how costs were defined or created.',
                                         height = "45px", width = "100%", resize = "none")
                         )
                    )
                ) # End column
              ), # End row
            # Zones and groups ----
                    shiny::fluidRow(
                    shiny::column(width = 2),
                    shiny::column(width = 12,
                     # Planning units
                     bs4Dash::box(
                       title = 'Zones and specific groups',
                       closable = FALSE,
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       collapsible = TRUE,
                       shiny::p("Not all planning units or planning objectives might
                         receive the same attention or are considered equally.
                         Here we record whether certain area or themes were
                         exclusively considered, included or excluded."),
                       # Ecosystem specifics
                       bs4Dash::box(
                         title = "Was there any ecosystem specificity?",
                         closable = FALSE,
                         width = 12,
                         solidHeader = TRUE,
                         status = "secondary",
                         collapsible = FALSE,
                         shiny::p("Planning can be conducted on all land or sea within a given
                           region, but it can also be specific to certain ecosystems
                           or land-use types, such as for example forests.",
                           shiny::strong("Check No if all available land or sea within the study region was considered.")),
                          shinyWidgets::awesomeRadio(
                            inputId = ns("checkecosystem"),
                            label = "Was there any ecosystem specificity?",
                            choices = c("No", "Yes"),
                            selected = "No",
                            inline = FALSE,
                            checkbox = TRUE
                          ),
                      shiny::br(),
                      shiny::conditionalPanel(
                        condition = "input.checkecosystem == 'Yes'",
                        ns = ns,
                        shiny::textAreaInput(inputId = ns("specificecosystem"), label = "Which ecosystems were considered?",
                                      placeholder = 'List them here',
                                      height = "45px", width = "100%", resize = "none")
                        )
                      ),
                      shiny::br(),
                      # Zones
                      bs4Dash::box(
                        title = "Where any zones used for the planning?",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shiny::p("Planning can be structured by considering multiple
                          management or land/water-use objectives through zones.
                          Here we describe them if those are set."),
                        shinyWidgets::awesomeRadio(
                          inputId = ns("checkzones"),
                          label = "Were different management zones considered?",
                          choices = c("No", "Yes"),
                          selected = "No",
                          inline = FALSE,
                          checkbox = TRUE
                        ),
                        shiny::br(),
                        shiny::conditionalPanel(
                          condition = "input.checkzones == 'Yes'",
                          ns = ns,
                          shiny::p("Describe the zones used in the planning. Zones can be useful to prioritize
                            for not a single, but a set of management decisions. For example, protected area
                            managers might want to identify areas of minimal intervention ('core-areas')
                            as well as sustainable use areas."),
                          shiny::p("Reference: Watts, Matthew E., Ian R. Ball, Romola S. Stewart, Carissa J. Klein, Kerrie Wilson, Charles Steinback, Reinaldo Lourival, Lindsay Kircher, and Hugh P. Possingham. Marxan with Zones: Software for Optimal Conservation Based Land- and Sea-Use Zoning. Environmental Modelling & Software 24, no. 12 (December 2009): 1513-21. https://doi.org/10.1016/j.envsoft.2009.06.005."),

                          DT::DTOutput(outputId = ns("specificzones")),
                          shiny::actionButton(inputId = ns("add_zone"), label = "Add a new zone",
                                              icon = shiny::icon("plus")),
                          shiny::actionButton(inputId = ns("remove_zone"), label = "Remove last added zone",
                                              icon = shiny::icon("minus")),
                          shiny::fileInput(inputId = ns('load_zones'),label = 'Alternatively upload a zone list:',
                                           accept = c('csv', 'comma-separated-values','.csv', 'tsv', '.tsv')),
                          shiny::p("(Doubleclick on an added row to change the input values)")
                        )
                      ),
                      shiny::br(),
                      # Areas included or excluded
                      bs4Dash::box(
                        title = "Inclusion or exclusions?",
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "secondary",
                        collapsible = FALSE,
                        shiny::p("Any areas or actions that were included or excluded by default?"),
                        shinyWidgets::pickerInput(
                          inputId = ns("inclusionexclusion"),
                          label = "Select:",
                          choices = c("None", "Areas or Action included", "Areas or Action excluded"),
                          selected = "None",
                          multiple = TRUE
                        ),
                        shiny::conditionalPanel(
                          condition = "input.inclusionexclusion != 'None'",
                          ns = ns,
                          shiny::textAreaInput(inputId = ns("defaultareas"),
                                               label = "Describe inclusions or exclusions",
                                               placeholder = 'Describe in wording the specific areas or actions considered...',
                                        height = "60px", width = "100%", resize = "vertical")
                        )
                      )
                     )
               ) # Column end
             ), # Fluid row end
            shiny::br(),
            # Threats  ----
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(width = 12,
                            # Threats
                            bs4Dash::box(
                              title = 'Threats',
                              closable = FALSE,
                              width = 12,
                              solidHeader = TRUE,
                              status = "primary",
                              collapsible = TRUE,
                              shiny::p("Select any threats that were targeted in the planning
                                       or that, directly or indirectly, shape the planning outcome.
                                       The threat description broadly follows the IUCN Threat categorization system."),
                              shiny::br(),
                              shiny::p("IUCN Threat classification: https://www.iucnredlist.org/resources/threat-classification-scheme"),
                            # Threat types
                            bs4Dash::box(
                              title = "Threat types",
                              closable = FALSE,
                              width = 12,
                              solidHeader = TRUE,
                              status = "secondary",
                              collapsible = FALSE,
                              shinyWidgets::pickerInput(
                                inputId = ns("threattypes"),
                                label = "Select/deselect all options",
                                choices = c(
                                  "None",
                                  "Residential & commercial development",
                                  "Agriculture & Aquaculture",
                                  "Energy production & Mining",
                                  "Transportation & Traffic",
                                  "Biological Resource Use (fishing, harvesting, logging)",
                                  "Human Intrusions & Disturbance (recreation, war)",
                                  "Natural system modification (Fire, Dams)",
                                  "Invasive species, genes or diseases",
                                  "Pollution (Waste water, pollutants)",
                                  "Climate Change and severe weather (droughts, flooding)",
                                  "Geological events (Earthquakes)",
                                  "Other"
                                ),
                                options = list(`actions-box` = TRUE),
                                multiple = TRUE
                              ),
                              shiny::br(),
                              # Any other types?
                              shiny::textAreaInput(inputId = ns("otherthreattype"), label = "Other types",
                                                   placeholder = 'Any other type of threat not considered? Describe',
                                                   height = "45px", width = "100%", resize = "none")
                          ), # Threat type box end
                          shiny::br(),
                          # How were Threats considered/included?
                          bs4Dash::box(
                            title = "How were threats considered in the planning?",
                            closable = FALSE,
                            width = 12,
                            solidHeader = TRUE,
                            status = "secondary",
                            collapsible = FALSE,
                            shiny::p('There are multiple ways of including threats, for example
                                     by considering them as risk factor in the prioritization,
                                     as cost or penalty in selecting a solution.
                                     Here we record these various options.'),
                            shinyWidgets::pickerInput(
                              inputId = ns("threatinclusion"),
                              label = "Select/deselect all options",
                              choices = c(
                                "Feature (aggregated risk)",
                                "Feature (individual)",
                                "Cost layer",
                                "Selection penalty",
                                "Exclusion area",
                                "Management zone or feature contribution",
                                "Other"
                              ),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE
                            ),
                            shiny::textAreaInput(inputId = ns("threatdetail"), label = "Threat details",
                                                 placeholder = 'Elaborate on other ways of including threats in the planning',
                                                 height = "60px", width = "100%", resize = "vertical")
                          )
                  ) # Threats box end
              ) # Fluid column end
             ), # Fluidrow end
             shiny::br(),
             # Feature entries ----
             shiny::fluidRow(
               shiny::column(width = 2),
               shiny::column(width = 12,
                      # Features
                      bs4Dash::box(
                        title = 'Feature types and description',
                        closable = FALSE,
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        collapsible = TRUE,
                        shiny::p("What types of features are included in the spatial planning?
                          As features we do traditionally describe all (spatial)
                          information that enters the prioritization with the aim of
                          accruing benefits. Examples include estimates of the distribution
                          of species or Nature Contributions to people (NCPs)."),
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
                                "Species (genetic)",
                                "Connectivity",
                                "Ecosystems or Habitats",
                                "Land or water cover and use",
                                "Socio-economic (e.g. income from coastal fishing)",
                                "Political (e.g. protected area)",
                                "Biophysical (e.g. climate velocity)",
                                "Nature contributions to people (supply)",
                                "Nature contributions to people (demand)",
                                "Other"
                              ),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE
                            ),
                            shiny::br(),
                            # Any other types?
                            shiny::textAreaInput(inputId = ns("otherfeaturetype"), label = "Other types",
                                          placeholder = 'Any other broad feature types included? Describe',
                                          height = "45px", width = "100%", resize = "none")
                        ),
                        shiny::br(),
                        # Aggregated features
                        bs4Dash::box(
                          title = "Where any features types aggregated before use in the planning?",
                          closable = FALSE,
                          width = 12,
                          solidHeader = TRUE,
                          status = "secondary",
                          collapsible = FALSE,
                          shiny::p("An common example is the use of 'stacked' distribution
                          layers and subsequent inclusion of species richness in the prioritization."),
                          shinyWidgets::prettyToggle(
                            inputId = ns('checkaggregated'),
                            label_on = "Yes",
                            icon_on = shiny::icon("check"),
                            status_on = "success",
                            status_off = "danger",
                            label_off = "No",
                            icon_off = shiny::icon("remove")
                          ),
                          shiny::conditionalPanel(
                            condition = "input.checkaggregated",
                            ns = ns,
                            shiny::textAreaInput(inputId = ns('featureaggregated'), label = "Aggregation of feature types",
                                        placeholder = 'If applicable please describe which and how.',
                                        height = "45px", width = "100%", resize = "none")
                          )
                        ),
                        shiny::br(),
                        # List the features
                        bs4Dash::box(
                          title = "Provide a summary of all features:",
                          closable = FALSE,
                          width = 12,
                          solidHeader = T,
                          status = "secondary",
                          collapsible = TRUE,
                          shiny::p("List all features used in the planning, their type and an approximate number.
                                   Where possible assign groupings based on the Feature types above."),
                          DT::DTOutput(outputId = ns("featurelist")),
                          shiny::actionButton(inputId = ns("add_feature"), label = "Add a new feature row",
                                              icon = shiny::icon("plus")),
                          shiny::actionButton(inputId = ns("remove_feature"), label = "Remove last feature row",
                                              icon = shiny::icon("minus")),
                          shiny::helpText("(Doubleclick on an added row to change the input values)"),
                          shiny::br(),
                          shiny::p("Alternatively upload a grouped feature table in csv or tsv format.
                                   Note that this table needs to have exactly 3 columns with the name | group | number"),
                          shiny::fileInput(inputId = ns('load_feature'),label = 'Alternatively upload a feature/group list:',
                                    accept = c('csv', 'comma-separated-values','.csv', 'tsv', '.tsv'))
                        ),
                        shiny::br(),
                        # How were features created?
                        bs4Dash::box(
                          title = "How were features created and what do they contain?",
                          closable = FALSE,
                          width = 12,
                          solidHeader = TRUE,
                          status = "secondary",
                          collapsible = FALSE,
                          shiny::p('Commonly input features in the planning were created
                            through separate processes, such as through qualitative
                            data gathering or ecological modelling. Here we briefly
                            describe how input features were created.'),
                          shiny::textAreaInput(inputId = ns("featureorigin"), label = "",
                                           placeholder = 'Describe the origin of the input features',
                                           height = "60px", width = "100%", resize = "vertical"),
                          shiny::br(),
                          shiny::helpText("(Please be brief and where possible refer to existing text or other protocols)")
                        )
                      )
                  ) # Fluid column end
                ), # Fluidrow end

            # End of page button row
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(width = 8,
                            # Add backward button
                            shinyWidgets::actionBttn(
                              inputId = "go_design",
                              label = "Back to Design",
                              style = "simple",
                              color = "primary",
                              size = "sm",
                              block = FALSE,
                              icon = shiny::icon("arrow-left")
                            ),
                            # Add forward button
                            shinyWidgets::actionBttn(
                              inputId = "go_context",
                              label = "Continue to context",
                              style = "simple",
                              color = "primary",
                              size = "sm",
                              block = FALSE,
                              icon = shiny::icon("arrow-right")
                            )
              )
            )
          ) # Fluidpage end
  ) # End tab
}

#' Specification Server Functions
#'
#' @importFrom shiny observe observeEvent
#' @noRd
mod_Specification_server <- function(id, results, parentsession){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Get protocol
    protocol <- load_protocol()$specification # Get all overview UI elements

    # Load all parameters in specification and add
    ids <- get_protocol_ids(group = "specification")
    shiny::observe({
      for(id in ids){
        if(id == "featurelist"){
          results[[id]] <- data.frame(feature_table()) |> asplit(MARGIN = 1)
        } else if(id == "specificzones") {
          results[[id]] <- data.frame(zones_table()) |> asplit(MARGIN = 1)
        } else {
          results[[id]] <- input[[id]]
        }
      }
    })

    # --- #
    # Define the features list
    feature_table <- shiny::reactiveVal(
      data.frame(name = character(0),
                 group = character(0),
                 number = numeric(0L))
    )

    # Get feature groups for event
    featoptions <- get_protocol_options('featuretypes')
    # Events for feature table
    shiny::observeEvent(input$add_feature, {
      shiny::showModal(
        shiny::modalDialog(title = "Add new feature", footer = NULL, easyClose = T,
                    shiny::textInput(ns("name"), "Feature name"),
                    shiny::selectInput(ns("group"), "Feature group",choices = featoptions),
                    shiny::numericInput(ns("number"), "Total number",value = 1, min = 1),
                    shiny::actionButton(ns("save_new_feature"), "Add new feature")
        )
      )
    })

    shiny::observeEvent(input$save_new_feature, {
      if(input$name == "" | input$group == ""){
        shiny::showNotification("Please provide a Feature and select a group", duration = 2, type = "warning")
      } else {
        new_feature = data.frame("name" = input$name, "group" = input$group, "number" = input$number,
                                 stringsAsFactors = F)
        # Check for zero number
        if(input$number==0){
          shiny::showNotification("Features with number zero?", duration = 4, type = "error")
        }
        new_data <- feature_table() |> dplyr::add_row(new_feature)
        feature_table(new_data)
        shiny::removeModal()
      }
    })

    shiny::observeEvent(input$remove_feature, {
      new_data <- feature_table()
      if(nrow(new_data)==0){
        shiny::showNotification("No features added yet!",
                                duration = 2, type = "warning")
      } else {
        new_data <- new_data |> dplyr::slice(-dplyr::n())
        feature_table(new_data)
      }
    })

    #output the datatable based on the dataframe (and make it editable)
    output$featurelist <- DT::renderDataTable({
        DT::datatable(feature_table(), rownames = FALSE,
                    colnames = c("Feature name", "Feature group", "Total number"),
                    filter = "none", selection = "none",
                    style = "auto",
                    editable = TRUE)
    })

    # Manual edit
    shiny::observeEvent(input$featurelist_cell_edit, {
      info <- input$featurelist_cell_edit
      modified_data <- feature_table()
      modified_data[info$row, info$col+1] <- info$value
      feature_table(modified_data)
    })

    # Load an external file
    loadedfeatures <- shiny::reactive({
      file <- input$load_feature$datapath
      shiny::req(file)

      data <- readr::read_csv(file, show_col_types = FALSE)
      # Do some checks?
      shiny::validate(
        shiny::need(ncol(data)!=3, "Uploaded data requires exactly 3 columns")
      )
      names(data) <- c("name", "group", "number")
      return(data)
    })

    # Observe event to update data table
    shiny::observeEvent(loadedfeatures(), {
      output$featurelist <- DT::renderDT({
        DT::datatable(loadedfeatures(), rownames = FALSE,
                      colnames = c("Feature name", "Feature group", "Total number"),
                      filter = "none", selection = "none",
                      style = "auto",
                      editable = TRUE)
      })
    })

    # --- #
    # Define the features list
    zones_table <- shiny::reactiveVal(
      data.frame(name = character(0),
                 aim = character(0),
                 costs = character(0),
                 contributions = character(0)
      )
    )

    # Events for author table
    shiny::observeEvent(input$add_zone, {
      new_data <- zones_table() |> dplyr::add_row(
        data.frame(name = "My zone", aim = "Zone purpose",
                   costs = "Differing costs",
                   contributions = "Who benefits")
      )
      zones_table(new_data)
    })

    shiny::observeEvent(input$remove_zone, {
      new_data <- zones_table()
      if(nrow(new_data)==0){
        shiny::showNotification("No zones added yet!",
                                duration = 2, type = "warning")
      } else {
        new_data <- new_data |> dplyr::slice(-dplyr::n())
        zones_table(new_data)
      }
    })

    #output the datatable based on the dataframe (and make it editable)
    output$specificzones <- DT::renderDT({
      DT::datatable(zones_table(), rownames = FALSE,
                    colnames = c("Zone name", "Purpose", "Costs", "Who benefits"),
                    filter = "none", selection = "none",
                    style = "auto",
                    editable = TRUE)
    })

    # Manual edit
    shiny::observeEvent(input$specificzones_cell_edit, {
      info <- input$specificzones_cell_edit
      modified_data <- zones_table()
      modified_data[info$row, info$col+1] <- info$value
      zones_table(modified_data)
    })

    # Load an external file
    loadedzones <- shiny::reactive({
      file <- input$load_zones$datapath
      shiny::req(file)

      if(is.null(input$load_zones)){
        return(NULL)
      }
      data <- readr::read_csv(file, show_col_types = FALSE)
      # Do some checks?
      shiny::validate(
        shiny::need(ncol(data)!=4, "Uploaded data requires exactly 4 columns")
      )
      names(data) <- c("name", "aim", "costs", "contributions")
      return(data)
    })
    # Automatically render the loadedzones
    # Observe event to update data table
    shiny::observeEvent(loadedzones(), {
      output$specificzones <- DT::renderDT({
        DT::datatable(loadedzones(), rownames = FALSE,
                      filter = "none", selection = "none",
                      style = "auto",
                      editable = TRUE)
      })
    })

    # Events for hiding data input boxes
    shiny::observeEvent(input$featuretypes, {
      if('Other' %in% input$featuretypes){
        shinyjs::show("otherfeaturetype")
      }
    })

    # --- #
    # Add Tooltips for each element
    shiny::observeEvent(parentsession$input$help_switch,{
      # Enable tooltips if set
      add_protocol_help(parentsession, protocol, type = "popover")
    })
    # --- #

    # Show spatial grain box
    shiny::observeEvent(input$pu_type, {
      if(any(c('Gridded','Regular Polygon (e.g. rectangle)') %in% input$pu_type)){
        shinyjs::show("pu_grain")
        shinyjs::show("pu_grainunit")
      } else {
        shinyjs::hide("pu_grain")
        shinyjs::hide("pu_grainunit")
      }
    })

  })
}

## To be copied in the UI
# mod_Specification_ui("Specification_1")

## To be copied in the server
# mod_Specification_server("Specification_1")
