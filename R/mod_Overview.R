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
              "Let's start with a new reporting protocol. In the overview step we describe
              all the properties of the conducted planning study. The entries below
              intend to both uniquely identify the study, provide necessary information
              on the availability of code or data and allows to categorizes the study itself
              based on the listed properties.
              ",
              shiny::br(),
              shiny::strong("By default example popups are shown for text fields, which can be disabled through the questionmark at the top bar.")
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
            shiny::textAreaInput(inputId = ns("studyname"),
                                  label = shiny::div("Study name"),
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
            shiny::p("Add each author of the study to the table below. If a ORCID is not
                     known or available, leave blank."),
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
            shiny::div("Who is the corresponding author? Here contact information can be added."),
            shiny::textAreaInput(inputId = ns("authoremail"), label = "",
                          placeholder = 'Email of the corresponding author',
                          height = "45px", width = "100%", resize = "none")
          ),
          # Link to study
          bs4Dash::box(
            title = "(Optional) Link to study",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "gray",
            collapsible = FALSE,
            shiny::div("Has the study already been published? If so, provide a reference."),
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
          # Study region
          bs4Dash::box(
            title = "(Optional) Study region",
            closable = FALSE,
            width = 12,
            solidHeader = TRUE,
            status = "secondary",
            collapsible = FALSE,
            shiny::div("Here the planning unit grid can be provided as geospatial dataset. Currently
                       supported is the upload of gridded or vector planning unit files.
                       Accepted formats are shapefiles, geopackages or geotiffs."),
            # shiny::div("Alternatively a boundary box (longitude-latitude) can be provided."),
            shiny::hr(),
            # --- #
            shinyWidgets::radioGroupButtons(
              inputId = ns("spatial_selector"),
              label = "",
              choices = c(#"Bounding box",
                          "Spatial file"),
              individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle",
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o",
                            style = "color: steelblue"))
            ),
            # shiny::conditionalPanel(
            #   condition = "input.spatial_selector == 'Bounding box'",
            #   ns = ns,
            #   shiny::p("Enter minimum and maximum longitude (x) and latitude (y) coordinates:"),
            #   shiny::fluidRow(
            #     shiny::splitLayout(
            #       shiny::column(8,
            #                     shiny::numericInput(inputId = ns("studyregion_bbox_xmin"),value = NULL,
            #                                         label = "xmin",min = -180,max = 180, width = "100px"),
            #                     shiny::numericInput(inputId = ns("studyregion_bbox_ymin"),value = NULL,
            #                                         label = "ymin",min = -90,max = 90, width = "100px")
            #       ),
            #       shiny::column(8,
            #                     shiny::numericInput(inputId = ns("studyregion_bbox_xmax"),value = NULL,
            #                                         label = "xmax",min = -180,max = 180, width = "100px"),
            #                     shiny::numericInput(inputId = ns("studyregion_bbox_ymax"),value = NULL,
            #                                         label = "ymax",min = -89.99999,max = 90, width = "100px")
            #                     )
            #     )
            #   )
            # ),
            shiny::conditionalPanel(
              condition = "input.spatial_selector == 'Spatial file'",
              ns = ns,
              # Input: Select a spatial filefile ----
              shiny::fileInput(
                ns("studyregion"),
                "Choose geospatial File",
                multiple = FALSE,
                accept = c(
                  ".shp", ".gpkg",
                  ".tif", ".geotiff"
                )
              ),
              shiny::helpText("Note that the maximum file size is 30 MB and larger files might take a while to render.")
            ),
            shiny::br(),
            leaflet::leafletOutput(ns("studymap")),
            shiny::actionButton(inputId = ns("updatemap"),
                                label = "Update View", icon = shiny::icon("refresh")),
            shiny::actionButton(inputId = ns("clearmap"),
                                label = "Clear map", icon = shiny::icon("broom"))

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
            shiny::p("Define the temporal scale over which the planning and specifically
                     the planning objective applies. This should not be interpreted as
                     a period of data coverage. If outside the chosen scale,
                     please provide details in the textbox."),
            shinyWidgets::sliderTextInput(
              inputId = ns("studytime"),
              label = "Choose a range:",
              choices = 1960:2100,
              grid = TRUE,
              selected = c(2000,2020)
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
                          "Freshwater", "Marine (Pelagic)",
                          "Marine (Seabed)", "Coastal", "Air"),
              size = "sm",
              justified = TRUE,
              checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
              )
            )
          )
        ) # Column end
      ),
      # Data and code availability ----
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
            shiny::imageOutput(ns("peng2011"),inline = TRUE,fill = FALSE),
            shiny::br(),
            shiny::helpText("Source: Peng, R. D. (2011). Reproducible research in computational science. Science, 334(6060), 1226-1227."),
            shiny::br(),
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
                shiny::p("If applicable please enter a link to the data storage repository."),
                shinyWidgets::awesomeRadio(
                  inputId = ns('inputavailability'),
                  label = "Is the input data provided with the study?",
                  choices = c("No", "Yes"),
                  selected = "No",
                  inline = FALSE,
                  checkbox = TRUE
                ),
                shiny::br(),
                shiny::conditionalPanel(
                  condition = "input.inputavailability == 'Yes'",
                  ns = ns,
                  shiny::textAreaInput(inputId = ns('inputdata'), label = "Input data",
                                       placeholder = 'If applicable please enter a link to the data storage repository.',
                                       height = "45px", width = "100%", resize = "none")
                )
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
              shiny::p("Typical outputs include for example priority maps or performance indicators.
                       Describe all outouts here and where they are stored."),
              shinyWidgets::awesomeRadio(
                inputId = ns('outputavailability'),
                label = "Are the outputs of the planning provided with the study?",
                choices = c("No", "Yes"),
                selected = "No",
                inline = FALSE,
                checkbox = TRUE
              ),
              shiny::br(),
              shiny::conditionalPanel(
                condition = "input.outputavailability == 'Yes'",
                ns = ns,
                shiny::textAreaInput(inputId = ns('outputdata'), label = "Output data",
                                     placeholder = 'If applicable please enter a link to the data storage repository.',
                                     height = "45px", width = "100%", resize = "none")
              )
            ),
            shiny::br(),
            # Link to code
            bs4Dash::box(
              title = 'Has the analytical code to reproduce the results been made available?',
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "gray",
              collapsible = FALSE,
              shiny::p("Preparing data for analysis and creating priority maps can be done
                       with computer code. If such code was created, consider storing it somewhere
                       and make it available."),
              shinyWidgets::awesomeRadio(
                inputId = ns('codeavailability'),
                label = "Has the analytical code for making the outputs been made available?",
                choices = c("No", "Yes"),
                selected = "No",
                inline = FALSE,
                checkbox = TRUE
              ),
              shiny::br(),
              shiny::conditionalPanel(
                condition = "input.codeavailability == 'Yes'",
                ns = ns,
                shiny::textAreaInput(inputId = ns('outputcode'), label = "Analysis code",
                                     placeholder = 'If applicable please enter a link to the code storage repository.',
                                     height = "45px", width = "100%", resize = "none")
              )
            )
          ) # End of column
        ) # End of fluid row
      ),

      # uiOutput("Overview_UI")

      # End of page button row
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(width = 8,
               # # Add reset button
               # shinyWidgets::actionBttn(
               #   inputId = ns("reset"),
               #   label = "Clear all fields?",
               #   style = "simple",
               #   color = "danger",
               #   size = "md",
               #   block = FALSE,
               #   icon = icon("broom")
               # ),
               # Add backward button
               shinyWidgets::actionBttn(
                 inputId = "go_home",
                 label = "Back to start",
                 style = "simple",
                 color = "primary",
                 size = "sm",
                 block = FALSE,
                 icon = shiny::icon("arrow-left")
               ),
               # Add forward button
               shinyWidgets::actionBttn(
                 inputId = "go_design",
                 label = "Continue with the design",
                 style = "simple",
                 color = "primary",
                 size = "sm",
                 block = FALSE,
                 icon = shiny::icon("arrow-right")
               )
        )
      ) # End of fluid row for buttons
    ) # End of fluid page
  ) # End of tab
}

#' Overview Server Functions
#'
#' @importFrom shiny observe observeEvent
#' @noRd
mod_Overview_server <- function(id, results, parentsession){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    protocol <- load_protocol()$overview # Get all overview UI elements
    # output$Overview_UI = render_protocol("Overview", protocol)

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
      data.frame(firstname = character(0),
                 surname = character(0),
                 orcid = character(0) )
    )

    # Events for author table
    shiny::observeEvent(input$add_author, {
      new_data <- authors() |> dplyr::add_row(
        data.frame(firstname = "EDIT ME", surname = "EDIT ME", orcid = "EDIT ME")
      )
      authors(new_data)
    })

    shiny::observeEvent(input$remove_author, {
      new_data <- authors()
      if(nrow(new_data)==0){
        shiny::showNotification("No authors added yet!",
                                duration = 2, type = "warning")
      } else {
        new_data <- new_data |> dplyr::slice(-dplyr::n())
        authors(new_data)
      }
    })

    #output the datatable based on the dataframe (and make it editable)
    output$authors_table <- DT::renderDataTable({
      DT::datatable(authors(),rownames = FALSE,
                    colnames = c("First name", "Surname", "ORCID"),
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
    #### Studyregion updates ####

    # Gather study region from bounding box
    # xmin <- shiny::reactive(input$studyregion_bbox_xmin)
    # ymin <- shiny::reactive(input$studyregion_bbox_ymin)
    # xmax <- shiny::reactive(input$studyregion_bbox_xmax)
    # ymax <- shiny::reactive(input$studyregion_bbox_ymax)
    # if(input$spatial_selector == 'Bounding box'){
    #   # xmin <- 40; ymin <- 20; xmax <- 70; ymax <- 30
    #   out <- sf::st_bbox(c(xmin = rv(xmin), xmax = rv(xmax),
    #                        ymin = rv(ymin), ymax = rv(ymax))) |>
    #     sf::st_as_sfc() |> sf::st_as_sf(crs = sf::st_crs(4326))
    #   assertthat::assert_that(sf::st_is_valid(out))
    # } else {
    # }

    # Test default
    myregion <- shiny::reactive({
      # Get the studypath
      file <- input$studyregion$datapath
      shiny::req(file)

      if(!is.null(input$studyregion)){
        # Found vector
        if(tolower( tools::file_ext(file)) %in% c("shp","gpkg")){
          out <- sf::st_read(file, quiet = TRUE) |>
            sf::st_transform(crs = sf::st_crs(4326))
        } else if(tolower( tools::file_ext(file)) %in% c("tif","geotiff")){
          out <- terra::rast(file)
          out[out>0] <- 1 # Replace all with 1
          out <- out |> terra::as.polygons() |> sf::st_as_sf() |>
            sf::st_cast("MULTIPOLYGON") |>
            sf::st_transform(crs = sf::st_crs(4326))
        }
        return(out)
      } else {
        return(NULL)
      }
    })

    # Observe Event to automatically draw uploaded file
    shiny::observeEvent(myregion(), {
      if(inherits(myregion(), "sf")){
        # Try to redraw
        map <- leaflet::leafletProxy("studymap", session) |>
          leaflet::clearShapes() |>
          leaflet::addPolygons(data = myregion(),
                               stroke = TRUE,
                               color = "darkred")
        # Calculate centroid and zoom in
        cent <- cbind(
          mean( sf::st_coordinates( myregion() )[,1] ),
          mean( sf::st_coordinates( myregion() )[,2] )
        )
        map <- map |>
          leaflet::setView(
            lng = cent[,1],
            lat = cent[,2],
            zoom = 7
          )
        map
      }
    })

    # Reactive studyregion redraw
    shiny::observeEvent(input$updatemap, {
      if(inherits(myregion(), "sf")){
        # If my region is not null, update
        map <- leaflet::leafletProxy("studymap", session) |>
          leaflet::clearShapes() |>
          leaflet::addPolygons(data = myregion(),
                               stroke = TRUE,
                               color = "darkred")

        # Calculate centroid and zoom in
        cent <- cbind(
          mean( sf::st_coordinates( myregion() )[,1] ),
          mean( sf::st_coordinates( myregion() )[,2] )
        )
        map <- map |>
          leaflet::setView(
            lng = cent[,1],
            lat = cent[,2],
            zoom = 7
            )
        map
      }
    })

    # Clean map
    shiny::observeEvent(input$clearmap, {
      map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        leaflet::addTiles(leaflet::providers$OpenStreetMap) |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap,
                                  options = leaflet::providerTileOptions(noWrap = TRUE),
                                  group="Open Street Map")
      output$studymap <- leaflet::renderLeaflet({map})
    })

    # Render default study region leaflet code
    output$studymap <- leaflet::renderLeaflet({
      # generate base leaflet
      map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        leaflet::addTiles(leaflet::providers$OpenStreetMap) |>
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap,
                                  options = leaflet::providerTileOptions(noWrap = TRUE),
                                  group="Open Street Map")
      map
    })

    # ----- #
    # Send a pre-rendered image, and don't delete the image after sending it
    output$peng2011 <- shiny::renderImage({
      path_figure <- base::normalizePath(
        system.file("Peng2011_Science_spectrum.png",
                                  package = "ODPSCP",
                                  mustWork = TRUE)
      )
      # Return a list containing the filename and alt text
      list(src = path_figure,
           width = "300px",
           height = "120px",
           alt = paste("Peng 2011, Reproducible Research in Computational Science, Science"))

    }, deleteFile = FALSE)

    # --- #
    # Add Tooltips for each element
    shiny::observeEvent(parentsession$input$help_switch,{
      # Enable tooltips if set
      add_protocol_help(parentsession, protocol, type = "popover")
    })
    # --- #

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
