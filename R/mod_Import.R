#' Import UI Function
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
mod_Import_ui <- function(id){
  ns <- NS(id)

  # TODO:
  # Import existing Marxan / Zonation / Prioritizr configuration files?
  bs4Dash::tabItem(tabName = "Import",
          shiny::fluidPage(
            bs4Dash::box(
              title = shiny::tagList(shiny::icon("upload"), "Import a previously saved protocol"),
              closable = FALSE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsed = FALSE,
              collapsible = FALSE,
            shiny::p("Here you can upload a previously saved protocol file. This
                     file needs to be either in yaml or csv format. After uploading,
                     the various entries in the protocol will be filled with the previous version."),
            shiny::br(),
            shiny::div("Please note:"),
            shiny::helpText("> Non accepted fields will not be parsed."),shiny::br(),
            shiny::helpText("> Reexporting the protocol resets internally the version and date."),
            shiny::br(),
            shiny::hr(),
            shiny::fileInput(
              inputId = ns("protocolFile"),
              width = "100%",
              label = "Load a previously exported protocol",
              accept = c(
                "text/csv",
                "text/comma-separated-values",
                ".yaml"
              )
            ),
            shiny::helpText("Default max. file size is 30MB")
            ) # End of box
          ) # End fluid page
    ) # End of tab box
}

#' Import Server Functions
#'
#' @noRd
mod_Import_server <- function(id){
 shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Parse the results
    imports <- shiny::reactive({
      file <- input$protocolFile$datapath
      # Request the file
      shiny::req(file)

      if(is.null(input$protocolFile)){
        return(NULL)
      }
      # Get extension
      ext <- tools::file_ext(file)
      # Different processing depending on upload type
      out <- tryCatch({
        switch(ext,
               csv = utils::read.csv(file, sep = ","),
               yaml = yaml::read_yaml(file),
               shiny::validate("Invalid file; Please upload a .csv or .yaml file!")
              )
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(shiny::safeError(e))
        }
      )
      # Checks
      if(inherits(out, "try-error")){
        shiny::showNotification("File not valid!", duration = 2, type = "error")
        return(NULL)
      } else if(is.data.frame(out)){
        shiny::validate(
          shiny::need(nrow(out)>10, "Uploaded dataframe seems to small?")
        )
      } else if(is.list(out)){
        shiny::validate(
          shiny::need(length(out)>=5, "Uploaded data requires 5 entry groups.")
        )
      }
      return(out)
    })

    # Validate the protocol and import protocol
    shiny::observeEvent(imports(), {
      if(!is.null(imports())){
        check <- validate_protocol_results(imports())
        if(!is.null(check)){
          shiny::showNotification(check, duration = 5,closeButton = TRUE, type = "error")
        }
      }
      # Insert protocol
      bs4Dash::updateTabItems(session, inputId = "sidebarmenu", selected = "Overview")
    })
  })
}

## To be copied in the UI
# mod_Import_ui("Import_1")

## To be copied in the server
# mod_Import_server("Import_1")
