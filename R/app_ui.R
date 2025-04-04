#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tags$head(
      tags$script(shiny::HTML("
      window.onbeforeunload = function (e) {
        var confirmationMessage = 'Are you sure you want to leave (accidentally closed)?';
        (e || window.event).returnValue = confirmationMessage;
        return confirmationMessage;
      };
    "))
    ),
    # --- #
    # Add the overall dashboard
    bs4Dash::dashboardPage(
      # Preloader using waiter
      preloader = list(html = shiny::tagList(
          waiter::spin_balance(), "Loading ..."), color = "#3c8dbc"),
      # freshTheme =  odpscp_theme(), # Theme designed with fresh
      # Other options
      dark = NULL,
      scrollToTop = TRUE,
      fullscreen = FALSE,
      help = TRUE, # Default enable tooltips
      # controlbar = bs4Dash::dashboardControlbar(),
      # Define header and footer
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "A Protocol for SCP",
          color = "primary",
          opacity = 0,
          href = NULL,
          image = shiny::img(src = base::normalizePath(system.file("app/www/logo.png", package = "ODPSCP")),
                             height = "20px")
        ),
        status = "white",
        border = TRUE,
        compact = FALSE,
        leftUi = NULL, rightUi = NULL,
        shiny::div(style="position:relative; left:calc(5%);",
                   shinyWidgets::actionBttn(
                     inputId = "bookmark",
                     label = "Save settings",
                     style = "material-flat",
                     color = "default",
                     size = "xs",
                     icon = shiny::icon("link", lib = "glyphicon")
                   )
                   # shiny::bookmarkButton(label = , id = "bookmark")
        )
        ),
      footer = bs4Dash::dashboardFooter(
        fixed = FALSE,
        left = shiny::tagList(
          shiny::div("Protocol version: ", get_protocol_version(),
            style = "padding-top: 10px; font-size: 14px; font-weight:bold;")
          # HTML("&nbsp; &nbsp; &nbsp; &nbsp;")
          # "(",format(Sys.Date(),"%Y"),")"
          # a(
          #   href = "https://github.com/iiasa/ODPSCP",
          #   target = "_blank",
          #   "Github"
          # )
        ),
        right = shiny::tagList(
          shiny::img(
            src = "www/iiasa-logo.png",
            title = "IIASA", height = "40px",
            target = "_blank"
          )
        )
      ),
      # Setup sidebar #
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        collapsed = FALSE,
        minified = TRUE,
        expandOnHover = TRUE,
        fixed = FALSE,
        id = NULL,
        customArea = NULL,
        #### Define sidebar menu ####
        bs4Dash::sidebarMenu(
          compact = TRUE,
          id = "sidebarmenu",
          bs4Dash::menuItem(
            "Home",
            tabName = "Home",selected = TRUE,
            icon = shiny::icon("home")
          ),
          bs4Dash::menuItem(
            text = "Protocol",
            startExpanded = TRUE,
            # icon = icon("bars"),
            bs4Dash::menuSubItem(
              "Overview",
              tabName = "Overview",
              icon = shiny::icon("file-lines")
            ),
            bs4Dash::menuSubItem(
              "Design",
              tabName = "Design",
              icon = shiny::icon("object-group")
            ),
            bs4Dash::menuSubItem(
              "Specification",
              tabName = "Specification",
              icon = shiny::icon("tree")
            ),
            bs4Dash::menuSubItem(
              "Context",
              tabName = "Context",
              icon = shiny::icon("users-rectangle")
            ),
            bs4Dash::menuSubItem(
              "Prioritization",
              tabName = "Prioritization",
              icon = shiny::icon("map")
            )
          ),
          bs4Dash::sidebarHeader("Import/Export"),
          bs4Dash::menuItem(
            "Import protocol",
            tabName = "Import",
            icon = shiny::icon("upload")
          ),
          bs4Dash::menuItem(
            "Export protocol",
            tabName = "Export",
            icon = shiny::icon("file-export")
          ),
          bs4Dash::sidebarHeader("Info"),
          bs4Dash::menuItem(
            "News",
            tabName = "News",
            icon = shiny::icon("newspaper")
          ),
          bs4Dash::menuItem(
            "Glossary",
            tabName = "Glossary",
            icon = shiny::icon("book")
          ),
          bs4Dash::menuItem(
            "Issues and Feedback",
            href =  "https://github.com/iiasa/ODPSCP/issues",
            newTab = TRUE,
            icon = shiny::icon("question")
          ),
          bs4Dash::menuItem(
            "Source code",
            href =  "https://github.com/iiasa/ODPSCP",
            newTab = TRUE,
            icon = shiny::icon("code")
          )
        )
      ), # Sidebar end
      #### Body with sidebar menu ----
      body = bs4Dash::dashboardBody(
        # title page --------------------------------------------------------------
        bs4Dash::tabItems(
          # Starting site
          mod_Home_ui("Home_1"),
            # Protocol
            mod_Overview_ui("Overview_1"),
            mod_Design_ui("Design_1"),
            mod_Specification_ui("Specification_1"),
            mod_Context_ui("Context_1"),
            mod_Prioritization_ui("Prioritization_1"),
          # News
          mod_News_ui("News_1"),
          mod_Glossary_ui("Glossary_1"),
          # Import
          mod_Import_ui("Import_1"),
          mod_Export_ui("Export_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  # Add resource path
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ODPSCP"
    ),

    # Add here other external resources
    # rintrojs::introjsUI(),
    sever::useSever(),
    waiter::use_waiter(),
    shinyFeedback::useShinyFeedback(),
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs()
  )
}
