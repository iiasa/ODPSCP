#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import waiter
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # --- #
    # Add the overall dashboard
    bs4Dash::dashboardPage(
      # Preloader using waiter
      preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#3c8dbc"),
      # freshTheme =  dark_theme(), # Theme designed with fresh
      # Other options
      dark = FALSE,
      scrollToTop = TRUE,
      fullscreen = FALSE,
      help = NULL, # Default enable tooltips
      # controlbar = dashboardControlbar(),
      # Define header and footer
      header = bs4Dash::dashboardHeader(
        title = dashboardBrand(
          title = "A Protocol for SCP",
          color = "primary",
          opacity = 0,
          href = NULL,
          image = "logo.png"
        ),
        status = "white",
        border = TRUE,
        compact = FALSE,
        leftUi = NULL, rightUi = NULL
        ),
      footer = bs4Dash::dashboardFooter(
        fixed = FALSE,
        left = tagList(
          p("Protocol version: ", get_protocol_version(),
            style = "padding-top: 10px; font-size: 14px; font-weight:bold;")
          # a(
          #   href = "https://github.com/iiasa/ODPSCP",
          #   target = "_blank",
          #   "Github"
          # )
        ),
        right = tagList(
          "IIASA BEC",
          "(",format(Sys.Date(),"%Y"),")",
          img(
            href = "www/iiasa-logo.png",
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
            tabName = "Home",
            icon = icon("home")
          ),
          bs4Dash::menuItem(
            text = "Protocol",
            startExpanded = TRUE,
            # icon = icon("bars"),
            bs4Dash::menuSubItem(
              "Overview",
              tabName = "Overview",
              icon = icon("file-lines")
            ),
            bs4Dash::menuSubItem(
              "Design",
              tabName = "Design",
              icon = icon("object-group")
            ),
            bs4Dash::menuSubItem(
              "Specification",
              tabName = "Specification",
              icon = icon("tree")
            ),
            bs4Dash::menuSubItem(
              "Context",
              tabName = "Context",
              icon = icon("users-rectangle")
            ),
            bs4Dash::menuSubItem(
              "Prioritization",
              tabName = "Prioritization",
              icon = icon("map")
            )
          ),
          sidebarHeader("Import/Export"),
          bs4Dash::menuItem(
            "Import protocol",
            tabName = "Import",
            icon = icon("upload")
          ),
          bs4Dash::menuItem(
            "Export protocol",
            tabName = "Export",
            icon = icon("download")
          ),
          sidebarHeader("Info"),
          bs4Dash::menuItem(
            "News",
            tabName = "News",
            icon = icon("newspaper")
          ),
          bs4Dash::menuItem(
            "Issues and Feedback",
            href =  "https://github.com/iiasa/ODPSCP/issues",
            newTab = TRUE,
            icon = icon("question")
          ),
          bs4Dash::menuItem(
            "Source code",
            href =  "https://github.com/iiasa/ODPSCP",
            newTab = TRUE,
            icon = icon("code")
          )
        )
      ), # Sidebar end
      #### Body with sidebar menu ----
      body = bs4Dash::dashboardBody(
        # title page --------------------------------------------------------------
        tabItems(
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
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ODPSCP"
    ),

    # Add here other external resources
    # rintrojs::introjsUI(),
    sever::useSever(),
    waiter::use_waiter(),
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs()
  )
}
