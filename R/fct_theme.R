#' The theme used for styling the package
#'
#' @description This function creates a theme for the protocol.
#' To be used in a bs4dashboard.
#'
#' @details
#' https://dreamrs.github.io/fresh/articles/vars-bs4dash.html
#'
#' @return Returns a css object to be used in the dashboard.
#'
#' @import fresh
#' @noRd
odpscp_theme <- function(){
  # Create theme
  tt <- fresh::create_theme(
    fresh::bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    fresh::bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    fresh::bs4dash_layout(
      main_bg = "#353c42"
    ),
    fresh::bs4dash_sidebar_light(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    fresh::bs4dash_status(
      primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
    ),
    fresh::bs4dash_color(
      gray_900 = "#FFF"
    )
  )
  return(tt)
}


#' Dark theme used for styling the package
#'
#' @description This function creates a theme for the protocol.
#' To be used in a bs4dashboard.
#'
#' @details
#' https://unleash-shiny.rinterface.com/beautify-with-fresh.html
#'
#' @return Returns a css object to be used in the dashboard.
#'
#' @import fresh
#' @noRd
dark_theme <- function(){
  create_theme(
    bs4dash_vars(
      navbar_light_color = "#bec5cb",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF",
      text_light = "#272c30"
    ),
    bs4dash_layout(main_bg = "#353c42"),
    bs4dash_sidebar_dark(
      bg = "#272c30",
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30",
      submenu_color = "#FFF",
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(dark = "#272c30"),
    bs4dash_color(gray_900 = "#FFF", white = "#272c30")
  )
}
