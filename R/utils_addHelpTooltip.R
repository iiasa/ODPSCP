#' This helper function add popover toolbars to all protocol elements
#'
#' @description
#' Although this could be done directly in the protocol,
#' it generally remains clearer to add the help documentation directly here.
#' @param session A target session which adds the respective popovers.
#' @param protocol A [`list`] with protocol elements.
#' @param type A [`character`] indicating whether \code{"popover"} or \code{"tooltip"}
#' are to be added.
#'
#' @return Only adds to ui
#' @noRd
add_protocol_help <- function(session, protocol, type = "popover"){
  assertthat::assert_that(
    utils::hasName(session,"input"),
    is.list(protocol),
    length(protocol)>0,
    is.character(type)
  )
  # Match types
  type <- match.arg(type, c("popover", "tooltip"),several.ok = FALSE)

  for(n in names(protocol)){
    sub <- protocol[[n]]
    if(session$input$help_switch){
      if(type == "popover"){
        bs4Dash::addPopover(
          id = sub['render-id'],
          options = list(
            content = sub$popexample,
            title = "Example:",#sub$question,
            placement = "auto",
            trigger = "focus"
          )
        )
      } else {
        bs4Dash::addTooltip(
          id = sub['render-id'],
          options = list(
            content = sub$popexample,
            title = "Example:", #sub$question,
            placement = "auto",
            trigger = "focus"
          )
        )
      }
    } else {
      # Disable tootips
      if(type == "popover"){
       bs4Dash::removePopover(
        id = sub['render-id']
        )
      } else {
        bs4Dash::removeTooltip(
          id = sub['render-id']
        )
      }
    }
  }

}
