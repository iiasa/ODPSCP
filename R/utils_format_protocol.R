#' Protocol format wrapper function
#'
#' @description
#' A utility function to format a filled out protocal into a different format.
#' Supported options are:
#' * data.frame
#' * html
#'
#' @param results A filled out protocol in [`list`] or reactive format.
#' @param format The output format that the results should have. Available options
#' include \code{"data.frame"} or \code{"list"}.
#' @param path_protocol The filepath to the actual protocol template
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
format_protocol <- function(results, format = "data.frame",path_protocol = NULL){
  assertthat::assert_that(is.list(results),
                          is.character(format),
                          is.character(path_protocol) || is.null(path_protocol))
  # Match output format
  format <- match.arg(format, c("data.frame", "list"), several.ok = FALSE)

  # If is null, load protocol
  template <- load_protocol(path_protocol = path_protocol)

  # Names of field indicators
  exportVals <- names(rvtl(results))

  # Data.frame formatting
  if(format == "data.frame"){
    protocol <- protocol_to_table() |> dplyr::mutate(value = "")

    for(i in 1:length(exportVals)){
      if(any(exportVals[i] %in% protocol$render_id)) {
        val <- results[[exportVals[i]]]
        if(length(val)>0){
          if(is.data.frame(val)){
            val <- paste(val$forename, paste0(val$surename, " (",val$orcid,") "), sep = ",", collapse = "; ")
          }
          # Time slide correction
          if(length(val)==2){
            val <- paste(val[1], val[2], sep = ",")
          }
          protocol$value[which(protocol$render_id == exportVals[i])] <- val
        }
      }
    } # List format
  } else if(format == "list"){
    protocol <- list()
    protocol[["overview"]] <- list()
    protocol[["design"]] <- list()
    protocol[["specification"]] <- list()
    protocol[["context"]] <- list()
    protocol[["prioritization"]] <- list()

    for(i in 1:length(exportVals)){
      # Get group
      gr <- get_protocol_elementgroup(exportVals[i])
      if(is.null(gr)) next()
      protocol[[gr$group]][[exportVals[i]]] <- results[[exportVals[i]]]
    }
  }
  return(protocol)
}

#' List to table
#'
#' @description
#' A helper function that converts a list to a table. Used internally
#' by format_protocol.
#'
#' @param protocol A filled out protocol in [`list`] format.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
protocol_to_table <- function(path_protocol = NULL){
  assertthat::assert_that(is.character(path_protocol) || is.null(path_protocol))

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  # Make a grand table
  # FIXME: Lazy coding but works
  results <- data.frame()
  for(gr in names(template)[-1]){
    pp <- template[[gr]]
    for(element in names(pp)){
      ppp <- pp[[element]]

      # Now assign to master table
      results <- dplyr::bind_rows(
        results,
        data.frame(group = gr, element = element,
                   render_id = ppp[["render-id"]],
                   question = ppp[["question"]])
      )
      # If there is an other id, add that as well
      if("fieldtype_conditional_render-id" %in% names(ppp)){
        results <- dplyr::bind_rows(
          results,
          data.frame(group = gr, element = element,
                     render_id = ppp[["fieldtype_conditional_render-id"]],
                     question = "Other details")
        )
      }
    }
  }
  return(results)
}
