#' This function loads the protocol
#'
#' @description
#' Utility function to load the ODPSCP protocol.
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`list`] object.
#'
#' @noRd
load_protocol <- function(path_protocol = NULL){
  assertthat::assert_that(
    is.null(path_protocol) || is.character(path_protocol)
  )
  if(is.null(path_protocol)){
    path_protocol <- system.file("01_protocol.yaml",
                                 package = "ODPSCP",
                                 mustWork = TRUE)
  }
  assertthat::assert_that(file.exists(path_protocol))

  # Load the protocol
  pp <- yaml::read_yaml(path_protocol)
  return(pp)
}

#' Get render ids from the protocol
#'
#' @description
#' Utility function to get all ids from a protocol
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`character`] object with all ids.
#'
#' @noRd
get_protocol_ids <- function(path_protocol = NULL, group = NULL){
  # Load the protocol
  protocol <- load_protocol(path_protocol)

  if(!is.null(group)){
    group <- match.arg(tolower(group), c("protocol","overview",
                                       "design","specification",
                                       "context","prioritization"), several.ok = FALSE)
    protocol <- protocol[[group]]
  }

  # Get all render ids
  pp <- unlist(protocol)
  pp <- pp[grep("render-id", names(pp))] |> as.character()
  assertthat::assert_that(length(pp)>0, is.character(pp))
  return(pp)
}

#' Get protocol version
#'
#' @description
#' Utility function to get the protocol version number
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`character`] with the current version number
#'
#' @noRd
get_protocol_version <- function(path_protocol = NULL){
  # Load the protocol
  protocol <- load_protocol(path_protocol)
  version <- protocol$protocol$version
  return(version)
}

#' Get protocol group for render-id
#'
#' @description
#' Utility function to get the group for a render-id
#' @param id A character with the id for which options are to be queried.
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @return A [`data.frame`] with the group and element
#'
#' @noRd
get_protocol_elementgroup <- function(id, path_protocol = NULL){
  assertthat::assert_that(
    is.character(id),
    is.null(path_protocol) || is.character(path_protocol)
  )
  # Load the protocol
  template <- load_protocol(path_protocol)

  # FIXME: Hacky, lazy coding from a train
  out <- data.frame()
  for(gr in names(template)[-1]){
    pp <- template[[gr]]
    for(element in names(pp)){
      ppp <- pp[[element]]
      if(ppp[['render-id']] == id){
        out <- dplyr::bind_rows(
          out,
          data.frame(group = gr, element = element, id = id)
        )
      }
      # For conditional render-ids
      if(utils::hasName(ppp, 'fieldtype_conditional_render-id')){
        if(ppp[['fieldtype_conditional_render-id']] == id){
          out <- dplyr::bind_rows(
            out,
            data.frame(group = gr, element = element, id = id)
          )
        }
      }
    }
  }
  if(nrow(out)==0) return( NULL )
  return(out)
}

#' Get protocol options if specified
#' @description
#' Utility function to get the options if for a render-id. Can also be used to
#' query other fields in the protocol.
#' @param id A character with the render_id.
#' @param path_protocol The filepath to the actual protocol template (Default: \code{NULL})
#' @param field A [`character`] of the field to be returned (Default: \code{"options"}).
#' @returns Either \code{NULL} or a vector with found options.
#' @examples
#' /dontrun{
#'  get_protocol_options('featuretypes')
#' }
#'
#' @noRd
get_protocol_options <- function(id, path_protocol = NULL, field = "options"){
  assertthat::assert_that(is.character(id),
                          is.character(field),
                          is.character(path_protocol) || is.null(path_protocol))

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  check <- get_protocol_elementgroup(id)
  if(!is.null(check)){
    # Get the actual options
    if(field %in% names(template[[check$group]][[check$element]])){
      check <- template[[check$group]][[check$element]][[field]]
    } else { check <- NULL }
  }
  return(check)
}

#' Check for mandatory fields to be filled
#'
#' @description
#' Small convenience function that checks for mandatory fields
#'
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @returns A [`vector`] of character entries that are mandatory in the protocol.
#' @noRd
get_protocol_mandatory <- function(path_protocol = NULL){
  assertthat::assert_that(is.character(path_protocol) || is.null(path_protocol))

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  results <- vector()
  for(gr in names(template)[-1]){
    pp <- template[[gr]]
    for(element in names(pp)){
      ppp <- pp[[element]]
      if(ppp$mandatory) results <- append(results, values = ppp[['render-id']] )
    }
  }
  return(results)
}

#' Check mandatory fields in results
#'
#' @description
#' This small helper check whether mandatory entries in the results have been filled.
#' @param results A [`list`] with the protocol results.
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @returns A [`vector`] of mandatory character entries that missing.
#' @noRd
check_protocol_mandatory <- function(results, path_protocol = NULL){
  # Checks protocol
  assertthat::assert_that(is.character(path_protocol) || is.null(path_protocol))
  assertthat::assert_that(is.list(results))

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  # If missing, load again
  mand <- get_protocol_mandatory(path_protocol)

  out <- vector()
  for(gr in names(template)[-1]){
    pp <- template[[gr]]
    for(element in names(pp)){
      ppp <- pp[[element]]
      # Now check if present in results
      op <- results[[gr]][[element]]$value
      if(is.null(op) || op==""){
        if(ppp[['render-id']] %in% mand) out <- append(out, ppp[['render-id']])
      }
    }
  }
  return(out)
}

#' Validate loaded protocol results
#'
#' @description
#' This small helper check whether a loaded results file is actually valid.
#' In the case it is not
#' @param results A [`list`] with the protocol results.
#' @param path_protocol A [`character`] pointing to the destination of the protocol.
#' @returns Either \code{NULL} in case of no issues or a [`character`] with a error message.
#' @noRd
validate_protocol_results <- function(results, path_protocol = NULL){
  # Checks protocol
  assertthat::assert_that(is.character(path_protocol) || is.null(path_protocol))
  assertthat::assert_that(is.list(results) || is.data.frame(results),
                          length(results)>1)

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  # --- #
  # Entries missing
  if(is.data.frame(results)){
    if(!all(results$group %in% names(template))){
      return("The loaded file is missing the protocol groups!")
    }
  } else {
    if(!all(names(results) %in% names(template))){
      return("The loaded file is missing the protocol groups!")
    }
  }

  # Check specifically for data.frame
  if(is.data.frame(results)){
    if(!utils::hasName(results, "group") || !utils::hasName(results, "render_id")){
      return("Necessary entries not found in uploaded data.frame!")
    }
  }

  # Check file ids
  if(is.data.frame(results)){
    ids <- results$render_id
  } else {
    ids <- lapply(results, function(z) names(z)) |> unlist()
  }
  # Check that ids actually exist
  if(!all(ids %in% get_protocol_ids(path_protocol = path_protocol))){
    return("Not all ids in the protocol file do exist in the current protocol!")
  }

  # Check mandatory entries
  if(!all(get_protocol_mandatory(path_protocol = path_protocol)) %in% ids){
    return("Exported protocol does not contain all mandatory groups!")
  }
  # --- #
  return(NULL)
}
