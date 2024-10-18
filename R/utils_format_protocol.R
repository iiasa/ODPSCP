#' Protocol format wrapper function
#'
#' @description
#' A utility function to format a filled out protocal into a different format.
#' Supported options are:
#' * data.frame
#'
#' @param results A filled out protocol in [`list`] or reactive format.
#' @param format The output format that the results should have. Available options
#' include \code{"data.frame"} or \code{"list"}.
#' @param studyregiondummy A [`logical`] flag of whether the studyregion should be replaced with a dummy?
#' @param path_protocol The filepath to the actual protocol template.
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
format_protocol <- function(results, format = "data.frame",
                            studyregiondummy = FALSE, path_protocol = NULL){
  assertthat::assert_that(is.list(results),
                          is.character(format),
                          is.logical(studyregiondummy),
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
          # For studyregion:
          if(utils::hasName(val, "datapath")){
            if(studyregiondummy){
              val <- "Study region provided!"
            } else {
              # Format EPSG and WKT to text
              val <- format_studyregion_to_text(val)
            }
          } else if(is.data.frame(val)){
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
      # Get result
      val <- results[[exportVals[i]]]
      if(utils::hasName(val, "datapath")){
        val <- format_studyregion_to_text(val)
      }
      protocol[[gr$group]][[exportVals[i]]] <- val
    }
  }
  return(protocol)
}

#' Small helper for spatial conversion to wkt
#' @param val A [`list`] with the datapath for the spatial file
#' @return A [`character`] with a WKT.
#' @noRd
format_studyregion_to_text <- function(val){
  assertthat::assert_that(utils::hasName(val, "datapath"))
  # Load from data path
  val <- try({
    spatial_to_sf(val$datapath, make_valid = FALSE)
  }, silent = TRUE)
  if(inherits(val,"try-error")){
    val <- "Studyregion could not be loaded?"
  } else {
    # Convert to sfc
    val <- val |> sf::st_as_sfc()
    val <- paste0(
      # Also append SRID in front
      sf::st_crs(val) |> sf::st_as_text(),";", sf::st_as_text(val)
    )
  }
  return(val)
}

#' List to table
#'
#' @description
#' A helper function that converts a list to a table. Used internally
#' by format_protocol.
#'
#' @param path_protocol A filled out protocol in [`list`] format.
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

#' Format a results list to document
#'
#' @description
#' This helper function create a markdown document that is then rendered in
#' different outputs using the \code{"pandoc"} R-package.
#'
#' Most documentation and helper texts are taken from the protocol text with
#' only the results being entered in this sheet.
#'
#' @param results A filled out protocol in [`list`] format.
#' @param file A [`character`] with the output file name.
#' @param format A [`character`] indicating the output format. Either \code{"html"},
#' \code{"docx"} or \code{"pdf"}.
#' @return NULL
#'
#' @noRd
protocol_to_document <- function(results, file, format = "docx", path_protocol = NULL){
  assertthat::assert_that(is.character(path_protocol) || is.null(path_protocol))
  # Check for other inputs
  assertthat::assert_that(is.list(results),
                          is.character(file))
  # Match the format
  format <- match.arg(format, c("html", "docx", "pdf"), several.ok = FALSE)
  # Add docx by default (also for pandoc)
  if(tools::file_ext(file) != "docx") file <- paste0(tools::file_path_sans_ext(file), ".", "docx")

  # If is null, load protocol
  template <- load_protocol(path_protocol)

  # Formatted document header
  fpar <- officer::fpar(
    officer::ftext(text = paste0(template$protocol$name, " Protocol Version: ", template$protocol$version),
                   prop = officer::fp_text(font.size = 24,bold = TRUE))
  )

  # Create basic file
  doc <- officer::read_docx() |>
    officer::body_add_fpar(value = fpar) |>
    officer::body_add_fpar(value = officer::fpar( officer::ftext(text = paste0("Created through ", template$protocol$repository),
                                                                   prop = officer::fp_text(font.size = 14,bold = FALSE)) )) |>
    officer::body_add_fpar(value = officer::fpar( officer::ftext(text = paste0("Generated on ", Sys.Date() ),
                                                  prop = officer::fp_text(font.size = 14,bold = FALSE)))) |>
    officer::body_add_par("Protocol content", style = "heading 1") |>
    officer::body_add_toc(level = 2) |>
    officer::body_add_break()

  # Now per group and element add to output
  for(g in names(results)){ # g = names(results)[1]

    # Add a new header for group
    doc <- doc |> officer::body_add_par(value = tools::toTitleCase(g), style = "heading 1")

    for(el in names(results[[g]])){ # el = names(results[[g]])[1]

      # Get the name and description of the target element
      w <- get_protocol_elementgroup(el)
      if(is.null(w)) next() # Skip?
      sub <- template[[w[['group']]]][[w[['element']]]]

      # --- #
      ## Now add to output specifically
      # The question
      fpar <- officer::fpar(
        officer::ftext(text = paste0(sub$question),
                       prop = officer::fp_text(font.size = 14, bold = TRUE))
      )
      doc <- doc |> officer::body_add_fpar(value = fpar)
      # A description there in
      fpar <- officer::fpar(
        officer::ftext(text = paste0(sub$description),
                       prop = officer::fp_text(font.size = 10,italic = TRUE))
      )
      doc <- doc |> officer::body_add_fpar(value = fpar)

      # Small linebreak
      doc <- doc |> officer::body_add_par(value = "", style = "Normal")

      # --- #
      # Parse the result
      res <- results[[g]][[el]]
      # Specific function for studyregion rendering
      if(el == "studyregion"){
        # Render studyregion
        sp <- strsplit(res,";") # Split SRID off
        # Catch error in case region could not be loaded
        if(is.na(sp[[1]][2])){
          # Add to body
          fpar <- officer::fpar(
            officer::ftext(text = sp[[1]][1],
                           prop = officer::fp_text(font.size = 12,italic = FALSE))
          )
          doc <- doc |> officer::body_add_fpar(value = fpar)
        } else {
          # Correctly parsed geometry
          sp <- sp[[1]][2] |> sf::st_as_sfc() |> sf::st_sf(crs = sp[[1]][1])
          gg <- ggplot2::ggplot() +
            ggplot2::geom_sf(data = sp) +
            ggplot2::labs(title = "Outline of studyregion")
          # Add to document
          doc <- doc |> officer::body_add_gg(value = gg)
          try({ rm(gg)},silent = TRUE)
        }
        try({ rm(sp) },silent = TRUE)
      } else if(el %in% c("authors_table","featurelist",
                          "evalidentification","specificzones")) {

        # Lists for example for table
        if(is.list(res)) {
          if(length(res)>0){
            # Tables
            ft <- flextable::flextable( dplyr::bind_rows(res) ) |>
              flextable::set_table_properties(layout = "autofit")
            doc <- doc |> flextable::body_add_flextable(value = ft)
          } else {
            res <- "Not specified"
          }
        }
      } else {
        # All other entries
        if(all(is.logical(res))) res <- ifelse(res, "Yes", "No")
        if(all(is.na(res))) res <- "Not specified"

        # If multiple entries, paste together via -
        if(length(res)>1) res <- paste(res, collapse = " - ")

        # Add to body
        fpar <- officer::fpar(
          officer::ftext(text = res,
                         prop = officer::fp_text(font.size = 12,italic = FALSE))
        )
        doc <- doc |> officer::body_add_fpar(value = fpar)

      }
      # Small linebreak
      doc <- doc |> officer::body_add_par(value = "", style = "Normal")

    }
  }
  # Generate output to file
  doc |> print(target = file)

  # Transform the output to different type depending on setting
  if(format != "docx"){
    rmarkdown::pandoc_convert(
      input = file,
      to = format,
      output = paste0(tools::file_path_sans_ext(file), ".", format)
    )
  }
}
