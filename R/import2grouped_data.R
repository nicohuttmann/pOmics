#' Stores data frame in a list and groups adjacent columns
#'
#' @param import imported data frame
#'
#' @return
#' @export
#'
#'
import2grouped_data <- function(import) {

  # List to be filled with grouped columns
  grouped_data <- tibble::lst(variables.data = tibble::tibble(.rows = nrow(import)),
                       data.frames = tibble::lst())


  observations <- identify_observations(import)

  column.names <- gsub(paste(observations, collapse = "|"), "", x = colnames(import))



  # # TRUE means grouped column
  # column.type <- column.names %in% which_names(table(column.names) == max(table(column.names)))

  # Add data to list
  for (i in which(!duplicated(column.names))) {

    # variables data
    if (sum(column.names[i] == column.names) == 1) {

      grouped_data[["variables.data"]] <- grouped_data[["variables.data"]] %>%
        dplyr::mutate(!!column.names[i] := dplyr::pull(import, which(column.names[i] == column.names)))

    # Data frames
    } else {


      #
      grouped_data[["data.frames"]][[column.names[i]]] <- dplyr::select(import, which(column.names[i] == column.names))

      colnames(grouped_data[["data.frames"]][[column.names[i]]]) <- gsub(pattern = column.names[i],
                                                                         replacement = "",
                                                                         x = colnames(grouped_data[["data.frames"]][[column.names[i]]]))

      names(grouped_data[["data.frames"]])[length(grouped_data[["data.frames"]])] <- ifelse(substring(text = column.names[i], first = nchar(column.names[i])) == ".",
                     substring(text = column.names[i], first = 1, nchar(column.names[i]) - 1),
                     column.names[i])
    }

  }





  # Return list
  return(grouped_data)

}
