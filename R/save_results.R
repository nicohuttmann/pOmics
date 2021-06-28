#' Store analysis object in Analysis list
#'
#' @param data data to be saved
#' @param name name of data
#' @param replace replace existing element with same name
#'
#' @return
#' @export
#'
#'
save_results <- function(data, name, replace) {

  # Check input
  if (!hasArg(data)) stop("Data is missing.")

  # Check if name given
  name <- ask_name(name = name, message = "Name of analysis part: ")


  # Check Analysis list
  new_analysis_list(silent = TRUE, replace = FALSE)

  # Check name
  if (name %in% names(Analysis)) {

    # Replace not given
    if (!hasArg(replace)) {

      # Choose how to continue
      todo <- menu(c("Rename new analysis", "Replace old analysis", "Stop"))

      # Abort mission
      if (todo %in% c(0, 3)) return(invisible(data))


      # No replace so indicate that name must be changed
    } else if (!replace) {

      todo <- 1

    } else {

      todo <- 2

    }



    # Rename
    while (todo == 1 && name %in% names(Analysis)) {
      name <- ask_name(message = "New name: ")
    }

  }

  # Add new entry to Analysis list
  Analysis[[name]] <<- data

  # Return invisibly
  return(invisible(data))

}
