#' Creates button to hide plots and tables
#'
#' @param label button label
#'
#' @return
#' @export
#'
#'
button_begin <- function(label = "View/Hide") {

  # Buttons set up
  check_buttons()

  # Raise button counter
  next_button()

  # Paste names
  code <- paste0('<button class="btn btn-primary" data-toggle="collapse" data-target="#',
                 .n.button,
                 '"> ',
                 label,
                 ' </button> <div id="',
                 .n.button,
                 '" class="collapse">  ')

  # return unevaluated code
  return(noquote(code))

}
