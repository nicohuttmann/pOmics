#' Lists available template names from pOmics-templates repository
#'
#' @return
#' @export
#'
#'
.template_names <- function() {

  template_names <- c(`_template.R` =
                        "Script: Template",
                      `00_Import_data.R` =
                        "Script 00: Import data",
                      `01_Data_preparation.R` =
                        "Script 01: Data preparation (samples)",
                      `02_Data_cleanup.R` =
                        "Script 02: Data cleanup (proteins)",
                      `03_Setup_annotation_databases.R` =
                        "Script 03: Setup annotation databases")

  return(template_names)

}


#' Downloads template scripts and copies to clipboard
#'
#' @return
#' @export
#'
#'
copy_template <- function() {

  # Available template names
  template_names <- .template_names()


  # Choose template
  template_id <- menu(choices = template_names,
                      title =
                        "Which template should be copied to the clipboard?")


  # Notify when no template chosen
  if (template_id == 0) {
    message("No template has been downloaded.")
    return(invisible(FALSE))
  }


  # Template name
  template_name <- template_names[template_id]


  # Paste template url
  template_url <- paste0(
    "https://raw.githubusercontent.com/nicohuttmann/pOmics-templates/master/",
    template_file_names[template_id])


  # Copy template to clipboard
  utils::writeClipboard(readLines(template_url))


  # Message
  message(paste0("Template script <",
                 template_name,
                 "> has been copied to the clipboard."))


  return(invisible(TRUE))

}


#' Downloads template scripts and writes to new file
#'
#' @param folder where to save template script
#'
#' @return
#' @export
#'
#'
download_template <- function(folder = "Scripts") {


  # Available template names
  template_names <- .template_names()


  # Choose template
  template_id <- menu(choices = template_names,
                      title =
                        "Which template should be copied to the clipboard?")


  # Notify when no template chosen
  if (template_id == 0) {
    message("No template has been downloaded.")
    return(invisible(FALSE))
  }


  # Template name
  template_name <- template_names[template_id]


  # Paste template url
  template_url <- paste0(
    "https://raw.githubusercontent.com/nicohuttmann/pOmics-templates/master/",
    names(template_names)[template_id])


  # Check folder
  if (!dir.exists(folder)) {

    message("Selected folder could not be found.")

    folder <- choose.dir(caption =
                           "Choose the folder to save the template script in:")

  }


  # Check if file already exists
  destfile <- paste0(folder,
                     "\\",
                     names(template_names)[template_id])

  if (file.exists(destfile)) {
    message("File already exists. Please change the name of the existing file.")
    return(invisible(FALSE))
  }

  # Download template script
  download.file(url = template_url,
                destfile = destfile,
                quiet = TRUE)


  # Message
  message(paste0("Template script <",
                 template_name,
                 "> has been downloaded to ",
                 paste0(folder,
                        "\\",
                        names(template_names)[template_id]),
                 "."))


  return(invisible(TRUE))

}

