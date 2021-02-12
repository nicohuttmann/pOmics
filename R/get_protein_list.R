#' Returns lists of proteins
#'
#' @param name name of protein list
#'
#' @return
#' @export
#'
#'
get_protein_list <- function(name) {

  #
  if (!hasArg(name)) {

    #
    if (!check_database(type = "Protein_lists")) stop("Protein_lists does not exist.")

      x <- menu(choices = names(.databases[["Protein_lists"]]), title = "Choose protein list: ")

      if (x == 0) return(NA)

      else return(.databases[["Protein_lists"]][[x]])


  } else if(check_database(id = name, type = "Protein_lists")) {

    return(get_database(id = name, type = "Protein_lists"))
  }

}
