#' Turns identification dataframe into list of variable entries
#'
#' @param data dataframe
#'
#' @return
#' @export
#'
#'
logical_df2list <- function(data) {

  # Check data
  if (typeof(data[[1]]) != "character" || any(unlist(lapply(data[-1], typeof)) != "logical"))
    stop("Dataframe must consist of one character and n logical columns.")


  data.list <- tibble::lst()

  for (i in colnames(data)[-1]) {

    data.list[[i]] <- data %>%
      dplyr::filter(rlang::eval_tidy(rlang::parse_expr(paste0("`", i, "`")))) %>%
      dplyr::pull(1)

  }


  # Return
  return(data.list)

}
