#' Creates basic data structure
#'
#' @param replace replace existing lists
#'
#' @return
#' @export
#'
#'
initialize_data_structure <- function(replace = F) {
  
  new_info_list(replace = replace)
  
  new_datasets_list(replace = replace)
  
  new_imports_list(replace = replace)
  
  new_cache(replace = replace)
  
}


#' Creates imports list
#'
#' @param replace Should imports list be replaced if already existing
#'
#' @return
#' @export
#'
#'
new_imports_list <- function(replace = F) {
  
  #
  if (!".imports" %in% objects(all.names = T, envir = .GlobalEnv) || replace) {
    .imports <<- tibble::lst()
    
    # Indicate if new info list was created
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
  
}


#' Creates new info list
#'
#' @param replace Should info list be replaced if already existing
#'
#' @return logical indication if new .info list was created
#' @export
#'
#'
new_info_list <- function(replace = F) {
  
  #
  if (!".info" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .info <<- tibble::lst("default_dataset" = NA,
                          "raw_datasets" = tibble::lst())
    
    # Defaults data
    new_default_data()
    
    
    # Indicate if new info list was created
    return(invisible(TRUE))
    
  } else {
    return(invisible(FALSE))
  }
  
}


#' Creates datasets list
#'
#' @param replace Should datasets list be replaced if already existing
#'
#' @return
#' @export
#'
#'
new_datasets_list <- function(replace = F) {
  
  #
  if (!".datasets" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .datasets <<- tibble::lst()
    
    # Indicate if new datasets list was created
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}


#' Creates databases list
#'
#' @param replace Should databases list be replaced if already existing
#'
#' @return
#' @export
#'
#'
new_databases_list <- function(replace = F) {
  
  #
  if (!".databases" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .databases <<- tibble::lst()
    
    # Indicate if new databases list was created
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}


#' Creates new cache list
#'
#' @param replace Should cache list be replaced if already existing
#'
#' @return whether a new cache list was created
#' @export
#'
#'
new_cache <- function(replace = T) {
  
  #
  if (!".cache" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .cache <<- tibble::lst()
    
    
    # Indicate if new cache list was created
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
  
}


#' Add or replace analysis list
#'
#' @param silent disable messages
#' @param replace replace if list already exists
#'
#' @return
#' @export
#'
#'
new_analysis_list <- function(silent = F, replace = F) {
  
  # Analysis list already in global environment
  if ("Analysis" %in% ls(all.names = T, pos = .GlobalEnv)) {
    
    # Should analysis list be replaced
    if (replace) {
      
      # Assign new list
      Analysis <<- tibble::lst()
      
      # Message (optional)
      if (!silent) message("Analysis list replaced.")
      
      
    } else {
      
      # Do nothing; optional message
      if (!silent) message("Analysis list already exists.")
      
    }
    
    
    # Analysis list not found
  } else {
    
    # Assign new list
    Analysis <<- tibble::lst()
    
    # Message (optional)
    if (!silent) message("New Analysis list added.")
    
  }
  
}
