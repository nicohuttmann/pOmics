#' Returns all files in the Data folder excluding RData
#'
#' @param dir directory of files
#' @param exclude expression based on which to remove files
#'
#' @return
#' @export
#'
#'
list_files<- function(dir, exclude = c("README", ".RData")) {

  objects <- list.files(path = dir, include.dirs = F, recursive = T)


  # Remove files from list
  for (i in exclude) {

    objects <- objects[!grepl(i, objects)]

  }

  return(paste(dir,
               objects,
               sep = ifelse(substring(dir, nchar(dir)) == "/", "", "/")))

}
