#' Opens MSigDB entries
#'
#' @param names vector of entry names
#'
#' @return
#' @export
#'
#'
browse_msigdbr <- function(names = "MODULE_212") {
  
  # 
  for (i in name) {
    browseURL(paste0("https://www.gsea-msigdb.org/gsea/msigdb/", i))
  }
  
}
