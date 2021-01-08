#' Test vector of strings for a common prefix of minimal given length
#'
#' @param names vector of strings
#' @param min.similarity minimal similarity of prefix
#' @param enforce.residue is a character after the prefix required
#'
#' @return TRUE if common prefix exists
#' @export
#'
#'
same_prefix <- function(names, min.similarity = 8, enforce.residue = T) {

  # Common prefix of all strings
  prefix <- common_prefix(names)

  # Is a character after the prefix required
  if (enforce.residue)
    return(nchar(prefix) >= min.similarity && !(prefix %in% names))

  # Only checks length of common prefix
  else
    return(nchar(prefix) >= min.similarity)

}
