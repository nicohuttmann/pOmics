#' Helper function for df2groupedList to add column/s to the list
#'
#' @param grouped.list list to be appended
#' @param x source data frame
#' @param from start column index
#' @param to end column index
#' @param separate should columns be added separately
#'
#' @return
#' @export
#'
add_group_columns <- function(grouped.list, x, from, to, separate) {

  # Add columns separately
  if (separate) {

    for (i in from:to) {

      # Add new list entry of transposed column/s
      grouped.list[[length(grouped.list) + 1]] <- x[[i]]
      # Add colnames after transposing data
      names(grouped.list[[length(grouped.list)]]) <- rownames(x)
      # Add name
      names(grouped.list)[length(grouped.list)] <- colnames(x)[i]

    }

  } else {

    #

    # Add new list entry of transposed column/s
    grouped.list[[length(grouped.list) + 1]] <- t(as.matrix(x[, from:to]))
    # Add name; common part if multiple rows
    names(grouped.list)[length(grouped.list)] <- common_prefix(colnames(x)[from:to])
    # Add colnames after transposing data
    colnames(grouped.list[[length(grouped.list)]]) <- rownames(x)
    # Add rownames w/out prefix
    rownames(grouped.list[[length(grouped.list)]]) <- substring(text = colnames(x)[from:to],
                                                                first = nchar(names(grouped.list)[length(grouped.list)]) + 1)

  }

  # Return list
  grouped.list

}
