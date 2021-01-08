#' Stores data frame in a list and groups adjacent columns
#'
#' @param x data frame
#' @param min.similarity minimal similarity of column names
#' @param min.groupsize minimal group size
#'
#' @return grouped list of data frame columns
#' @export
#'
#'
df2groupedList <- function(x, min.similarity = 8, min.groupsize = 6) {

  # List to be filled with grouped columns
  grouped.list <- list()


  # Add all columns to the list
  # done will be checke after each iteration based on column number
  done <- FALSE
  # from is reset everytime columns are transferred to the list
  from <- 1
  # Loop will stop when last column has been added to the grouped.list
  for (i in 2:ncol(x)) {

    # Compare data type
    if (mode(x[[i - 1]]) != mode(x[[i]])) {
      # Add column/s
      grouped.list <- add.group.columns(grouped.list = grouped.list,
                          x = x,
                          from = from,
                          to = i - 1,
                          separate = (i - from) < min.groupsize)
      # Set new start of column group
      from <- i

      # If data type is the same, check names for similarity
      # If not similar; add
    } else if (!same.prefix(names = colnames(x)[from:i],
                              min.similarity = min.similarity,
                              enforce.residue = T)) {

      # Add column/s
      grouped.list <- add.group.columns(grouped.list = grouped.list,
                          x = x,
                          from = from,
                          to = i - 1,
                          separate = (i - from) < min.groupsize)
      # Set new start of column group
      from <- i

      # If i and i - 1 belong to the same column group only check if it is the last
    } else if (i == ncol(x)){

      # Add column/s
      grouped.list <- add.group.columns(grouped.list = grouped.list,
                          x = x,
                          from = from,
                          to = i,
                          separate = (i - from) < min.groupsize)

    }

  }

  # Return list
  return(grouped.list)

}
