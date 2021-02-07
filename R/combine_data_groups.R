#' Sums up numerical and logical data groupwise
#'
#' @param data data
#' @param groups groups
#' @param method method to use for combination
#'
#' @return
#' @export
#'
#'
combine_data_groups <- function(data, groups, method) {



  groups <- groups[rownames(data)]


  data.combined <- matrix(0, nrow = length(unique(groups)), ncol = ncol(data))
  rownames(data.combined) <- unique(groups)
  colnames(data.combined) <- colnames(data)


  # Check combination method
  if(length(method) != 1 || !method %in% c("mean", "median", "sum", "max", "min", "sd", "and", "or"))
    stop("Combination method not found.")


  if(method == "mean") {

    for(i in 1:nrow(data.combined)) {
      for(j in 1:ncol(data.combined)) {
        data.combined[i, j] <- mean(data[groups == rownames(data.combined)[i], j], na.rm = T)
      }
    }

  }

  else if(method == "median") {

    for(i in 1:nrow(data.combined)) {
      for(j in 1:ncol(data.combined)) {
        data.combined[i, j] <- median(data[groups == rownames(data.combined)[i], j], na.rm = T)
      }
    }

  }

  else if(method == "sum") {

    for(i in 1:nrow(data.combined)) {
      for(j in 1:ncol(data.combined)) {
        data.combined[i, j] <- sum(data[groups == rownames(data.combined)[i], j], na.rm = T)
      }
    }

  }

  else if(method == "max") {

    for(i in 1:nrow(data.combined)) {
      for(j in 1:ncol(data.combined)) {
        data.combined[i, j] <- max(data[groups == rownames(data.combined)[i], j], na.rm = T)
      }
    }

  }

  else if(method == "min") {

    for(i in 1:nrow(data.combined)) {
      for(j in 1:ncol(data.combined)) {
        data.combined[i, j] <- min(data[groups == rownames(data.combined)[i], j], na.rm = T)
      }
    }

  }

  else if (method == "sd") {

    for(i in 1:nrow(data.combined)) {
      for(j in 1:ncol(data.combined)) {
        data.combined[i, j] <- sd(data[groups == rownames(data.combined)[i], j], na.rm = T)
      }
    }

  }

  else if(method == "and") {

    for(i in 1:nrow(data.combined)) {

      for(j in 1:ncol(data.combined)) {

        data.combined[i, j] <- data[groups == rownames(data.combined)[i], j][1]

        for(k in 1:sum(groups == rownames(data.combined)[i])) {

          data.combined[i, j] <- combineAND(data.combined[i, j], data[groups == rownames(data.combined)[i], j][k])

        }

      }

    }

  }

  else if(method == "or") {

    for(i in 1:nrow(data.combined)) {

      for(j in 1:ncol(data.combined)) {

        data.combined[i, j] <- data[groups == rownames(data.combined)[i], j][1]

        for(k in 1:sum(groups == rownames(data.combined)[i])) {

          data.combined[i, j] <- combineOR(data.combined[i, j], data[groups == rownames(data.combined)[i], j][k])

        }

      }

    }

  }

  return(data.combined)

}
