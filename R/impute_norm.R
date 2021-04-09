#' Imputes values based on normal distribution
#'
#' @param data data (either a tibble, data frame or matrix)
#' @param shift shift in standard deviations
#' @param width width in standard deviations
#' @param seed seed
#'
#' @return
#' @export
#'
#'
impute_norm <- function(data, shift = 1.8, width = 0.3, seed = 123) {

  # Check input
  if (!hasArg(data)) stop("No data given.")

  # Set seed
  set.seed(seed = seed)

  # Impute values
  if (tibble::is_tibble(data) | is.data.frame(data)) {

    data <- data %>%
      dplyr::mutate(across(.cols = where(function(x) is.numeric(x) & any(x == 0)), .fns = function(x) {
        x.log2 <- log2(x)
        x.log2[is.infinite(x.log2)] <- NA
        x[x == 0] <- round(2 ^ rnorm(n = sum(x == 0),
                                     mean = mean(x.log2, na.rm = TRUE) - shift * sd(x.log2, na.rm = TRUE),
                                     sd = width * sd(x.log2, na.rm = TRUE)),
                           digits = -2)
        x
      }
      ))

  } else if (is.matrix(data)) {

    # Prepare log2 data frame
    data.log2 <- data
    data.log2[data.log2 == 0] <- NA
    data.log2 <- log2(data.log2)

    set.seed(seed)

    # Impute
    for (i in which(apply(X = data, MARGIN = 2, FUN = function(x) any(x == 0)))) {

      data[data[, i] == 0, i] <- round(2 ^ rnorm(n = sum(data[, i] == 0),
                                                 mean = mean(data.log2[, i], na.rm = TRUE) - shift * sd(data.log2[, i], na.rm = TRUE),
                                                 sd = width * sd(data.log2[, i], na.rm = TRUE)),
                                       digits = -2)

    }

  } else {

    stop("Please provide a tibble, data.frame or a matrix as data input.")

  }

  # Return
  return(data)

}
