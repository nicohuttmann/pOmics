#' Interface for user to define a list of GO terms
#'
#' @param variables variables
#' @param TERM2GENE TERM2GENE dataframe
#' @param graphics Choose from graphic interface
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
choose_terms <- function(proteins, TERM2GENE, graphics) {

  if (!hasArg(TERM2GENE)) {

    message("Please provide a TERM2GENE dataframe.")

    return(NULL)

  }


  if (hasArg(proteins)) {

    TERM2GENE <- TERM2GENE %>%
      dplyr::filter(GENE %in% proteins)

  }

  # Get annotations
  annotations.list <- TERM2GENE_2_list (TERM2GENE)

  # Prepare annotations
  annotations.df <- data.frame(Term = names(annotations.list),
                               Proteins = unlist(lapply(annotations.list, length)))


  annotations.df <- annotations.df[order(annotations.df$Proteins, decreasing = T), ]

  rownames(annotations.df) <- c()


  terms.select <- paste0(annotations.df$Term, " (", annotations.df$Proteins, ")")

  terms.view <- annotations.df$Term

  terms <- annotations.df$Term



  # View whole dataframe
  View(annotations.df, title = "GO terms")


  # What to do first
  x <- menu(choices = c("Choose terms from current list",
                        "Search for terms",
                        "Threshold terms by number of proteins",
                        "Stop"),
            title = "What would you like to do first?")


  # First action
  if (x %in% 1:3) {


    # Restrict terms prior to selection
    while (x %in% 2:4) {


      # Search terms in current selection
      if (x == 2) {

        # Intersect current terms with new terms
        terms2 <- intersect(terms,
                            annotations.df$Term[
                              grep(pattern = ask_name(message = "Provide complete term or pattern to be searched: "),
                                   x = annotations.df$Term, ignore.case = T)])

        # Check new list of terms
        if (length(terms2) == 0) {

          message("No terms found. Keeping current selection")

        # Assign new terms vector
        } else {
          terms <- terms2
        }

      }


      # Threshold terms by number of proteins
      if (x == 3) {

        min <- -1
        # Ask for number
        while (is.na(min) || min < 0 || min > max(annotations.df[annotations.df$Term %in% terms, "Proteins"])) {

          min <- ask_name(message = "Minimum number of proteins per term: ")
          min <- suppressWarnings(as.numeric(min))

        }

        # Assign new list of terms
        terms <- intersect(terms,
                           annotations.df[annotations.df$Proteins >= min, "Term"])

      }

      # Start from beginning
      if (x == 4) {

        terms <- annotations.df$Term

      }


      # View current dataframe
      View(annotations.df[annotations.df$Term %in% terms, ], title = "GO terms")

      # What to do next
      x <- menu(choices = c("Choose terms from current list",
                            "Search term in current selection",
                            "Threshold terms by number of proteins",
                            "Start from beginning",
                            "Stop"),
                title = "What would you like to do next?")

    }


    # Choose terms
    if (x == 1) {


      if (!hasArg(graphics)) {

        graphics <- c(T, F)[menu(choices = c("graphical window", "console"),
                                              title = "Choose from graphical window or console?")]
      }


      # Select terms
      y <- utils::select.list(choices = terms,
                              multiple = TRUE,
                              title = "Choose GO terms: ",
                              graphics = graphics)

      # Return terms
      return(y)

    }


    # Return NULL as no action
  } else {
    return(invisible(NULL))
  }

}
