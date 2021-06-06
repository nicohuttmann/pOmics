#' Interface for user to define a list of GO terms
#'
#' @param return.ID return GO ID or terms
#' @param graphics Choose from graphic interface
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
choose_GO_terms <- function(return.ID = F, graphics = F) {

  # Check database
  if (!check_database("GO-ID")) new_annotations_data_UniProt(annotation = "GO-ID", save = TRUE)


  # Get annotations
  annotations <- TERM2Gene

  # Prepare annotations
  annotations.df <- data.frame(Term = AnnotationDbi::Term(names(annotations)),
                               Proteins = unlist(lapply(annotations, length)))
  annotations.df <- annotations.df[order(annotations.df$Term), ]

  terms.select <- paste0(annotations.df$Term, " (", annotations.df$Proteins, ")")
  names(terms.select) <- rownames(annotations.df)

  terms.view <- rownames(annotations.df)
  names(terms.view) <- annotations.df$Term

  terms <- rownames(annotations.df)



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
                            rownames(annotations.df)[
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
        while (is.na(min) || min < 0 || min > max(annotations.df[terms, "Proteins"])) {

          min <- ask_name(message = "Minimum number of proteins per term: ")
          min <- suppressWarnings(as.numeric(min))

        }

        # Assign new list of terms
        terms <- intersect(terms,
                           rownames(annotations.df)[annotations.df$Proteins >= min])

      }

      # Start from beginning
      if (x == 4) {

        terms <- rownames(annotations.df)

      }


      # View current dataframe
      View(annotations.df[terms, ], title = "GO terms")

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

      # Select terms
      y <- utils::select.list(choices = terms.select[terms],
                              multiple = TRUE,
                              title = "Choose GO terms: ",
                              graphics = graphics)

      y <- terms.view[match(names(terms.select)[terms.select %in% y], terms.view)]

      # Return ID or terms
      if (return.ID) {
        return(y)
      } else {
        y2 <- names(y)
        names(y2) <- y
        return(y2)
      }

    }


    # Return NULL as no action
  } else {
    invisible(NULL)
  }

}
