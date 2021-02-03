#' Adds p-values to geom_segment dataframe fo rdendrogram plot
#'
#' @param segment segment data
#' @param dend.enrich dend.table.enrichment
#'
#' @return
#' @export
#'
#'
add_segment_pvalue <- function(segment, dend.enrich) {
  
  
  # Get 
  dend.table.pvalue <- -log10(dend.enrich[["dend.table.pvalue"]][!duplicated(dend.enrich[["dend.table.cluster"]][, ncol(dend.enrich[["dend.table.cluster"]])]), ])
  dend.table.cluster <- dend.enrich[["dend.table.cluster"]][!duplicated(dend.enrich[["dend.table.cluster"]][, ncol(dend.enrich[["dend.table.cluster"]])]), ]
  rownames(dend.table.pvalue) <- c()
  rownames(dend.table.cluster) <- c()
  
  
  # Max cluster cut where pvalues where calculated
  n <- ncol(dend.table.cluster)
  
  
  segment <- segment_data_y
  
  segment$k <- NA
  segment$l <- NA
  
  
  
  # Define first cluster cut manually
  x <- sort(unique(segment$x), decreasing = TRUE)[1]
  # Add k for y lines
  segment[(segment$x == x) %and% (segment$xend == x), "k"] <- 1
  # Add l for y lines (inherited from previous cluster)
  segment[(segment$x == x) %and% (segment$xend == x), "l"] <- 1
  # Add k for x lines
  segment[(segment$x == x) %and% (segment$xend != x), "k"] <- 1 + 1
  # Add l for y lines (inherited from previous cluster)
  segment[(segment$x == x) %and% (segment$xend != x), "l"] <- 1:(1 + 1)
  
  
  
  # Do for all other
  for (i in 2:(n - 1)) {
    
    x <- sort(unique(segment$x), decreasing = TRUE)[i]
    
    # Add k for y lines
    segment[(segment$x == x) %and% (segment$xend == x), "k"] <- i
    
    # Add l for y lines (inherited from previous cluster)
    segment[(segment$x == x) %and% (segment$xend == x), "l"] <- segment[(segment$x != x) %and% (segment$xend == x), "l"]
    
    # Add k for x lines
    segment[(segment$x == x) %and% (segment$xend != x), "k"] <- i + 1
    
    # Add l for y lines (inherited from previous cluster)
    segment[(segment$x == x) %and% (segment$xend != x), "l"] <- i:(i + 1)
    
  }
  
  
  # Fill rest (all inherited)
  for (i in n :length(unique(segment$x))) {
    
    x <- sort(unique(segment$x), decreasing = TRUE)[i]
    
    segment[(segment$x == x), c("k", "l")] <- segment[(segment$x != x) %and% (segment$xend == x), c("k", "l")]
    
  }
  
  
  # Add pvalues
  segment$logpvalue <- 0
  
  for (i in 1:nrow(segment)) {
    segment[i, "logpvalue"] <- dend.table.pvalue[match(segment[i, "l"], dend.table.cluster[, segment[i, "k"]]),
                                                 segment[i, "k"]]
  }
  
  
  # Return
  return(segment)
  
  
}
