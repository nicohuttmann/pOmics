% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simplify_GO_terms_semantic.R
\name{simplify_GO_terms_semantic}
\alias{simplify_GO_terms_semantic}
\title{Removes similar GO terms and keeps terms in order (assumes terms are ordered by p-value)}
\usage{
simplify_GO_terms_semantic(
  terms,
  similarity.measure = "Wang",
  similarity.threshold = 0.4,
  dataset
)
}
\arguments{
\item{terms}{vector of GO terms}

\item{similarity.measure}{method to measure pairwise GO terms similarity (see ?GOSemSim::mgoSim)}

\item{similarity.threshold}{threshold to remove terms (0-1, default = 0.4)}

\item{dataset}{dataset}
}
\value{
Named logical vector
}
\description{
Removes similar GO terms and keeps terms in order (assumes terms are ordered by p-value)
}
