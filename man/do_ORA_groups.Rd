% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_ORA_groups.R
\name{do_ORA_groups}
\alias{do_ORA_groups}
\title{Performs overexpression enrichment on protein group vectors}
\usage{
do_ORA_groups(
  proteins,
  database,
  pvalueCutoff = 0.05,
  pAdjustMethod = "none",
  qvalueCutoff = 0.2,
  minGSSize = 10,
  maxGSSize = 500,
  algorithm = "classic",
  dataset
)
}
\arguments{
\item{proteins}{character vector}

\item{database}{database to use}

\item{pvalueCutoff}{p-value/confidence threshold to exclude terms}

\item{pAdjustMethod}{one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"}

\item{qvalueCutoff}{q-value cutoff for annotations}

\item{minGSSize}{minimum number of proteins for annotation to be used for enrichment}

\item{maxGSSize}{maximum number of proteins for annotation to be used for enrichment}

\item{algorithm}{algorithm to use ("classic", "elim", "weight", "weight01")}

\item{dataset}{dataset}
}
\value{

}
\description{
Performs overexpression enrichment on protein group vectors
}
