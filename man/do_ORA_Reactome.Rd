% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_ORA_Reactome.R
\name{do_ORA_Reactome}
\alias{do_ORA_Reactome}
\title{Performs over-representation analysis using Reactome annotations}
\usage{
do_ORA_Reactome(
  proteins,
  pvalueCutoff = 0.05,
  pAdjustMethod = "none",
  qvalueCutoff = 0.2,
  minGSSize = 10,
  maxGSSize = 500,
  dataset,
  view = F,
  return.all = F,
  add.info = F
)
}
\arguments{
\item{proteins}{numeric/logical vector of proteins indicating group}

\item{pvalueCutoff}{p-value cutoff for annotations}

\item{pAdjustMethod}{one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"}

\item{qvalueCutoff}{q-value cutoff for annotations}

\item{minGSSize}{minimum number of annotated proteins to be included}

\item{maxGSSize}{maximum number of annotated proteins to be included}

\item{dataset}{dataset}

\item{view}{view results}

\item{return.all}{return enrichResult object; useful for further analysis of enrichment results}

\item{add.info}{add additional information to the results data frame}
}
\description{
Performs over-representation analysis using Reactome annotations
}
