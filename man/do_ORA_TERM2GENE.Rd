% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_ORA_CORUM.R
\name{do_ORA_TERM2GENE}
\alias{do_ORA_TERM2GENE}
\title{Performs over-representation analysis using Gene Ontology annotations}
\usage{
do_ORA_TERM2GENE(
  proteins,
  pvalueCutoff = 0.05,
  pAdjustMethod = "none",
  qvalueCutoff = 0.2,
  minGSSize = 3,
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

\item{minGSSize}{minimum number of proteins for annotation to be used for enrichment}

\item{maxGSSize}{maximum number of proteins for annotation to be used for enrichment}

\item{dataset}{dataset}

\item{view}{view results}

\item{return.all}{return enrichResult object; useful for further analysis of enrichment results}

\item{add.info}{add additional information to the results data frame}
}
\value{

}
\description{
Performs over-representation analysis using Gene Ontology annotations
}
