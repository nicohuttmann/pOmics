% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_org.R
\name{select_org}
\alias{select_org}
\title{Queries data from Annotation packages}
\usage{
select_org(keys, columns, output = "vector.keep", keytype, OrgDb, dataset, ...)
}
\arguments{
\item{keys}{vector of protein identifiers (see keytypes_org())}

\item{columns}{data columns to return(see columns_org())}

\item{output}{format of output ("vector.keep" (default), "vector.na,
"vector.rm", "mapping.na", "mapping.rm", "TERM2GENE")}

\item{keytype}{type of supplied keys (identified automatically if not
provided)}

\item{OrgDb}{string of Annotation package name to use}

\item{dataset}{dataset}

\item{...}{arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")}
}
\description{
Queries data from Annotation packages
}
