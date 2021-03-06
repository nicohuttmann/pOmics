% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transpose_tibble.R
\name{transpose_tibble}
\alias{transpose_tibble}
\title{Transposes tibble and uses first column as column names}
\usage{
transpose_tibble(
  tibble,
  from.row.names = "observations",
  to.row.names = "variables"
)
}
\arguments{
\item{tibble}{tibble}

\item{from.row.names}{row names column of initial data frame}

\item{to.row.names}{row names column after transposing}
}
\value{

}
\description{
Transposes tibble and uses first column as column names
}
