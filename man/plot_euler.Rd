% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_euler.R
\name{plot_euler}
\alias{plot_euler}
\title{Plots Euler diagram from dataframe}
\usage{
plot_euler(
  data_,
  fontsize = 8,
  linewidth = 0.5,
  transpose = T,
  from.row.names = "observations",
  to.row.names = "variables",
  view = T,
  input,
  output = "plot_euler"
)
}
\arguments{
\item{data_}{data_ list}

\item{fontsize}{size of labels}

\item{linewidth}{thiccness of ellipses}

\item{transpose}{transpose data frame}

\item{from.row.names}{row names column of initial data frame}

\item{to.row.names}{row names column after transposing (must be variables)}

\item{view}{view plot}

\item{input}{name of input data}

\item{output}{name of output data}
}
\description{
Plots Euler diagram from dataframe
}
