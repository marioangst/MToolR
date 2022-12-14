% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new.R
\name{igraph_from_mtools_el}
\alias{igraph_from_mtools_el}
\title{Create an igraph graph object from a M-Tool edgelist}
\usage{
igraph_from_mtools_el(
  edgelist,
  concepts,
  from_col = "From",
  to_col = "To",
  weight_col = "Weight"
)
}
\arguments{
\item{edgelist}{A raw edgelist necessarily containing a sender, receiver and weight column}

\item{concepts}{character vector of unique concepts gathered in mental model}

\item{from_col}{The name of the column to read sender nodes from. Defaults to MTool output "From"}

\item{to_col}{The name of the column to read receiver nodes from. Defaults to MTool output "To"}

\item{weight_col}{The name of the}
}
\value{
A weighted, directed igraph object
}
\description{
Create a weighted, direct igraph graph object from M-Tool data.
This most often will not be seen by the user as it is used internally at
import.
}
