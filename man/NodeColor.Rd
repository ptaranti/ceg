% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stratified_staged_tree.R
\name{NodeColor}
\alias{NodeColor}
\title{NodeColor}
\usage{
NodeColor(num.variable, num.situation, num.category, stage.structure,
  range.color)
}
\arguments{
\item{num.variable}{(numeric) - number of variables.}

\item{num.situation}{(vector) - number of stages associated with each variable.}

\item{num.category}{(vector) - it identifies the number of edges that emanate from situations in each level.}

\item{stage.structure}{list with two components:
\itemize{
\item  numeric - score associated with a level
\item  list of vectors - stage structure
}}

\item{range.color}{(numeric) - it chooses the palette. If 1, it is used a 8-color palette.
If 2, it is used a 501-color palette.}
}
\value{
vector - node colors
}
\description{
This function yields the node colors.
}
