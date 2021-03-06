% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stratified_event_tree.R
\docType{methods}
\name{plot,Stratified.event.tree,ANY-method}
\alias{plot,Stratified.event.tree,ANY-method}
\title{Stratified.event.tree Plotting}
\usage{
\S4method{plot}{Stratified.event.tree,ANY}(x)
}
\arguments{
\item{x}{Stratified.event.tree S4 object}
}
\value{
the plot and also a pdf version is saved in the working directory.
}
\description{
Method to plot a Stratified.event.tree S4 object. The current \code{ceg} package
implementation depends on \code{Rgraphviz} package from Bioconductor for
plotting.
}
\examples{
plot(set)

}
