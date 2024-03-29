% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_all_versions}
\alias{get_all_versions}
\title{Get the PIDs of all versions of an object}
\usage{
get_all_versions(node, pid)
}
\arguments{
\item{node}{(MNode) The Member Node to query.}

\item{pid}{(character) Any object in the chain.}
}
\value{
(character) A vector of PIDs in the chain, in order.
}
\description{
Get the PIDs of all versions of an object.
}
\examples{
\dontrun{
cn <- CNode("STAGING2")
mn <- getMNode(cn,"urn:node:mnTestKNB")
pid <- "urn:uuid:3e5307c4-0bf3-4fd3-939c-112d4d11e8a1"

ids <- get_all_versions(mn, pid)
}
}
