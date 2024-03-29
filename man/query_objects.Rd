% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_objects.R
\name{query_objects}
\alias{query_objects}
\title{Return key information on all objects on the Arctic Data Center}
\usage{
query_objects(n = 10000000, cache_tolerance = 14)
}
\arguments{
\item{n}{(integer) Number of rows to return}

\item{cache_tolerance}{(integer) Maximum number of days tolerable for age of
cached results. If the cached results are older than the cache tolerance, a
new dataset will be cached and the old cache deleted. To force a cache
refresh, set tolerance to zero.}
}
\value{
(data.frame) Result of the SOLR query
}
\description{
This function returns the identifier, origin, formatType, size,
dateUploaded, and obsoletedBy fields for every object on the
Arctic Data Center as a data.frame. This can then be used to extract
various metrics and plots.
}
