% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Runs.r
\name{Runs}
\alias{Runs}
\title{Calculate and write maximum runs}
\usage{
Runs(
  AllPols,
  CRSproject = terra::crs(AllPols),
  nameID,
  nameFeho,
  WindTablexHour = NULL,
  flagRuns = "both",
  CreateSharedFrontLines = "T"
)
}
\arguments{
\item{AllPols}{spatVector object with polygons of fire progression}

\item{CRSproject}{crs of the spatVector}

\item{nameID}{name of polygon unique ID field}

\item{nameFeho}{name of hour ID field}

\item{WindTablexHour}{table with wind direction per hour. default is NULL if no wind direction is provided}

\item{flagRuns}{parameter to indicate whether both pol and wind runs are calculated (values='both', 'pols' or 'wind'). Default is 'both'}

\item{CreateSharedFrontLines}{parameter to indicate whether front lines file needs to be created. Default is T}
}
\value{
it writes one or two shapefiles with runs
}
\description{
Function for calculate and write maximum runs according to a fire isochrone map (poluygons) and a wind direction per hour table
}
