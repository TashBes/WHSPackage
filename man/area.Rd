% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/area_functions.R
\name{area}
\alias{area}
\title{Polygon area calculation}
\usage{
area(data, col, name)
}
\arguments{
\item{data}{The polygon or shape file that you want to calculate the area for.}

\item{col}{The column in the shape file that represents the polygons}

\item{name}{The name of the polygons}
}
\description{
Calculates the area of all polygons, and the percentage of area within the world heritage polygon that they occupy both individually and as a whole.
}
\details{
The packages tidyverse, janitor, sf, and gt need to be installed
}
