% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{eq_clean_data}
\alias{eq_clean_data}
\title{Cleaning the Data}
\source{
The data should come from the NOOA website
}
\usage{
eq_clean_data(data)
}
\arguments{
\item{data}{A "-" delimited file with the data concerning earthquakes
that can be downloaded from the the NOAA website}
}
\value{
This function returns a dataframe with all the data in a tidy
format
}
\description{
Cleaning the Data
}
\note{
We must first read the data

We change the format of the date, and change lognitude and latitudes
into numerics
}
\examples{
\\code{  data <- read.delim("results")
eq <- eq_clean_data(data)}

}
