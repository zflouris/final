% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Cleaning the location}
\source{
The data should come from the NOOA website
}
\usage{
eq_location_clean(data)
}
\arguments{
\item{data}{is a dataframe with all the data that we aquired from
the NOOA website and we cleaned with the help of the function
eq_clean_data().}
}
\value{
This function returns a dataframe with all the data in a tidy
format. All the locations have the appropriate format.
}
\description{
This is a function that makes some further changes to the variable
LOCATION that will be useful later. In particular, we keep only the name
of the place and we dismiss the country. For example, the location
"Italy: Palermo" becomes "Palermo".
}
