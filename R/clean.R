#'Cleaning the Data
#'
#'@param data A "-" delimited file with the data concerning earthquakes
#'that can be downloaded from the the NOAA website
#'
#'@return This function returns a dataframe with all the data in a tidy
#'format
#'
#'@source The data should come from the NOOA website
#'
#'@importFrom dplyr, tidyr, lubridate
#'
#'@note We must first read the data
#'
#'@note We change the format of the date, and change lognitude and latitudes
#'into numerics
#'
#'@examples \code{  data <- read.delim("results")
#' eq <- eq_clean_data(data)}
#'
eq_clean_data <- function(data) {
  eq <- data %>%
    dplyr::mutate(DATE = paste(YEAR, MONTH, DAY, sep = "-")) %>%
    dplyr::select(-YEAR, -MONTH, -DAY)

  eq$DATE <- lubridate::ymd(eq$DATE)

  eq$LATITUDE <- base::as.numeric(eq$LATITUDE)
  eq$LONGITUDE <- base::as.numeric(eq$LONGITUDE)
  return(eq)
}
#'Cleaning the location
#'
#'This is a function that makes some further changes to the variable
#'LOCATION that will be useful later. In particular, we keep only the name
#'of the place and we dismiss the country. For example, the location
#'"Italy: Palermo" becomes "Palermo".
#'
#'@param data is a dataframe with all the data that we aquired from
#'the NOOA website and we cleaned with the help of the function
#'eq_clean_data().
#'
#'@return This function returns a dataframe with all the data in a tidy
#'format. All the locations have the appropriate format.
#'
#'@source The data should come from the NOOA website
#'
#'@importFrom dplyr, tidyr, stringr
#'
#'@example \code{eq <- eq_location_clean(eq)}
#'
library(tidyverse)
library(lubridate)

eq_location_clean <- function(data){

  data$LOCATION_NAME <- data$LOCATION_NAME %>%
    base::as.character() %>%
    stringr::str_extract(":.+") %>%
    stringr::str_sub(start=3)  %>%
    stringr::str_to_title()
  return(data)
}
