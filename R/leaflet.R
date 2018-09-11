#'Leaflet map
#'
#'eq_map creates a leaflet map for the earthquakes in our data set.
#'
#'@param data is a cleaned data set of the earthquakes
#'
#'@param annot_col is the character name of the collumn that the user
#'wants to appear in the pop-up window.
#'
#'@importFrom leaflet, dplyr
#'@return a leaflet map
#'
#'@examples eq %>%
#'            filter((COUNTRY %in% c("GREECE")) & year(DATE)>1981) %>%
#'            eq_map(annot_col = "DEATHS")
#'
#'
library(leaflet)

eq_map <- function(data, annot_col){
  x<-data[,c(annot_col)]
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = data,
                     lng = ~LONGITUDE, lat = ~LATITUDE,
                     radius = ~EQ_PRIMARY,
                     popup = ~ paste(x)  )
}

#' New label for the leaflet map
#'
#' eq_create_label creates the content of a label that includes the
#' locationm the magnitude and the number of deaths of the earthquake
#'
#'@note if one of the information is missing the it will not appear in
#' the label
#'
#'@import dplyr
#'
#'@param data is a cleaned data set of the earthquakes
#'
#'@return a data frame with a column for the labels' content
#'
#'@examples \code{eq %>% mutate(popup_text = eq_create_label(.)) %>%
#'              eq_map(annot_col = "popup_text")}

eq_create_label <- function(data){
       location <- ifelse(!is.na(data$LOCATION_NAME),
                          paste(sep = " ",
                                "<b> Location</b>:",
                                 data$LOCATION_NAME,
                                 "<br>"),
                          "<br>")

       magnitude <- ifelse(!is.na(data$EQ_PRIMARY),
                           paste(sep = " ","<b> Magnitude</b>:",
                                data$EQ_PRIMARY,
                                "<br>"),
                           "<br>")

       deaths <- ifelse(!is.na(data$DEATHS),
                        paste(sep = " ","<b> Magnitude</b>:",
                              data$DEATHS,
                              "<br>"),
                          "<br>")

return(paste(sep = " ", location, magnitude,deaths))
}


