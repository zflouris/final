#'Timeline of Earthquackes
#'
#'geom_timeline creates a diagram that involve a line with points that
#'indicate the earthquakes in chronological order.
#'
#'
#'@importFrom grid, ggplot, dplyr
#'
#'@examples \code{ggplot(data=eq %>% filter(COUNTRY == "GREECE" &
#'                 year(DATE)>1970)) +
#'geom_timeline(aes(x=DATE, y =COUNTRY ,size = INTENSITY,color = DEATHS))+
#'theme(panel.grid = element_blank(),
#'      axis.ticks.y = element_blank(),
#'      axis.title.y = element_blank(),
#'      panel.background = element_blank())}
#'
library(grid)

geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = Geomtimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...
      )
    )
  }



Geomtimeline <- ggproto("Geomtimeline",Geom,

      required_aes = c("x"),

      default_aes = aes(shape = 19,
                        colour = "black",
                        size = 1.5,
                        fill = NA,
                        alpha = 0.5,
                        stroke = 0.5,
                        y = c(0.3)),

      non_missing_aes = c("size", "shape", "colour","y"),

      draw_key = draw_key_point,

      draw_group = function(data, panel_scales, coord) {
                       coords <-
                              coord$transform(data, panel_scales)


                       points <-
                              grid::pointsGrob(
                                         x = coords$x,
                                         y = coords$y,
                                         pch = coords$shape,
                                         gp = grid::gpar(
                                                  col = alpha(coords$colour,
                                                              coords$alpha),
                                                  fill = alpha(coords$fill,
                                                              coords$alpha),
                                                  fontsize =
                            coords$size * .pt + coords$stroke * .stroke / 2,
                                                  lwd =
                                                coords$stroke * .stroke / 2)
                                                  )

                          line <-
                                grid::linesGrob(
                                          x = coords$x,
                                          y = coords$y,
                                          gp = gpar(col= "black",
                                                    alpha= 0.6,
                                                    lwd = 0.5)
                                                  )

                          grid::gList(points, line)
                        }
                        )

#' Labels for the Timeline of Earthquackes
#'
#'geom_timeline_label adds labels to the timeline of earthquakes. The user
#'can choose the number n_max of earthquakes  of greatest intensity for
#'which the labels wants to appear.
#'
#'
#'@importFrom grid, ggplot, dplyr
#'
#'#'@examples ggplot(data=eq %>% filter(COUNTRY == "GREECE" &
#'                 year(DATE)>1970)) +
#'geom_timeline(aes(x=DATE, y =COUNTRY ,size = INTENSITY,color = DEATHS))+
#'geom_timeline_label(aes(label = LOCATION_NAME, x=DATE,size = INTENSITY,
#'                         y =COUNTRY),n_max=4)+
#'theme(panel.grid = element_blank(),
#'      axis.ticks.y = element_blank(),
#'      axis.title.y = element_blank(),
#'      panel.background = element_blank())
#'
#'
Geomtimeline_label <-
  ggproto("Geomtimeline_label",
          Geom,
          required_aes = c("label"),
          default_aes = aes(shape = 19,
                            colour = "black",
                            size = 1.5,
                            fill = NA,
                            alpha = 0.5,
                            stroke = 0.5,
                            y = c(0.2),
                            x = c(10)),
          draw_key = draw_key_point,

          draw_group = function(data, panel_scales, coord, n_max)
          {
            coords <- coord$transform(data, panel_scales)
            k <- n_max
            sortedcoords <- coords %>%
              group_by(y) %>%
              top_n(n = k, wt = size )


            lines <- segmentsGrob(x0=sortedcoords$x,
                                  y0=sortedcoords$y,
                                  x1=sortedcoords$x,
                                  y1=sortedcoords$y + 0.1,
                                  #default.units = "native",
                                  gp = gpar(col= "black",
                                            alpha= 0.6,
                                            lwd = 0.5)
            )

            labels <-  textGrob(label = coords$label,
                                x = sortedcoords$x,
                                y = sortedcoords$y + 0.1,
                                rot = 45,
                                just = "left",
                                #default.units = "native",
                                gp = gpar(
                                  fontsize=8
                                ))

            gList(lines,labels)
          })
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = Geomtimeline_label, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)) }

