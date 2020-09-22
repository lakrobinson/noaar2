#' @title geom_timeline function
#'
#' @description
#' This geom charts a timeline of earthquakes for a given country
#' with points along the timeline for each earthquake. The size of the point inicates
#' the magnitude of the earthquake, while the color indicates the number of deaths.
#' The date (x) is a required aesthetic. The country (y) is optional.
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- df_earthquakes %>% filter(COUNTRY %in% c("GREECE", "USA"), YEAR > 2002)
#'  ggplot(df, aes(x = date, y = COUNTRY,
#'                 color = as.numeric(TOTAL_DEATHS),
#'                 size = as.numeric(EQ_PRIMARY),
#'                 label = CLEAN_LOCATION_NAME)) +
#'    geom_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths") +
#'    ggplot2::theme(panel.background = ggplot2::element_blank(),
#'          legend.position = "bottom",
#'          axis.title.y = ggplot2::element_blank()) +
#'    ggplot2::xlab("DATE")
#' }
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity", na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#------

#' @title geom_timeline_label function
#'
#' @description
#' Run geom_timeline first to create timeline.
#' This geom will add a vertical line and label to those points.
#'
#' @param n_max Integer (default = 5); represents the max number of earthquakes to show.
#'
#' @import ggplot2, dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = date, y = COUNTRY,
#'                color = as.numeric(TOTAL_DEATHS),
#'                size = as.numeric(EQ_PRIMARY),
#'                label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'         legend.position = "bottom",
#'         axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
#'   geom_timeline_label(data=df)
#' }

geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity", na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, n_max = 5, ...) {

  # here we alter the number of earthquakes we apply a label to as n_max
  data <- data %>% dplyr::mutate(COUNTRY = as.character(COUNTRY), EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
    dplyr::arrange(COUNTRY, desc(EQ_PRIMARY))

  countries <- unique(data$COUNTRY)
  df_all <- data.frame()
  for(country in countries){
    df <- data %>% dplyr::filter(COUNTRY == country) %>% head(n_max)
    df_all <- rbind(df_all, df)
  }
  data <- df_all
  #print(data)

  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#------

#' @title Define GeomTimeline class
#'
#' @description
#' The GeomTimeline charts a timeline of earthquakes for a given country
#' with points along the timeline for each earthquake. The size of the point inicates
#' the magnitude of the earthquake, while the color indicates the number of deaths.
#' The date (x) is a required aesthetic. The country (y) is optional.
#'
#' @import ggplot2, grid, scales
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- df_earthquakes %>% filter(COUNTRY %in% c("GREECE", "USA"), YEAR > 2002)
#'  ggplot(df, aes(x = date, y = COUNTRY,
#'                 color = as.numeric(TOTAL_DEATHS),
#'                 size = as.numeric(EQ_PRIMARY),
#'                 label = CLEAN_LOCATION_NAME)) +
#'    geom_timeline() +
#'    labs(size = "Richter scale value", color = "# deaths") +
#'    ggplot2::theme(panel.background = ggplot2::element_blank(),
#'          legend.position = "bottom",
#'          axis.title.y = ggplot2::element_blank()) +
#'    ggplot2::xlab("DATE")
#' }
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"), # optional y aesthetic
                                 default_aes = ggplot2::aes(y=1, alpha=0.7, fill="grey", colour="grey", size=1, shape=21, stroke=1),
                                 draw_key = ggplot2::draw_key_point,

                                 # create points across a timeline for each level / country
                                 draw_group = function(data, panel_scales, coord) {
                                   #print(head(data))
                                   coords <- coord$transform(data, panel_scales)

                                   # pch ->  vector indicating what plotting symbol to use
                                   # pch = 21 is a filled circle
                                   points <- grid::pointsGrob(coords$x, coords$y,
                                                              pch = coords$shape,
                                                              size = grid::unit(coords$size / 6, "lines"),
                                                              gp = gpar(col = alpha(coords$colour, coords$alpha),
                                                                        fill = alpha(coords$colour, coords$alpha)
                                                              )
                                   )

                                   line <- grid::segmentsGrob(
                                     x0 = 0, y0 = coords$y,
                                     x1 = 1, y1 = coords$y,
                                     gp = gpar(col = "grey", alpha=0.7, size=1)
                                   )
                                   grid::gList(points, line)
                                 }
)

#------

#' @title Define GeomTimelineLabel class
#'
#' @description
#' The GeomTimelineLabel will add a vertical line and label to those points.
#'
#' @import ggplot2, grid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = date, y = COUNTRY,
#'                color = as.numeric(TOTAL_DEATHS),
#'                size = as.numeric(EQ_PRIMARY),
#'                label = CLEAN_LOCATION_NAME)) +
#'   geom_timeline() +
#'   labs(size = "Richter scale value", color = "# deaths") +
#'   ggplot2::theme(panel.background = ggplot2::element_blank(),
#'         legend.position = "bottom",
#'         axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
#'   geom_timeline_label(data=df)
#' }
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),
                                      default_aes = ggplot2::aes(y=1, alpha=0.7, fill="grey", colour="grey"),
                                      draw_key = ggplot2::draw_key_label,

                                      # timeline and points acquired using geom_timeline
                                      # add a vertical line with label
                                      draw_group = function(data, panel_scales, coord) {
                                        #print(head(data))
                                        coords <- coord$transform(data, panel_scales)

                                        y_extension <- 0.05
                                        line <- grid::segmentsGrob(# vertical line
                                          x0 = coords$x, y0 = coords$y,
                                          x1 = coords$x, y1 = coords$y + y_extension,
                                          gp = grid::gpar(col = "grey", alpha=0.7, size=1)
                                        )

                                        text <- grid::textGrob(
                                          label=coords$label,
                                          x = coords$x,
                                          y = coords$y + y_extension,
                                          rot = 45,
                                          just = c("left", "bottom")
                                        )

                                        grid::gList(line, text)
                                      }
)

#------

#' @title Plot earthquake timeline without labels
#'
#' @description
#' This plots and formats the earthquake data.
#' The plot will be saved as earthquakes_timeline.png if save_png = TRUE.
#'
#' @param df Earthquake data
#' @param save_png Indicates whether to save plot
#'
#' @return NULL
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- df_earthquakes %>%
#'   filter(COUNTRY %in% c("GREECE", "USA"), YEAR > 2002)
#' plot_earthquakes_timeline(df, save_png=TRUE)
#' }
plot_earthquakes_timeline <- function(df, save_png=FALSE){
  ggplot(df, aes(x = date, y = COUNTRY,
                 color = as.numeric(TOTAL_DEATHS),
                 size = as.numeric(EQ_PRIMARY))) +
    geom_timeline() +
    labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) +
    ggplot2::xlab("DATE")

  if(save_png){ggsave("earthquakes_timeline.png")}
}

#------

#' @title Plot  earthquake timeline with labels
#'
#' @description
#' This plots and formats the earthquake data, with the addition of labels for the points along the timeline.
#' The plot will be saved as earthquakes_timeline_label.png if save_png = TRUE.
#'
#' @param df Earthquake data
#' @param save_png Indicates whether to save plot
#'
#' @return NULL
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- df_earthquakes %>%
#'   filter(COUNTRY %in% c("GREECE", "USA"), YEAR > 2002)
#' plot_earthquakes_timeline_label(df, save_png=TRUE)
#' }
plot_earthquakes_timeline_label <- function(df, save_png=FALSE){
  ggplot(df, aes(x = date, y = COUNTRY,
                 color = as.numeric(TOTAL_DEATHS),
                 size = as.numeric(EQ_PRIMARY),
                 label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE") +
    geom_timeline_label(data=df)

  if(save_png){ggsave("earthquakes_timeline_label.png")}
}

