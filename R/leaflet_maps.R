#' @title Generate leaflet map with annotations
#'
#' @description Subset of earthquake data plotted on a map using leaflet.
#' Input dataframe requires LONGITUDE, LATITUDE and EQ_PRIMARY columns.
#' Other required argument: annotation column ('annot_col')
#'
#' @param df dataframe for plotting
#' @param annot_col column from input dataframe for annotation
#'
#' @return Returns an interactive leaflet map
#'
#' @import leaflet, dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  map1 <-load_data() %>%
#'    eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "GREECE" & lubridate::year(date) >= 2002) %>%
#'    eq_map(annot_col = "date")
#'  print(map1)
#' }
eq_map <- function(df, annot_col){
  lmap <- df %>% leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng=df$LONGITUDE,
                              lat=df$LATITUDE,
                              radius=df$EQ_PRIMARY,
                              popup=df[[annot_col]],
                              color="blue",
                              weight=1,
                              opacity=0.5)
}

#------

#' @title Generate earthquake label column
#'
#' @description Input a dataframe (df) and the function will output a vector with HTML column names
#' Required columns: CLEAN_LOCATION_NAME, EQ_PRIMARY and DEATHS
#'
#' @param df dataframe for creating annotation column
#'
#' @return Returns a vector of earthquake details
#'
#' @import dplyr, lubridate
#'
#' @export
#'
#' @examples
#' \dontrun{
#' map2 <-load_data() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "GREECE" & lubridate::year(date) >= 2002) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' print(map2)
#' }
eq_create_label <- function(df){
  len <- length(df$CLEAN_LOCATION_NAME)
  locations <- df$CLEAN_LOCATION_NAME
  magnitude <- df$EQ_PRIMARY
  deaths <- df$DEATHS

  ptxt <- rep("", len)
  for(i in 1:len){
    txt <- paste0("<b>Location: </b>", locations[i], "</br>",
                  "<b>Magnitude: </b>", magnitude[i], "</br>",
                  "<b>Total Deaths: </b>", deaths[i])
    ptxt[i] <- txt
  }
  return(ptxt)
}
