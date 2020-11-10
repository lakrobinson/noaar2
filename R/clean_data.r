#' @title Load raw data
#'
#' @description Function loads raw, tab delimited data (from NOAA, in data folder).
#'
#' @param path file path to data source (default == "data/signif.txt.tsv")
#'
#' @return Returns a dataframe
#'
#' @importFrom readr read_delim
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data()
#'   print(class(df))
#'   head(df)
#' }
#'
load_data <- function(path=file.path("inst/extdata", "signif.txt.tsv")){
  df <- readr::read_delim(path, delim="\t")
}

#------

#' @title Generate a single date column from NOAA data's DAY, MONTH and YEAR columns
#'
#' @description Takes the three date-related columns and turns them into one cohesive date column. Some instances of
#' DAY and MONTH are NA. NA days will be replaced with 1, NA months will be replaced with Jan.
#' Negative years have been removed.
#'
#' @param days day of year as (int)
#' @param months month (int)
#' @param years year (int)
#'
#' @return Returns a single date vector DATES
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data() %>% dplyr::filter(YEAR >= 0) %>%
#'        dplyr::mutate(date = get_date(DAY, MONTH, YEAR))
#'   print(class(df))
#'   head(df)
#' }
#'
get_date <- function(days, months, years){
  n <- length(days)
  dates <- seq(as.Date(Sys.Date()), by=0, len=n)
  for(i in 1:n){
    day <- days[i]
    month <- months[i]
    year <- years[i]
    if(is.na(day)){day <- 1}
    if(is.na(month)){month <- 1}
    date_str = paste(year, month, day, sep="-")
    dates[i] <- as.Date(date_str)
  }
  return(dates)
}

#------

#' @title Clean LOCATION_NAME column
#'
#' @description This function cleans the LOCATION_NAME column by removing country name and
#' converting names to title case.
#'
#' @param df dataframe containing LOCATION_NAME column
#'
#' @return Returns df with cleaned LOCATION_NAME column
#'
#' @importFrom dplyr filter, mutate
#' @importFrom stringr str_trim, str_to_title
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data() %>% eq_location_clean()
#'   print(class(df))
#'   head(df)
#' }
#'
eq_location_clean <- function(df){
  df <- df %>% dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_trim(gsub(".*:","", LOCATION_NAME))) %>%
    dplyr::mutate(CLEAN_LOCATION_NAME = stringr::str_to_title(CLEAN_LOCATION_NAME))
  return(df)
}

#------

#' @title Clean raw earthquake data
#'
#' @description Cleans raw data by creating a date column with year, month and day in one date, converting
#' LATITUDE and LONGITUDE columns to numeric and cleaning the LOCATION_NAME column with the eq_location_clean() function.
#' Negative years have been removed.
#'
#' @param df_raw dataframe of raw NOAA data
#'
#' @return Returns a cleaned version of df_raw
#'
#' @importFrom dplyr filter, mutate
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- load_data() %>% dplyr::filter(YEAR >= 0) %>%
#'        dplyr::mutate(date = get_date(DAY, MONTH, YEAR))
#'   print(class(df))
#'   head(df)
#' }

eq_clean_data <- function(df_raw){
  df <- df_raw
  df <- df %>% dplyr::filter(YEAR >= 0) %>%
    dplyr::mutate(date = get_date(DAY, MONTH, YEAR)) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
    eq_location_clean()
  return(df)
}
