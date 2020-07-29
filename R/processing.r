#' Data processing
#'
#' Internal function used to process activity data files to work in RAR functions.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time.
#'
#' @export

processing <- function(df){
  df <- df
  df$log.act <- log(df$act + 1)

  time_zone <- lubridate::tz(df$date_time)

  df$date <- as.Date(df$date_time, tz=time_zone)
  df$time <- format(df$date_time,"%H:%M:%S")

  dates <- unique(df$date)
  df$day <- match(df$date, dates)

  df$tm <- lubridate::hour(df$date_time)*60 + lubridate::minute(df$date_time) + lubridate::second(df$date_time)/60
  df$thrs <- lubridate::hour(df$date_time) + lubridate::minute(df$date_time)/60  + lubridate::second(df$date_time)/3600
  df$td <- (df$day - 1) * 1440 + df$tm
  df$n <- seq(1, nrow(df))

  return(df)
}
