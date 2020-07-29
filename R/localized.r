#' Localized
#'
#' Internal function used to analyze mean, standard deviation and relative activity within specified time-bins
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time.
#' @param log_transform accepts logical specifying if log-transformation should be applied to activity data
#'
#' @importFrom magrittr "%>%"
#' @export

localized <- function(df, log_transform=c(TRUE, FALSE)){
  df<-df

  day <- bins <- log.act <- act <- mean.activity <- NULL

  if(log_transform==T){
    day.by.time <- df %>% dplyr::group_by(day, bins) %>%
      dplyr::summarise(N = sum(!is.na(log.act), na.rm=T),
                       mean.activity = mean(log.act, na.rm=T),
                       sd = stats::sd(log.act, na.rm=T)) %>% as.data.frame()
  }

  if(log_transform==F){
    day.by.time <- df %>%
      dplyr::group_by(day, bins) %>%
      dplyr::summarise(N = sum(!is.na(act), na.rm=T),
                       mean.activity = mean(act, na.rm=T),
                       sd = stats::sd(act, na.rm=T)) %>% as.data.frame()
  }

  # Calculating Mean & SD activity by time across days
  measures <- day.by.time %>%
    dplyr::group_by(bins) %>%
    dplyr::summarise(N = sum(!is.na(mean.activity), na.rm=T),
                     mean.act = mean(mean.activity, na.rm=T),
                     sd.act = stats::sd(mean.activity, na.rm=T),
                     quant.25 = stats::quantile(mean.activity, 0.25), # calculating 25th percentile of mean act
                     quant.75 = stats::quantile(mean.activity, 0.75)) %>% as.data.frame() # calculating 75th percentile of mean act

  # Relative Activity
  measures$sum.act <- sum(measures$mean.act)
  measures$rel.act <- measures$mean.act/measures$sum.act

  #objects <- list("measures" = measures, "df" = df)

  return(measures)
}
