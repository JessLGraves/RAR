#' Person Time
#'
#' Internal function used to adjust for average waking hour before conducting localized RAR analysis
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time.
#' @param hour_bin specifies length of time-bin.
#' @param wake_hr specifies the average wake hour (can be number or column in dataset if multiple subjects).
#'
#' @export

person_time <- function(df, hour_bin, wake_hr){
  df <- df
  wake_hr_eval <- eval(substitute(wake_hr), df, parent.frame())

  if(as.character(substitute(wake_hr)) %in% colnames(df)==TRUE){
    df$wake_hr <- wake_hr_eval
    df$tadj <- df$thrs - df$wake_hr
    df$tadj[(df$tadj < 0)] <- df$tadj[(df$tadj < 0)] + 24
    df$bins <- as.factor(cut(df$tadj, seq(0, 24, hour_bin), include.lowest=TRUE))
  }

  if(class(wake_hr_eval)=="numeric"){
    df$wake_hr <- wake_hr_eval
    df$tadj <- df$thrs - df$wake_hr
    df$tadj[(df$tadj < 0)] <- df$tadj[(df$tadj < 0)] + 24
    df$bins <- as.factor(cut(df$tadj, seq(0, 24, hour_bin), include.lowest=TRUE))
  }

  return(df)
}
