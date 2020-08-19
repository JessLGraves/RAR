#' RAR_Local
#'
#' Calculates mean, standard deviation, and relative activity within user-specified time-bin widths (e.g. 4 hour time bins) across days. If user-specified, time will be adjusted for average rise-time.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time.
#' @param act_column name of the column within df that contains the activity count data.
#' @param time_column name of the column that contains date and time of observation. Time must be a POSIX object.
#' @param hour_bin user-specified numeric of hour length of bin (e.g. 4).
#' @param log_transform specifies if log transformation of activity data should be performed.
#' @param wake_hr average rise (i.e. wake) time for participant in hours. Must be numeric for single participant. For multiple participants, wake_hr must be merged in to activity dataset (df), wake_hr in this case represents the column name.
#' @param id_column name of column containing id if multiple subjects exist in dataframe.
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' rar_ex <- RAR(d, act, date_time)
#' # wake-hour adjusted, log scale
#' local <- RAR_Local(d, act, date_time, 4, TRUE, rar_ex$parameters$tLeft)
#' local$localized # mean, sd, and relative activity estiamtes across days within bins
#'
#' # Multiple subjects
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' d4.2 <- merge(d4, rar_ex4$parameters, by="id")
#' local4 <- RAR_Local(d4.2, act, date_time, 4, wake_hr=tLeft, id_column=id) # wake-hour adjusted
#' local4$localized
#'
#' local4.clock <- RAR_Local(d4.2, act, date_time, 4, id_column=id) # clock time, log scale
#' local4.clock$localized
#' }

RAR_Local <- function(df, act_column, time_column, hour_bin, log_transform=c(TRUE, FALSE), wake_hr=NULL, id_column=NULL){

  id <- . <- NULL

  df <- df[stats::complete.cases(df), ]
  df$date_time <- eval(substitute(time_column), df, parent.frame())
  df$act <- eval(substitute(act_column), df, parent.frame())

  if(missing(log_transform)){log_transform <- TRUE}

  if(!missing(id_column)){
      pt <- 0
    if(missing(wake_hr)){wake_hr_col <- 0}
    if(!missing(wake_hr)){
      wake_hr_col <- substitute(wake_hr)
      pt <- 1}

    df$id <- as.factor(eval(substitute(id_column), df, parent.frame()))

    ##### Data Processing #####
    df <- df %>% dplyr::group_by(id) %>% dplyr::do(processing(.)) %>% as.data.frame()
    df <- df %>% dplyr::group_by(id) %>% dplyr::do(person_time(., hour_bin, wake_hr=!!wake_hr_col)) %>% dplyr::ungroup() %>% as.data.frame()

    measures <- df %>% dplyr::group_by(id) %>% dplyr::do(localized(., log_transform)) %>% as.data.frame()
    measures$id <- as.character(measures$id)

    objects <- list("df" = df, "localized" = measures, "pt_val"=pt)
    return(objects)
  }

  if(missing(id_column)){
    pt<-0
    if(!missing(wake_hr)){pt<-1}
    if(missing(wake_hr)){wake_hr <- 0}
    ##### Data Processing #####
    df <- processing(df)
    df <- person_time(df, hour_bin, wake_hr)

    ##### Localized Measures #####
    measures <- localized(df, log_transform)
    #measures$id <- as.character(measures$id)

    objects <- list("df" = df, "localized" = measures, "pt_val" = pt)
    return(objects)
  }
}
