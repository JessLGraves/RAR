#' RAR Localized Measures: For Files with Mulitple Participants
#'
#' This function calculates localized measures of RAR timing, as described in Graves (2018). Measures include: Mean Activity, Standard Deviation of Activity, and Relative Activity at user-specified time-bins across days. This function takes a single dataframe including activity, time, ID, and wake-up hour for each participant.
#'
#' Outputs from this function include: dataframes of calculated localized measures based on user-specified hour bins, day x time summaries of activity, plots of Mean, SD, and Relative Activity Measures.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data, time, an ID column, and a column for average wake up hours. Time must be in HH:MM:SS format and stored as a character, factor, or POSIX object.
#' @param act_column specifies the name of the column within df that contains the activity count data. RAR_ExCosine will do a log(activity + 1) transformation.
#' @param time_column specifies the name of the column that contains time of observation
#' @param id_column specifies the name of the column that contains participant IDs
#' @param hour_bin specifices the number of hours within each time bin (e.g. 4 hour timebins = 4)
#' @param person_time logical specifying if person-specific time adjustements should be made. If missing, default is FALSE.
#' @param tLeft is a column name
#' @param transform logical specifying if log(activity + 1) transformation is desired
#' @param plot logical specifying if plots should be outputted. If missing, defaults to FALSE.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom magrittr %>%
#' @examples
#' data(rar_data_multi_wake)
#' Local_Multi = RAR_Localized_Multi(rar_data_multi_wake, act, time, id, 4, TRUE, wakehr, TRUE, TRUE)
#' Local_Multi$localized_measures
#' Local_Multi$plots
#' @export

RAR_Localized_Multi <- function(df, act_column, time_column, id_column, hour_bin, person_time = c(TRUE, FALSE),
                                tLeft=NULL, transform = c(TRUE, FALSE), plot=c(TRUE, FALSE)){

  id <- NULL
  activity <- NULL
  . <- NULL
  time <- NULL
  bins <- NULL
  mean.act <- NULL
  sd.act <- NULL
  rel.act <- NULL

  df = as.data.frame(df)
  df = df[stats::complete.cases(df),] # Removing any NAs in data

  if(person_time==F){

  act_col <- eval(substitute(act_column), df, parent.frame())
  time_col <- eval(substitute(time_column), df, parent.frame())
  id_col <- eval(substitute(id_column), df, parent.frame())

  hour_bin_select = hour_bin
  plot_select = plot
  tLeft_select = tLeft
  person_time_select = person_time
  transform_select = transform

  df$id = as.factor(id_col)
  df$time = time_col
  df$activity = act_col

  df = df[, c("time", "activity", "id")]
  }

  if(person_time==T){

    act_col <- eval(substitute(act_column), df, parent.frame())
    time_col <- eval(substitute(time_column), df, parent.frame())
    id_col <- eval(substitute(id_column), df, parent.frame())
    tleft_col <- eval(substitute(tLeft), df, parent.frame())

    hour_bin_select = hour_bin
    plot_select = plot
    tLeft_select = substitute(tLeft)
    person_time_select = person_time
    transform_select = transform

    df$id = as.factor(id_col)
    df$time = time_col
    df$activity = act_col
    df$wakeup = tleft_col

    df = df[, c("time", "activity", "id", "wakeup")]
    colnames(df) <- c("time", "activity", "id", as.character(substitute(tLeft_select)))
  }

  Localized_Out <- df %>% dplyr::group_by(id) %>%
    dplyr::do(RAR_Localized_Out = RAR_Localized(df = ., act_column = activity, time_column = time, hour_bin = hour_bin_select,
                                              person_time = person_time_select, tLeft = !!tLeft_select,
                                              transform = transform_select, plot = plot_select))

  ids = unique(df$id)
  localized_measures <- vector(mode="list", length=length(ids))
  for (i in 1:length(localized_measures)){
      localized_measures[[i]] <- as.data.frame(Localized_Out$RAR_Localized_Out[[i]]$localized_measures)
      localized_measures[[i]]$id <- as.character(Localized_Out$id[[i]])
  }

  localized_measures <- dplyr::bind_rows(localized_measures)

  if (plot_select==F){
  objects <- list("localized_measures" = localized_measures, "Localized_Out" = Localized_Out)
  return(objects)
  }

  if (plot_select==T & person_time_select==T){
      x.lab = "Time Bins (hours after activity started)"
      plot_mean <- ggplot2::ggplot(localized_measures, aes(x=bins, y=mean.act)) + ggplot2::geom_point() + ggplot2::geom_line(aes(group=1)) + ggplot2::facet_wrap(~id) +
          ggplot2::labs(x= x.lab, y="Mean Log(Activity+1)", title="Mean Activity at each Time Bin (Adjusted Time) by Participant")
      plot_sd <- ggplot2::ggplot(localized_measures, aes(x=bins, y=sd.act)) + ggplot2::geom_point() + ggplot2::geom_line(aes(group=1)) + ggplot2::facet_wrap(~id) +
          ggplot2::labs(x= x.lab, y="SD Log(Activity+1)", title="SD Activity at each Time Bin (Adjusted Time) by Participant")
      plot_rel <- ggplot2::ggplot(localized_measures, aes(x=bins, y=rel.act)) + ggplot2::geom_point() + ggplot2::geom_line(aes(group=1)) + ggplot2::facet_wrap(~id) +
          ggplot2::labs(x= x.lab, y="Relative Log(Activity+1)", title="Relative Activity at each Time Bin (Adjusted Time) by Participant")

      plots <- list(plot_mean, plot_sd, plot_rel)
      objects <- list("localized_measures" = localized_measures, "Localized_Out" = Localized_Out, "plots" = plots)

      return(objects)
  }

  if (plot_select==T & person_time_select==F){
      x.lab = "Time Bins (Clock Time)"
      plot_mean <- ggplot2::ggplot(localized_measures, aes(x=bins, y=mean.act)) + ggplot2::geom_point() + ggplot2::geom_line(aes(group=1)) + ggplot2::facet_wrap(~id) +
          ggplot2::labs(x=x.lab, y="Mean Log(Activity+1)", title="Mean Activity at each Time Bin (Adjusted Time) by Participant")
      plot_sd <- ggplot2::ggplot(localized_measures, aes(x=bins, y=sd.act)) + ggplot2::geom_point() + ggplot2::geom_line(aes(group=1)) + ggplot2::facet_wrap(~id) +
          ggplot2::labs(x=x.lab, y="SD Log(Activity+1)", title="SD Activity at each Time Bin (Adjusted Time) by Participant")
      plot_rel <- ggplot2::ggplot(localized_measures, aes(x=bins, y=rel.act)) + ggplot2::geom_point() + ggplot2::geom_line(aes(group=1)) + ggplot2::facet_wrap(~id) +
          ggplot2::labs(x=x.lab, y="Relative Log(Activity+1)", title="Relative Activity at each Time Bin (Adjusted Time) by Participant")

      plots <- list(plot_mean, plot_sd, plot_rel)
      objects <- list("localized_measures" = localized_measures, "Localized_Out" = Localized_Out, "plots" = plots)

      return(objects)
  }

}
