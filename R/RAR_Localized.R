#' RAR Localized Measures
#'
#' This function calculates localized measures of RAR timing, as described in Graves (2018). Measures include: Mean Activity, Standard Deviation of Activity, and Relative Activity at user-specified time-bins across days.
#'
#' Outputs from this function include: dataframes of calculated localized measures based on user-specified hour bins, day x time summaries of activity, plots of Mean, SD, and Relative Activity Measures.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time. Time must be in HH:MM:SS format and stored as a character, factor, or POSIX object.
#' @param act_column specifies the name of the column within df that contains the activity count data. RAR_ExCosine will do a log(activity + 1) transformation.
#' @param time_column specifies the name of the column that contains time of observation
#' @param hour_bin specifices the number of hours within each time bin (e.g. 4 hour timebins = 4)
#' @param person_time logical specifying if person-specific time adjustements should be made. If missing, default is FALSE.
#' @param tLeft numeric value indicating subject's up-mesor (or wake up hour). Can be estimated using RAR_ExCosine(). Or, if merged into dataframe, is column name.
#' @param transform logical specifying if log(activity + 1) transformation is desired
#' @param plot logical specifying if plots should be outputted. If missing, defaults to FALSE.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom magrittr %>%
#' @examples
#' data(rar_data)
#' # defaults to clock-time and log(act + 1)
#' rar_local = RAR_Localized(rar_data, act, time, 4, plot=TRUE)
#' rar_local$localized_measures
#' rar_local$plots
#' rar_local$plots[[1]]
#' @export


RAR_Localized <- function(df, act_column, time_column, hour_bin, person_time = c(TRUE, FALSE),
                          tLeft=NULL, transform = c(TRUE, FALSE), plot=c(TRUE, FALSE)){

    df = as.data.frame(df)
    df = df[stats::complete.cases(df),] # Removing any NAs in data

    if(as.character(substitute(act_column)) %in% colnames(df) == F){
        stop("act_column must be in your datafame")
    }

    if(as.character(substitute(time_column)) %in% colnames(df) == F){
        stop("time_column must be in your datafame")
    }

    if(24%%hour_bin!=0){
        stop("hour_bin must be an integer and a multiple of 24. e.g., 1, 2, 3, 4, 6, 8, 12")
    }

    if(class(transform) != "logical"){
        stop("transform must be either TRUE or FALSE")
    }

    if(missing(transform)){
        transform=T
    }

    if(missing(plot)){
        plot=F
    }

    if(missing(person_time) & missing(tLeft)){
        person_time=F
    }

    if(person_time==TRUE & missing(tLeft)){
        stop("Must supply tLeft")
    }

    if(missing(person_time) & !missing(tLeft)){
        stop("Must specify person_time = TRUE")
    }

    if(person_time==T &!missing(tLeft)){
        tLeft <- eval(substitute(tLeft), df, parent.frame())
        tLeft <- as.matrix(tLeft)

        if(dim(tLeft)[[1]]==1 & dim(tLeft)[[2]]==1) {
            tLeft = as.numeric(tLeft)
        }
    }
    act_col_name <- dplyr::enquo(act_column)
    act_column <- eval(substitute(act_column), df, parent.frame()) # Storing activity column values
        df$log.act = log(act_column + 1)

    time_column <- eval(substitute(time_column), df, parent.frame())

    bins <- NULL
    day <- NULL
    log.act <- NULL
    mean.act <- NULL
    mean.activity <- NULL
    quant.25 <- NULL
    quant.75 <- NULL
    rel.act <- NULL
    sd.act <- NULL

    # Convering time to POSIXct & Creating Day Index
    day_index = 1
    df$day = 1

    # If time not already in POSIXct form
    if(!any(class(time_column) == c("POSIXt"))){
        df$time.posixct = as.POSIXct(as.character(time_column), format="%H:%M:%S") #, format="%H:%M:%S"
        df$thrs <- lubridate::hour(df$time.posixct) + lubridate::minute( df$time.posixct)/60  + lubridate::second(df$time.posixct)/3600
        df$tm = lubridate::hour(df$time.posixct)*60 + lubridate::minute(df$time.posixct) + lubridate::second(df$time.posixct)/60

        for (i in 2:length(time_column) ){
            day_delta = as.numeric(df$tm[i] < df$tm[i-1])
            day_index = day_index + day_delta
            df$day[i] = day_index
           # df$td[i] = (day_index - 1) * 1440 + df$tm[i] # number of minutes after first midnight
        }
    }

    # If time already POSIXct form just create Day Index
    if(any(class(time_column) == c("POSIXt"))){
        df$thrs <- lubridate::hour(time_column) + lubridate::minute(time_column)/60  + lubridate::second(time_column)/3600
        df$tm = lubridate::hour(time_column)*60 + lubridate::minute(time_column) + lubridate::second(time_column)/60

        for (i in 2:length(time_column)){
            day_delta = as.numeric(df$tm[i] < df$tm[i-1])
            day_index = day_index + day_delta
            df$day[i] = day_index
            # df$td[i] = (day_index - 1) * 1440 + df$tm[i] # number of minutes after first midnight
        }
    }

    if(person_time==F){
        df$bins = as.factor(cut(df$thrs, seq(0, 24, hour_bin), include.lowest=TRUE)) # bins should be an integer.
    }

    if(person_time==T){
        df$day.adj <- NULL
        df$tadj = df$thrs - tLeft
        df$day.adj = as.numeric(df$day)
        df$day.adj[(df$tadj <0)] = df$day.adj[(df$tadj <0)] - 1
        df$tadj[(df$tadj < 0)] = df$tadj[(df$tadj < 0)] + 24
        df$bins = as.factor(cut(df$tadj, seq(0, 24, hour_bin), include.lowest=TRUE))
    }

    df$day <- as.factor(df$day)

    # Calculating Mean & SD activity across time and days
        if(transform==T){
            day.by.time <- as.data.frame(df %>%
                                             dplyr::group_by(day, bins) %>%
                                             dplyr::summarise(N = sum(!is.na(log.act), na.rm=T),
                                                              mean.activity = mean(log.act, na.rm=T),
                                                              sd = stats::sd(log.act, na.rm=T)))
            }

        if(transform==F){
            day.by.time <- as.data.frame(df %>%
                                             dplyr::group_by(day, bins) %>%
                                             dplyr::summarise(N = sum(!is.na(!! act_col_name), na.rm=T),
                                                              mean.activity = mean(!! act_col_name, na.rm=T),
                                                              sd = stats::sd(!! act_col_name, na.rm=T)))
            }


    ##### Final Localized Measures ####
    # Calculating Mean & SD activity by time across days
        measures <- as.data.frame(day.by.time %>%
                                  dplyr::group_by(bins) %>%
                                  dplyr::summarise(N = sum(!is.na(mean.activity), na.rm=T),
                                                   mean.act = mean(mean.activity, na.rm=T),
                                                   sd.act = stats::sd(mean.activity, na.rm=T),
                                                   quant.25 = stats::quantile(mean.activity, 0.25), # calculating 25th percentile of mean act
                                                   quant.75 = stats::quantile(mean.activity, 0.75))) # calculating 75th percentile of mean act

            # Relative Activity
            measures$sum.act <- sum(measures$mean.act)
            measures$rel.act = measures$mean.act/measures$sum.act

    #### Plots ####
if(plot==T){
    time_labels = c(as.character(measures$bins))

    if(person_time==T){
        x.lab = "Time Bins (hours after activity started)"
        plot_mean <- ggplot2::ggplot(measures, aes(x=bins, y=mean.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
            ggplot2::theme_bw() + ggplot2::geom_line(size=1) +
            ggplot2::geom_line(aes(x=bins, y=quant.25), color="pink") + ggplot2::geom_line(aes(x=bins, y=quant.75), color="pink") +
            ggplot2::labs(title="Mean Activity at each Time Bin (Adjusted Time)",
                          x=x.lab, y="Mean Log(activity + 1)", caption="Pink = 25th and 75th percentiles") +
            ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

        plot_sd <- ggplot2::ggplot(measures, aes(x=bins, y=sd.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
            ggplot2::theme_bw() + ggplot2::geom_line(size=1) + ggplot2::labs(title="Standard Deviation Activity at each Time Bin (Adjusted Time)",
                                                                             x=x.lab, y="SD Log(activity + 1)") +
            ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

        plot_rel <- ggplot2::ggplot(measures, aes(x=bins, y=rel.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
            ggplot2::theme_bw() + ggplot2::geom_line(size=1) +
            ggplot2::labs(title="Relative Activity at each Time Bin (Adjusted Time)",
                          x=x.lab, y="Relative Log(activity + 1)") +
            ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

        plots <- list(plot_mean, plot_sd, plot_rel)
        }

    if(person_time==F){
        x.lab = "Time Bins (Clock Time)"
        plot_mean <- ggplot2::ggplot(measures, aes(x=bins, y=mean.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
            ggplot2::theme_bw() + ggplot2::geom_line(size=1) +
            ggplot2::geom_line(aes(x=bins, y=quant.25), color="pink") + ggplot2::geom_line(aes(x=bins, y=quant.75), color="pink") +
            ggplot2::labs(title="Mean Activity at each Time Bin",
                          x=x.lab, y="Mean Log(activity + 1)", caption="Pink = 25th and 75th percentiles") +
            ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

        plot_sd <- ggplot2::ggplot(measures, aes(x=bins, y=sd.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
            ggplot2::theme_bw() + ggplot2::geom_line(size=1) + ggplot2::labs(title="Standard Deviation Activity at each Time Bin",
                                                                             x=x.lab, y="SD Log(activity + 1)") +
            ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

        plot_rel <- ggplot2::ggplot(measures, aes(x=bins, y=rel.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
            ggplot2::theme_bw() + ggplot2::geom_line(size=1) +
            ggplot2::labs(title="Relative Activity at each Time Bin",
                          x=x.lab, y="Relative Log(activity + 1)") +
            ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

        plots <- list(plot_mean, plot_sd, plot_rel)
            }

        #### Objects
        objects <- list("localized_measures" = measures, "day.by.time.summary" = day.by.time, "df" = df,
                        "plots" = plots)
        return(objects)
    }


    #### Objects
    objects <- list("localized_measures" = measures, "day.by.time.summary" = day.by.time, "df" = df)
    return(objects)
}
