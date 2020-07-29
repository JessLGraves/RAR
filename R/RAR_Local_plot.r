#' RAR_Local_plot
#'
#' Visualizes observed and fitted activity data based on mean, standard deviation, and relative activity based on user-specified time bins
#'
#' @author Jessica Graves
#' @param rar_local_object default output from RAR_Local().
#' @param id_vals character vector to print plots for each participant specified. Default is NULL. Use "ALL" to see activity measures for all participants simultaneously.
#'
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#'
#' @examples
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' local <- RAR_Local(d, act, date_time, 4, TRUE, 6.17) # get localized measures
#' RAR_Local_plot(local)
#'
#' # Multiple subjects
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' d4.2 <- merge(d4, rar_ex4$parameters, by="id")
#' local4 <- RAR_Local(d4.2, act, date_time, 4, wake_hr=tLeft, id_column=id) # wake-hour adjusted
#' RAR_Local_plot(local4, id_vals=c("1", "2"))
#'
#' local4.clock <- RAR_Local(d4.2, act, date_time, 4, id_column=id) # clock time, log scale
#' RAR_Local_plot(local4.clock, id_vals=c("1", "2"))


RAR_Local_plot <- function(rar_local_object, id_vals=NULL){

  bins <- mean.act <- quant.25 <- quant.75 <- rel.act <- sd.act <- NULL

  measures <- rar_local_object$localized

  time_labels <- c(as.character(measures$bins))
  x.lab <- "Time Bins (Clock Time)"
  plot.title <- ""

  if(rar_local_object$pt_val==1){
    x.lab <- "Time Bins (hours after activity started)"
    plot.title <- "(Adjusted Time)"
  }

  if(missing(id_vals)){
    plot_mean <- ggplot2::ggplot(measures, aes(x=bins, y=mean.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(size=1) +
      ggplot2::geom_line(aes(x=bins, y=quant.25), color="pink") + ggplot2::geom_line(aes(x=bins, y=quant.75), color="pink") +
      ggplot2::labs(title=paste0("Mean Activity at each Time Bin ", plot.title),
                    x=x.lab, y="Mean Activity", caption="Pink = 25th and 75th percentiles") +
      ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

    plot_sd <- ggplot2::ggplot(measures, aes(x=bins, y=sd.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(size=1) + ggplot2::labs(title=paste0("SD Activity at each Time Bin ", plot.title),
                                                                            x=x.lab, y="SD Activity") +
      ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

    plot_rel <- ggplot2::ggplot(measures, aes(x=bins, y=rel.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(size=1) +
      ggplot2::labs(title=paste0("Relative Activity at each Time Bin ", plot.title),
                    x=x.lab, y="Relative Activity") +
      ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5))

    objects <- list("plot_mean" = plot_mean, "plot_sd" = plot_sd, "plot_rel" = plot_rel)
    return(objects)
  }

  if(!missing(id_vals)){


    if(all(id_vals=="ALL")){

      measures <- measures

      plot_mean <- ggplot2::ggplot(measures, aes(x=bins, y=mean.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
        ggplot2::theme_classic() + ggplot2::labs(title=paste0("Mean Activity at each Time Bin ", plot.title),
                                                 x=x.lab, y="Mean Activity") +
        ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
        ggplot2::stat_summary(fun.y="mean", geom="line", color="red")

      plot_sd <- ggplot2::ggplot(measures, aes(x=bins, y=sd.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
        ggplot2::theme_classic() + ggplot2::labs(title=paste0("SD Activity at each Time Bin ", plot.title),
                                                 x=x.lab, y="SD Activity") +
        ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
        ggplot2::stat_summary(fun.y="mean", geom="line", color="red")

      plot_rel <- ggplot2::ggplot(measures, aes(x=bins, y=rel.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
        ggplot2::theme_classic() + ggplot2::labs(title=paste0("Relative Activity at each Time Bin ", plot.title),
                                                 x=x.lab, y="Relative Activity") +
        ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
        ggplot2::stat_summary(fun.y="mean", geom="line", color="red")

      objects <- list("plot_mean" = plot_mean, "plot_sd" = plot_sd, "plot_rel" = plot_rel)
      return(objects)
    }

    measures <- measures[which(measures$id %in% id_vals),]

    plot_mean <- ggplot2::ggplot(measures, aes(x=bins, y=mean.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(size=1) +
      ggplot2::geom_line(aes(x=bins, y=quant.25), color="pink") + ggplot2::geom_line(aes(x=bins, y=quant.75), color="pink") +
      ggplot2::labs(title=paste0("Mean Activity at each Time Bin ", plot.title),
                    x=x.lab, y="Mean Activity", caption="Pink = 25th and 75th percentiles") +
      ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) + ggplot2::facet_wrap(~ id, scales="free")

    plot_sd <- ggplot2::ggplot(measures, aes(x=bins, y=sd.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(size=1) + ggplot2::labs(title=paste0("SD Activity at each Time Bin ", plot.title),
                                                                            x=x.lab, y="SD Activity") +
      ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) + ggplot2::facet_wrap(~ id, scales="free")

    plot_rel <- ggplot2::ggplot(measures, aes(x=bins, y=rel.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(size=1) +
      ggplot2::labs(title=paste0("Relative Activity at each Time Bin ", plot.title),
                    x=x.lab, y="Relative Activity") +
      ggplot2::scale_x_discrete(labels = time_labels) + ggplot2::theme(plot.title = element_text(hjust = 0.5)) + ggplot2::facet_wrap(~ id, scales="free")

    objects <- list("plot_mean" = plot_mean, "plot_sd" = plot_sd, "plot_rel" = plot_rel)
    return(objects)
  }
}
