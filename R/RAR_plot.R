#' RAR_plot
#'
#' Visualizes observed and fitted activity data based on the default output from extended cosine model (RAR()). Default plotting features interpolated data where missing occurs.
#'
#' @author Jessica Graves
#' @param rar_output default output from RAR().
#' @param predicted specifies to plot predicted values based only on observed data (i.e. does not interpolate if missing values present). Default is FALSE.
#' @param id_vals character vector to print plots for each participant specified. Default is NULL.
#'
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#'
#' @examples
#' \dontrun{
#' data(age_wise)
#' d <- age_wise[age_wise$id==29,]
#' rar_ex <- RAR(d, act, date_time)
#' RAR_plot(rar_ex)
#'
#' # Multiple subjects
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' p <- RAR_plot(rar_ex4, id_vals=c("1", "2")) # plot for participants 1 and 2 with interpolated data
#' p$plot_log.act # on log scale
#' p$plot_act # on natural scale
#'
#' p2 <- RAR_plot(rar_ex4, TRUE, c("1", "2"))
#' }

RAR_plot <- function(rar_output, predicted=c(TRUE, FALSE), id_vals=NULL){

  act <- day <- e_interp.pred <- interp.pred <- log.act <- td <- NULL

  if(missing(predicted)){predicted <- FALSE}

  df <- rar_output$df_interp
  pred_vals <- df$interp.pred
  e_pred_vals <- df$e_interp.pred
  show <- TRUE
  plot_title <- "Observed and Predicted Activity (with missingness)"

  if(length(unique(df$id[!is.na(df$id)]))>1 & missing(id_vals)){
    stop("RAR output contains multiple subjects, please specify which ids to plot with id_vals.")
  }

  if(predicted){
    df <- rar_output$df_predicted
    pred_vals <- df$predicted
    e_pred_vals <- df$e_predicted
    show <- FALSE
    plot_title <- "Observed and Predicted Activity"
  }

  if (missing(id_vals)){
    day_labels <- unique(df$day)
    day_labels <- as.character(day_labels[stats::complete.cases(day_labels)])

    ##### Plot on Log Scale
    plot_log.act <- ggplot2::ggplot(df, aes(x=td, y=log.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(data=df, aes(x=td, y=pred_vals, group=1, color=as.factor(is.na(day))), size=1, show.legend = show) +
      ggplot2::labs(title=paste0(plot_title), x="Days of Observation", y="Log(activity + 1)", col="Missing Data") +
      ggplot2::scale_x_continuous(breaks=seq(0,max(df$td), by=1440), labels = day_labels) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    ##### Plot on Natural Scale
    plot_act <- ggplot2::ggplot(df, aes(x=td, y=act)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(data=df, aes(x=td, y=e_pred_vals, group=1, color=as.factor(is.na(day))), size=1, show.legend = show) +
      ggplot2::labs(title=paste0(plot_title), x="Days of Observation", y="Activity (raw)", col="Missing Data")  +
      ggplot2::scale_x_continuous(breaks=seq(0,max(df$td), by=1440), labels = day_labels) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    objects <- list("plot_log.act" = plot_log.act, "plot_act" = plot_act)
    return(objects)

  }

  if(!missing(id_vals)){
    df <- df[which(df$id %in% id_vals), ]
    day_labels <- unique(df$day)
    day_labels <- as.character(day_labels[stats::complete.cases(day_labels)])

    pred_vals <- df$interp.pred
    e_pred_vals <- df$e_interp.pred

    if(predicted){
      pred_vals <- df$predicted
      e_pred_vals <- df$e_predicted
    }
    ##### Plot on Log Scale
    plot_log.act <- ggplot2::ggplot(df, aes(x=td, y=log.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(data=df, aes(x=td, y=pred_vals, group=1, color=as.factor(is.na(day))), size=1, show.legend = show) +
      ggplot2::labs(title=paste0(plot_title), x="Days of Observation", y="Log(activity + 1)", col="Missing Data") +
      ggplot2::scale_x_continuous(breaks=seq(0,max(df$td), by=1440), labels = day_labels) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) + ggplot2::facet_wrap(~ id, scales = "free")

    ##### Plot on Natural Scale
    plot_act <- ggplot2::ggplot(df, aes(x=td, y=act)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
      ggplot2::theme_classic() + ggplot2::geom_line(data=df, aes(x=td, y=e_pred_vals, group=1, color=as.factor(is.na(day))), size=1, show.legend = show) +
      ggplot2::labs(title=paste0(plot_title), x="Days of Observation", y="Activity (raw)", col="Missing Data")  +
      ggplot2::scale_x_continuous(breaks=seq(0,max(df$td), by=1440), labels = day_labels) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) + ggplot2::facet_wrap(~ id, scales = "free")

    objects <- list("plot_log.act" = plot_log.act, "plot_act" = plot_act)
    return(objects)
  }

}
