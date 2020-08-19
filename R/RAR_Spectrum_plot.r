#' Plotting Power Spectrum of Residuals
#'
#' This function plots the estimated and smoothed power spectrum generated from RAR_Spectrum().
#'
#' @author Haoyi Fu, Jessica Graves
#' @param rar_spectrum default output from RAR_Spectrum().
#' @param id_vals character vector to print plots for each participant specified. Default is NULL.
#'
#' @export
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#'
#' @examples
#' \dontrun{
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' rar_ex <- RAR(d, act, date_time)
#' spec <- RAR_Spectrum(rar_ex)
#' RAR_Spectrum_plot(spec)
#'
#' # Multiple subjects
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' spec4 <- RAR_Spectrum(rar_ex4, id_column=id)
#' RAR_Spectrum_plot(spec4, id_vals=c("1", "2")) # log scale, ids 1 and 2
#' }


RAR_Spectrum_plot <- function(rar_spectrum, id_vals=NULL){
  spectrum_value <- rar_spectrum$spectrum_value
  predict.ss <- rar_spectrum$predict.ss

  log_transform <- rar_spectrum$log_transform
  method <- rar_spectrum$method

  freq.cycles.hour <- spec <- NULL

  if(length(unique(spectrum_value$id[!is.na(spectrum_value$id)])) > 1 & missing(id_vals)){
    stop("RAR_Spectrum output contains multiple subjects, please specify which ids to plot with id_vals.")
  }

  # if(missing(log_transform)){log_transform <- TRUE}
  # if(missing(method)){method <- "smooth"}

  if(log_transform==T){
    pt_text <- "Log-transformed "
    y_text <- "Log spectrum"
    spectrum_value$spec <- spectrum_value$log_spec
  }

  if(log_transform==F){
    pt_text <- "Natural scale "
    y_text <- "Spectrum"
    spectrum_value$spec <- spectrum_value$sp
  }

  pt_method <- ""

  # if(missing(method)){method <- 0}
  if(method=="smooth"){pt_method <- "& cubic smoothing spline"}
  if(method=="penalized"){pt_method <- "& penalized smoothing spline"}

  if(missing(id_vals)){
  plot <- ggplot2::ggplot(spectrum_value, aes(x=freq.cycles.hour, y=spec)) + ggplot2::geom_line(color="grey") +
    ggplot2::theme_classic() + ggplot2::geom_line(aes(x=predict.ss$x, y=predict.ss$y), color="blue", size=1) + ggplot2::theme_classic() +
    ggplot2::labs(title=paste0(pt_text, pt_method), x="Cycles/Hour", y=y_text) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5))

  return(plot)
  }

  if(!missing(id_vals)){

    spectrum_value <- spectrum_value[which(spectrum_value$id %in% id_vals), ]
    predict.ss <- predict.ss[which(predict.ss$id %in% id_vals), ]
    # spec <- spec[which(spectrum_value$id %in% id_vals)]

    plot <- ggplot2::ggplot(spectrum_value, aes(x=freq.cycles.hour, y=spec)) + ggplot2::geom_line(color="grey") +
      ggplot2::theme_classic() + ggplot2::geom_line(aes(x=predict.ss$x, y=predict.ss$y), color="blue", size=1) + ggplot2::theme_classic() +
      ggplot2::labs(title=paste0(pt_text, pt_method), x="Cycles/Hour", y=y_text) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5)) + ggplot2::facet_wrap(~ id, scales = "free")

    return(plot)
  }
}
