#' RAR_Bands_plot
#'
#' Estimates the effect of variability of residuals estimated from extended cosine model based on specific frequency bands.
#'
#' @author Jessica Graves
#' @param rar_object default output from RAR().
#' @param freq.bands a matrix of frequency bands, e.g. (t(c(0, 2/24))), ranging between 0 and 60.
#' @param sampling.rate the number of observations per second. Default is 1/60 for 60-second activity epochs.
#' @param id_vals character vector to print plots for each participant specified. Default is NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' rar_ex <- RAR(d, act, date_time, "antilogit")
#' bands <- RAR_Bands_plot(rar_ex, t(c(0, 2/24)))
#' bands$plots # effect of filter
#'
#' # Multiple bands
#' f1 <- c(0, 2/24); f2 <- c(2/24, 25); f3 <- c(25, 60)
#' fs = as.data.frame(rbind(f1, f2, f3))
#' bands_m <- RAR_Bands_plot(rar_ex, fs)
#' bands_m$plots[[1]] # effect of filter 1
#'
#' # Multiple subjects, multiple bands
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' bands_m.4 <- RAR_Bands_plot(rar_ex4, fs, id_vals=c("1", "2"))
#' bands_m.4$plots[[1]] # effect of filter from 0, 2/24 (filter [[1]])
#' bands_m.4$plots[[2]] # effect of filter 2
#' bands_m.4$plots[[3]] # effect of filter 3
#' }

RAR_Bands_plot <- function(rar_object, freq.bands, sampling.rate=(1/60), id_vals=NULL){

  df <- rar_object$df_predicted
  df <- df[stats::complete.cases(df),]     # Remove any NAs

  freq.bands <- as.data.frame(freq.bands)
  colnames(freq.bands)<-c("l.b", "u.b")
  no.bands <- dim(freq.bands)[1]

  date_time <- id <- . <- td <- predicted <- NULL

  if((min(freq.bands) < 0 ) | (max(freq.bands) > 60)){
    stop("frequency band intervals must be from [0,60], units are cycles/hour")
  }

  sampling.rate <- as.numeric(sampling.rate) # in cycles/seconds

  ### Fast Fourier Transform on Residuals & Effect of Filters on Predicted Values###
  if(missing(id_vals)){
    filt_effect <- fft_fit(df, freq.bands, no.bands, sampling.rate)
    }

  if(!missing(id_vals)){
    df <-df[which(df$id %in% id_vals),]

    filt_effect <- df %>% dplyr::group_by(id) %>% dplyr::do(fft_fit(., freq.bands, no.bands, sampling.rate)) %>% as.data.frame()
    filt_effect <- dplyr::bind_rows(filt_effect)
    filt_effect$id <- as.character(filt_effect$id)
    }

  filt_effect_cols <- names(filt_effect)[grepl("f.effect", names(filt_effect))] # should be the same for every subject

  # Outputs List of Plots showing effect of Filter
  if(missing(id_vals)){
    day_labels <- unique(df$day)
    day_labels <- as.character(day_labels[stats::complete.cases(day_labels)])

    plots <- list()
    for(i in 1:no.bands){
      p1 <- ggplot2::ggplot(filt_effect, aes(x=td, y=predicted)) + ggplot2::geom_line(color="black") +
        ggplot2::geom_line(ggplot2::aes_string(x="td", y=paste0("f.effect", i)), color="indianred3") + ggplot2::labs(x="Day", y="Activity") +
        ggplot2::ggtitle(paste0("Effect of Filter [", round(freq.bands[i,1], 3), ", ", round(freq.bands[i,2], 3), "]")) +
        ggplot2::scale_x_continuous(breaks=seq(0,max(df$td), by=1440), labels = day_labels) +
        ggplot2::theme_classic()

      print(p1)
      plots[[i]] <- p1
      }
  }

  if(!missing(id_vals)){
    day_labels <- unique(df$day)
    day_labels <- as.character(day_labels[stats::complete.cases(day_labels)])

    plots <- list()
    for(i in 1:no.bands){
      p1 <- ggplot2::ggplot(filt_effect, aes(x=td, y=predicted)) + ggplot2::geom_line(color="black") +
        ggplot2::geom_line(ggplot2::aes_string(x="td", y=paste0("f.effect", i)), color="indianred3") + ggplot2::labs(x="Day", y="Activity") +
        ggplot2::ggtitle(paste0("Effect of Filter [", round(freq.bands[i,1], 3), ", ", round(freq.bands[i,2], 3), "]")) +
        ggplot2::scale_x_continuous(breaks=seq(0,max(df$td), by=1440), labels = day_labels) +
        ggplot2::theme_classic() + ggplot2::facet_wrap(~id, scales="free")

      print(p1)
      plots[[i]] <- p1
    }
  }

    objects <- list("df" = df, "filt_effect" = filt_effect, "plots" = plots)

    return(objects)

}
