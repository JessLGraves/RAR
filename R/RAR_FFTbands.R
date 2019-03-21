#' RAR Fast Fourier Transform of Selected Frequency Bands
#'
#' This function performs Fast Fourier Transform (FFT) on residuals based on extended cosine model (RAR_ExCosine()) and applies user-specified frequency-band filters. The methodology can be found in detail in R.T Krafty et al, Measuring Variability in Rest-Activity Rhythms from Actigraphy with Application to Characterizing Symptoms of Depression (2019)
#'
#' @author Jessica Graves
#' @param df dataframe including residuals calculating from extended cosine model based on actigraphy data. Residuals must be in numeric class. Time must be in HH:MM:SS format and stored as a character, factor, or POSIX object.
#' @param resid_column specfies the name of the column in the df that contains residuals. This must be in numeric class
#' @param time_column specifies the name of the column that contains time of observation for residuals
#' @param predict_column specifies the name of the column that contains the predicted values from the extended cosine model
#' @param plot a logical value specifying if plots should be outputted. Plots illustrate effects of filters.
#' @param freq.bands a kx2 matrix or dataframe specifcing frequency band intervals of interested for calculating area under spectrum, where the 1st column is the lower bound, and the 2nd column is the upper bound for the frequency band. Frequencies supplied must be in cycles/hour.
#' @param sampling.rate a numeric value representing the sampling rate of data in Hz (cycles/second). e.g. data collected once every 30 seconds specified as sampling.rate=1/30.
#' @return a list containing original dataframe along with filter results, plots of filter effects (if plot=T), and lists of FFT per filter.
#' @details This function will apply user-specified frequency bands to residuals extracted from the extened cosine model and will illustrate the effect of those filters.
#' @seealso \code{fft}
#' @references 1. R.T Krafty et al, Measuring Variability in Rest-Activity Rhythms from Actigraphy with Application to Characterizing Symptoms of Depression (2019)
#' @importFrom xts xts
#' @importFrom ggplot2 aes
#' @importFrom stats fft
#'
#' @examples
#' data(df_predicted)
#' f1 <- c(0, 2/24) # Filter Values
#' f2 <- c(2/24, 25)
#' f3 <- c(25, 60)
#' fs = as.data.frame(rbind(f1, f2, f3))
#' filter = RAR_FFTbands(df_predicted, resid, time, predicted, TRUE, fs, 1/60)
#' filter$plots[[1]]
#' @export

RAR_FFTbands <- function(df, resid_column, time_column, predict_column, plot=c(TRUE,FALSE), freq.bands, sampling.rate){

  df=as.data.frame(df)
  df=df[stats::complete.cases(df),]     # Remove any NAs

  freq.bands <- as.data.frame(freq.bands)
  colnames(freq.bands)<-c("l.b", "u.b")
  no.bands = dim(freq.bands)[1]
  sampling.rate <- as.numeric(sampling.rate) # must be supplied in cycles/seconds

  V1 <- NULL
  daytime <-NULL

  if(as.character(substitute(resid_column)) %in% colnames(df)==F){
    stop("resid_column must be in your dataframe")
  }

  if(as.character(substitute(time_column)) %in% colnames(df)==F){
    stop("time_column must be in your dataframe")
  }

  if(as.character(substitute(predict_column)) %in% colnames(df)==F){
    stop("predic_column must be in your dataframe")
  }

  if((min(freq.bands) < 0 ) | (max(freq.bands) > 60)){
    stop("frequency band intervals must be from [0,60], units are cycles/hour")
  }

  if(missing(sampling.rate)){
    stop("sampling.rate must be supplied and must be in units of cycles/second")
  }

  ### Convert resid_column and time_column to correct formats ###
  # Store residual and time column values for future use
  resid_column<-eval(substitute(resid_column), df,parent.frame())
  time_column<-eval(substitute(time_column), df, parent.frame())
  predict_column<-eval(substitute(predict_column), df, parent.frame())

  # Create date variable for time-series
  df$day=1
  day_index=1

  # If time_column is not in "POSIXt", convert it to "POSIXt" with year month day
  if(!("POSIXt" %in% class(time_column))){

    df$time.posixct = as.POSIXct(as.character(time_column), format="%H:%M:%S")
    df$date = Sys.Date()
    df$tm = lubridate::hour(df$time.posixct)*60 + lubridate::minute(df$time.posixct) + lubridate::second(df$time.posixct)/60

    for (i in 2:length(time_column) ){
      day_delta = as.numeric(df$tm[i] < df$tm[i-1])
      day_index = day_index + day_delta
      df$day[i] = day_index
    }

    df$datetime = df$day + df$date
    df$daytime = as.POSIXct(paste(df$datetime, time_column),format="%Y-%m-%d %H:%M:%S", tz="EST")
  }

  # If time_column is in "POSIXt" class, nothing needs to do
  if("POSIXt" %in% class(time_column)){
    #df<-df
    df$daytime = as.POSIXct(time_column, format="%Y-%m-%d %H:%M:%S", tz="EST")
  }

  ### Fast Fourier Transform on Residuals ###
  # Create a time-series object for the FFT analysis

  resid.ts<-xts::xts(resid_column, df$daytime)
  freq.resid = sampling.rate
  freq.convert = 3600*freq.resid # get numeric value to transform FFT frequency to cycles/hour

  # Perform FFT
  fft.fit = stats::fft(resid.ts)

  # Covnert frequency of FFT of Residuals to cycles/hour
  freq.resid.hr = freq.convert*(0:(length(fft.fit)-1))/(length(fft.fit))

  ### Apply Filters ###
  # Different rules depending on if low-pass, band, or high-pass filter
  filters <- vector(mode="list", length=no.bands)
  f.results <- vector(mode="list", length=no.bands)
  for (i in 1:no.bands){
    filters[[i]]$fft <- fft.fit

    # Low-pass filters ([0,X])
    if (freq.bands[i,1] == 0){
      filters[[i]]$elim <- (freq.resid.hr >= (freq.bands[i,2])) & (freq.resid.hr <= (freq.convert-freq.bands[i,2]))
      filters[[i]]$fft[filters[[i]]$elim] = 0
      f.results[[i]] <- as.data.frame(Re(fft(filters[[i]]$fft, inverse=T)/length(filters[[i]]$fft)))
    }

    # Band Filter ([X, Y])
    if ((freq.bands[i,1] !=0) & (freq.bands[i,2] !=60)){
      filters[[i]]$elim.out <- (freq.resid.hr < freq.bands[i,1]) | (freq.resid.hr > (freq.convert-freq.bands[i,1]))
      filters[[i]]$elim.in <- (freq.resid.hr > (freq.bands[i,2])) & (freq.resid.hr < (freq.convert-freq.bands[i,2]))
      filters[[i]]$fft[filters[[i]]$elim.out] = 0
      filters[[i]]$fft[filters[[i]]$elim.in] = 0
      f.results[[i]] <- as.data.frame(Re(fft(filters[[i]]$fft, inverse=T)/length(filters[[i]]$fft)))
    }

    # High Pass Filter ([Y, 60])
    if ((freq.bands[i,2] == 60)){
      filters[[i]]$elim <- (freq.resid.hr <= freq.bands[i,1]) | (freq.resid.hr >= (freq.convert - freq.bands[i,1]))
      filters[[i]]$fft[filters[[i]]$elim] = 0
      f.results[[i]] <- as.data.frame(Re(fft(filters[[i]]$fft, inverse=T)/length(filters[[i]]$fft)))
    }

  }

  # Combine the results into single dataframe
  filter.results = as.data.frame(dplyr::bind_cols(f.results))
    # Create column names based on no.bands (e.g. f.result1, f.result2, etc)
    filt.col.names <- list()
    for (i in 1:no.bands){
      filt.col.names[i]<-paste0("f.result", i)
    }
    filt.col.names <- unlist(filt.col.names)
    colnames(filter.results)<- filt.col.names

  # Combine Results with original DF
  df <- cbind(df, filter.results)

  # Calculating effect of filter (Predicted Values + Filtered Residuals)
  filt_effect <- vector(mode="list", length=no.bands)
  for (i in 1:no.bands){
    filt_effect[[i]]<- as.data.frame((df[, filt.col.names[i] ] + predict_column))
  }


  if (plot==T){
      # Outputs List of Plots showing effect of Filter
      plots<-list()
      for (i in 1:no.bands){
        colnames(filt_effect[[i]]) <- "V1"
        df.plots <- cbind(df, filt_effect[[i]])

         p1 <- ggplot2::ggplot(df.plots, aes(x=daytime, y=predict_column)) + ggplot2::geom_line(color="black") +
           ggplot2::geom_line(aes(x=daytime, y=V1), color="indianred3") + ggplot2::labs(x="Time", y="Activity") +
           ggplot2::ggtitle(paste0("Effect of Filter [", round(freq.bands[i,1],3), ", ", round(freq.bands[i,2],3), "]")) + ggplot2::theme_bw()
        print(p1)
        plots[[i]] <- p1
        }

    objects <- list("df"=df, "plots"=plots, "filt_effect" = filt_effect, "filters" = filters, "f.results"=f.results)
    return(objects)

  }

  objects <- list("df"=df, "filt_effect"=filt_effect, "filters"=filters, "f.results"=f.results)
  return(objects)

}
