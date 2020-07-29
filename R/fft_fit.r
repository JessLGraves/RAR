fft_fit <- function(df, freq.bands, no.bands, sampling.rate){
  options(xts_check_TZ=FALSE)
  resid.ts <- xts::xts(df$resid, df$date_time)
  freq.convert <- 3600*sampling.rate # get numeric value to transform FFT frequency to cycles/hour

  # Perform FFT
  fft.fit <- stats::fft(resid.ts)

  # Covnert frequency of FFT of Residuals to cycles/hour
  freq.resid.hr <- freq.convert*(0:(length(fft.fit)-1))/(length(fft.fit))

  ### Apply Filters ###
  # Different rules depending on if low-pass, band, or high-pass filter
  filters <- vector(mode="list", length=no.bands)
  f.results <- vector(mode="list", length=no.bands)
  for (i in 1:no.bands){
    filters[[i]]$fft <- fft.fit

    # Low-pass filters ([0,X])
    if (freq.bands[i,1] == 0){
      filters[[i]]$elim <- (freq.resid.hr >= (freq.bands[i,2])) & (freq.resid.hr <= (freq.convert-freq.bands[i,2]))
      filters[[i]]$fft[filters[[i]]$elim] <- 0
      f.results[[i]] <- as.data.frame(Re(stats::fft(filters[[i]]$fft, inverse=T)/length(filters[[i]]$fft)))
    }

    # Band Filter ([X, Y])
    if ((freq.bands[i,1] !=0) & (freq.bands[i,2] !=60)){
      filters[[i]]$elim.out <- (freq.resid.hr < freq.bands[i,1]) | (freq.resid.hr > (freq.convert-freq.bands[i,1]))
      filters[[i]]$elim.in <- (freq.resid.hr > (freq.bands[i,2])) & (freq.resid.hr < (freq.convert-freq.bands[i,2]))
      filters[[i]]$fft[filters[[i]]$elim.out] <- 0
      filters[[i]]$fft[filters[[i]]$elim.in] <- 0
      f.results[[i]] <- as.data.frame(Re(stats::fft(filters[[i]]$fft, inverse=T)/length(filters[[i]]$fft)))
    }

    # High Pass Filter ([Y, 60])
    if ((freq.bands[i,2] == 60)){
      filters[[i]]$elim <- (freq.resid.hr <= freq.bands[i,1]) | (freq.resid.hr >= (freq.convert - freq.bands[i,1]))
      filters[[i]]$fft[filters[[i]]$elim] <- 0
      f.results[[i]] <- as.data.frame(Re(stats::fft(filters[[i]]$fft, inverse=T)/length(filters[[i]]$fft)))
    }

  }

  # Combine the results into single dataframe
  filter.results <- as.data.frame(dplyr::bind_cols(f.results))
  # Create column names based on no.bands (e.g. f.result1, f.result2, etc)
  filt.col.names <- list()
  for (i in 1:no.bands){
    filt.col.names[i] <- paste0("f.result", i)
  }
  filt.col.names <- unlist(filt.col.names)
  colnames(filter.results) <- filt.col.names

  # Combine Results with original DF
  df <- cbind(df, filter.results)

  # Calculating effect of filter (Predicted Values + Filtered Residuals)
  filt_effect <- vector(mode="list", length=no.bands)
  for (i in 1:no.bands){
    filt_effect[[i]] <- data.frame((df[, filt.col.names[i] ] + df$predicted))
    colnames(filt_effect[[i]]) <- paste0("f.effect", i)
  }

  filt_effect <- do.call(cbind, filt_effect)
  df <- cbind(df, filt_effect)

  return(df)
}

