#' Frequency band analysis
#'
#' Internal function used to estimate area under power spectrum at particular frequency bands
#'
#' @author Jessica Graves
#' @param df dataframe of predicted smoothed power spectrum estimated from spectrum_est()
#' @param freq.bands matrix specifying lower and upper bound of frequency band (e.g. (t(c(0, 2/24)), or rbind(c(0,2/24), c(2/24,25), c(25, 60))). Bounds must be between 0 and 60)
#' @param log_transform specifies if log power spectrum should be calculated. TRUE is default
#'
#' @export

band_analysis <- function(df, freq.bands, log_transform=c(TRUE, FALSE)){
  spec_predicted <- df
  freq.bands <- as.data.frame(freq.bands)
  colnames(freq.bands) <- c("l.b", "u.b")
  freq.len <- diff(spec_predicted$x)[1]
  f.results <- matrix(nrow = dim(freq.bands[1]), ncol = 3)

  if(log_transform==T){
    for (i in 1:dim(freq.bands)[1]){
      f.results[i,1] <- freq.bands$l.b[i]
      f.results[i,2] <- freq.bands$u.b[i]
      f.sp <- spec_predicted$y[ (spec_predicted$x >= freq.bands$l.b[i]) & (spec_predicted$x <= freq.bands$u.b[i]) ]
      f.sp <- exp(f.sp)
      f.results[i,3] <- sum(f.sp)*freq.len
    }
  }

  if(log_transform==F){
    for (i in 1:dim(freq.bands)[1]){
      f.results[i,1] <- freq.bands$l.b[i]
      f.results[i,2] <- freq.bands$u.b[i]
      f.sp <- spec_predicted$y[ (spec_predicted$x >= freq.bands$l.b[i]) & (spec_predicted$x <= freq.bands$u.b[i]) ]
      # f.sp <- exp(f.sp)
      f.results[i,3] <- sum(f.sp)*freq.len
    }
  }

  f.results <- as.data.frame(f.results)
  colnames(f.results) <- c("l.bound", "u.bound", "value")

  return(f.results)

}
