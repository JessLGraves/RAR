#' Spectrum estimation
#'
#' Internal function used to estimate the power spectrum of residuals estimated from the RAR().
#'
#' @author Jessica Graves
#' @param df dataframe containing residuals from  extended cosine model from RAR() and date and time.
#' @param method specifies if smooth or penalized spline should be used. Default is smooth.
#' @param log_transform specifies if log power spectrum should be calculated. TRUE is default
#' @param ... additional parameters from astsa::mvspec() or gss::gssanova()
#'
#' @export

spectrum_est <- function(df, method=c("pss", "whittle"), log_transform=c(TRUE,FALSE), ...){
  df <- df

  resid.ts<-xts::xts(df$resid, df$date_time) # convert residuals to time series object

  #Perform spectrum analysis and extract spectrum and frequency information
  spectrum <- astsa::mvspec(resid.ts, log="no", plot = F, ...)
  spectrum_value <- data.frame(spectrum$spec, spectrum$freq)
  colnames(spectrum_value) <- c("sp", "frequency")
  spectrum_value$log_spec <- log(spectrum_value$sp + 1)
  spectrum_value$freq.cycles.hour <- 3600*spectrum_value$frequency # convert from cycles/second to cycles/hour

  spec <- eval(substitute(sp), spectrum_value, parent.frame())

  if(log_transform==T){
    spec <- eval(substitute(log_spec), spectrum_value, parent.frame())
  }

  freq <- eval(substitute(freq.cycles.hour), spectrum_value, parent.frame())

  if(method=="pss"){
    ss <- stats::smooth.spline(freq, spec, ...)
    predict.ss <- rbind(data.frame(stats::predict(ss, freq)))
  }

  if(method=="whittle"){
    ss <- gss::gssanova(spec ~ spectrum_value$frequency, family = "Gamma", ...)
    freq1 <- as.data.frame(spectrum_value$frequency) # cycles per second scale

    predict.ss <- NULL
    predict.ss$x <- freq
    predict.ss$y <- stats::predict(ss, freq1)
    predict.ss <- rbind(data.frame(predict.ss))
  }

  objects <- list("spectrum_value" = spectrum_value, "spec" = spec, "spectrum" = spectrum, "freq" = freq,
                  "ss" = ss, "predict.ss" = predict.ss, "log_transform" = log_transform, "method" = method)
  return(objects)
}
