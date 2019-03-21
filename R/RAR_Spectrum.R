#' RAR Residuals spectrum analysis and smoothing splines
#'
#' This function performs Residual Circadian Spectrum (RCS) analysis from rest-activity rhythm actigraphy data. The methodology can be found in detail in R.T Krafty et al, Measuring Variability in Rest-Activity Rhythms from Actigraphy with Application to Characterizing Symptoms of Depression (2019)
#'
#' @author Haoyi Fu
#' @param df dataframe including residuals calculating from extended cosine model based on actigraphy data. Residuals must be in numeric class. Time must be in HH:MM:SS format and stored as a character, factor, or POSIX object.
#' @param resid_column specfies the name of the column in the df that contains residuals. This must be in numeric class
#' @param time_column specifies the name of the column that contains time of observation for residuals
#' @param plot a logical value specifying if plots should be outputted
#' @param method specifies the method using for the smoothing spline. method="smooth spline" indicates using a cubic smoothing spline to the supplied data. method="penalized spline" indicates using a penalized likelihood regression smoothing spline method
#' @param log a logical value specifying if log-transfromation should be used for the power spectrum
#' @param freq.bands a kx2 matrix or dataframe specifcing frequency band intervals of interested for calculating area under spectrum, where the 1st column is the lower bound, and the 2nd column is the upper bound for the frequency band. Frequencies supplied must be in cycles/hour.
#' @param ...  additional arguments passed from astsa::mvspec, stats::smooth.spline,gss::gssanova
#' @return a list containg residuals and time, power spectrum and corresponding frequency, plots list and corresponding methods if plot argument is set to be true
#' @details This function will perform power spectrum analysis for residuals extracting from the extended cosine model. Plots will be draw with power spectrum lines and the smoothing curve. Log argument indicates whether log-transformation will be performed for spectrum. The function includes two methods for smoothing spline: cubic smoothing spline and penalized likelihood regression smoothing spline.
#' @seealso \code{mvspec} \code{gssanova} \code{smooth.spline}
#' @references 1. R.T Krafty et al, Measuring Variability in Rest-Activity Rhythms from Actigraphy with Application to Characterizing Symptoms of Depression (2019)
#' @references 2. Chong Gu Smoothing Spline ANOVA Models
#' @references 3. R.H Shumway, D.S Stoffer, Time Series Analysis and Its Applications
#' @importFrom stats smooth.spline
#' @importFrom stats predict
#' @importFrom xts xts
#' @importFrom astsa mvspec
#' @importFrom gss gssanova
#' @importFrom ggplot2 aes
#'
#' @examples
#' data(df_predicted)
#' spec = RAR_Spectrum(df_predicted, resid, time, TRUE, "smooth spline", TRUE) # Using smooth spline and no frequency band analysis
#' spec$plot_log_ss
#' f1 <- c(0, 2/24) # Filter Values
#' f2 <- c(2/24, 25)
#' f3 <- c(25, 60)
#' fs = as.data.frame(rbind(f1, f2, f3))
#' spec2 = RAR_Spectrum(df_predicted, resid, time, TRUE, "smooth spline", TRUE, fs)
#' spec2$freq.bands # to get scores
#' @export


RAR_Spectrum <- function(df, resid_column, time_column, plot=c(TRUE,FALSE), method=c("smooth spline", "penalized spline"),
                         log=c(TRUE,FALSE), freq.bands = NULL, ...){

    ###Check NAS and missing columns and plots ###

    options(scipen = 999)
    df=as.data.frame(df)
    df=df[stats::complete.cases(df),]     # Remove any NAs

    if(as.character(substitute(resid_column)) %in% colnames(df)==F){
        stop("resid_column must be in your dataframe")
    }

    if(as.character(substitute(time_column)) %in% colnames(df)==F){
        stop("time_column must be in your dataframe")
    }

    if(missing(plot)){
        plot=F
    }

     if(missing(freq.bands)){
         bands=F
     }

     if(!missing(freq.bands)){
         bands=T

         if(dim(freq.bands)[2] !=2) {
             stop("Number of freq.bands columns must be equal to 2.")}
     }

    ### Convert resid_column and time_column to correct formats ###
    # Store residual and time column values for future use

    resid_column<-eval(substitute(resid_column), df,parent.frame())
    time_column<-eval(substitute(time_column), df, parent.frame())

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
        df<-df
        df$daytime = as.POSIXct(time_column, format="%Y-%m-%d %H:%M:%S", tz="EST")
    }



 ### Power spectrum analysis ###
 # Create a time-series object for the spectrum analysis

 resid.ts<-xts::xts(resid_column, df$daytime)

 #Perform spectrum analysis and extract spectrum and frequency information

 spectrum<-astsa::mvspec(resid.ts, log="no", plot = F, ...)
 spectrum_value<-data.frame(spectrum$spec, spectrum$freq)
 colnames(spectrum_value)=c("sp", "frequency")
 spectrum_value$log_spec<-log(spectrum_value$sp + 1)
 spectrum_value$freq.cycles.hour<-3600*spectrum_value$frequency # convert from cycles/second to cycles/hour

 # Store spectrum, log-transformed spectrum, frequency (cycles/hour) for future analysis

 spec<-eval(substitute(sp), spectrum_value, parent.frame())
 log_spec<-eval(substitute(log_spec), spectrum_value, parent.frame())
 freq<-eval(substitute(freq.cycles.hour), spectrum_value, parent.frame())

 ### Frequency Band Analysis ###
 #if(bands==T){

 #     freq.bands <- as.data.frame(freq.bands)
 #     colnames(freq.bands)<-c("l.b", "u.b")
 #     freq.len = diff(spectrum_value$freq.cycles.hour)[1]
 #
 #     f.results = matrix(, nrow = dim(freq.bands[1]), ncol = 3)
 #     for (i in 1:dim(freq.bands)[1]){
 #         f.results[i,1] <- freq.bands$l.b[i]
 #         f.results[i,2] <- freq.bands$u.b[i]
 #         f.sp = spectrum_value$sp[ (spectrum_value$freq.cycles.hour >= freq.bands$l.b[i]) & (spectrum_value$freq.cycles.hour <= freq.bands$u.b[i]) ]
 #         f.results[i,3] <- sum(f.sp)*freq.len
 #     }
 #
 #     f.results = as.data.frame(f.results)
 #     colnames(f.results) <- c("l.bound", "u.bound", "value")
 # #}
 #
 # f.results = as.data.frame(f.results)
 # colnames(f.results) <- c("l.bound", "u.bound", "value")

 ### Plot  power spectrum and smoothing splines ###
 #Include two methods: general smoothing splines and penalized likelihood smoothing splines
 #log argument indicates whether a log transform will be performed for the spectrum

 if(plot==T){

     if(method=="smooth spline"){

         if(log==T){

             # Using smoothing spline and pass inside functions

             ss1<-stats::smooth.spline(freq,log_spec,...)
             predict.ss1<-stats::predict(ss1,freq)

             # Plot log-transformed spectrum and smoothing spline
             freq.cycles.hour <-eval(substitute(freq.cycles.hour),spectrum_value,parent.frame())

             plot_log_ss1<-ggplot2::ggplot(spectrum_value,aes(x=freq.cycles.hour, y=log_spec)) + ggplot2::geom_line(color="grey") +
                 ggplot2::geom_line(aes(x=predict.ss1$x, y=predict.ss1$y), color="blue", size=1) + ggplot2::theme_bw() +
                 ggplot2::labs(title="Log-transformed spectrum and cubic smoothing spline", x="Cycles/Hour", y="Log spectrum") +
                 ggplot2::theme(plot.title = element_text(hjust = 0.5))

             # Frequency Band Analysis
             if(bands==T){
                 freq.bands <- as.data.frame(freq.bands)
                 colnames(freq.bands)<-c("l.b", "u.b")
                 freq.len = diff(predict.ss1$x)[1]

                 f.results = matrix(, nrow = dim(freq.bands[1]), ncol = 3)
                 for (i in 1:dim(freq.bands)[1]){
                     f.results[i,1] <- freq.bands$l.b[i]
                     f.results[i,2] <- freq.bands$u.b[i]
                     f.sp = predict.ss1$y[ (predict.ss1$x >= freq.bands$l.b[i]) & (predict.ss1$x <= freq.bands$u.b[i]) ]
                     f.sp = exp(f.sp)
                     f.results[i,3] <- sum(f.sp)*freq.len
                 }

                 f.results = as.data.frame(f.results)
                 colnames(f.results) <- c("l.bound", "u.bound", "value")

                 objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,
                               "smooth_spline"=ss1,"predicts.ss"=predict.ss1,"plot_log_ss"=plot_log_ss1, "freq.bands"=f.results)
                 return(objects)
             }

             # Return all results in a list
             objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"smooth_spline"=ss1,
                           "predicts.ss"=predict.ss1,"plot_log_ss"=plot_log_ss1)
             return(objects)
         }

         if(log==F){

             ss2<-stats::smooth.spline(freq,spec,...)
             predict.ss2<-stats::predict(ss2,freq)

             # Plot natural spectrum and smoothing spline
             sp <- eval(substitute(sp), spectrum_value, parent.frame())
             freq.cycles.hour <-eval(substitute(freq.cycles.hour), spectrum_value, parent.frame())

             plot_ss2<-ggplot2::ggplot(spectrum_value, aes(x=freq.cycles.hour,y=sp)) + ggplot2::geom_line(color="grey") +
                 ggplot2::geom_line(aes(x=predict.ss2$x, y=predict.ss2$y), color="blue", size=1) + ggplot2::theme_bw() +
                 ggplot2::labs(title="Natural scale spectrum and cubic smoothing spline", x="Cycles/Hour", y="Spectrum") +
                 ggplot2::theme(plot.title = element_text(hjust = 0.5))

             # Frequency Band Analysis
             if(bands==T){
                 freq.bands <- as.data.frame(freq.bands)
                 colnames(freq.bands)<-c("l.b", "u.b")
                 freq.len = diff(predict.ss2$x)[1]

                 f.results = matrix(, nrow = dim(freq.bands[1]), ncol = 3)
                 for (i in 1:dim(freq.bands)[1]){
                     f.results[i,1] <- freq.bands$l.b[i]
                     f.results[i,2] <- freq.bands$u.b[i]
                     f.sp = predict.ss2$y[ (predict.ss2$x >= freq.bands$l.b[i]) & (predict.ss2$x <= freq.bands$u.b[i]) ]
                     #f.sp = exp(f.sp)
                     f.results[i,3] <- sum(f.sp)*freq.len
                 }

                 f.results = as.data.frame(f.results)
                 colnames(f.results) <- c("l.bound", "u.bound", "value")

                 objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"smooth_spline"=ss2,
                               "predicts.ss"=predict.ss2,"plot_ss"=plot_ss2, "freq.bands"=f.results)
                 return(objects)
             }

             objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"smooth_spline"=ss2,
                           "predicts.ss"=predict.ss2,"plot_ss"=plot_ss2)
             return(objects)
         }
     }

     if(method=="penalized spline"){

         if(log==T){

             # Use penalized likelihood smoothing spline and pass inside functions

             pss1<-gss::gssanova(log_spec ~ spectrum_value$frequency,family = "Gamma",...)
             freq1<-as.data.frame(spectrum_value$frequency)
             predict.pss1<-predict(pss1,freq1)

             # Plot log-transformed spectrum and penalized smoothing spline

             plot_log_pss1<-ggplot2::ggplot(spectrum_value, aes(x=freq.cycles.hour, y=log_spec)) + ggplot2::geom_line(color="grey") +
                 ggplot2::geom_line(aes(x=spectrum_value$freq.cycles.hour, y=exp(predict.pss1)),color="blue",size=1) +
                 ggplot2::theme_bw() + ggplot2::labs(title="Log-transformed spectrum and penalized smoothing spline", x="Cycles/Hour",y="Log spectrum") +
                 ggplot2::theme(plot.title = element_text(hjust = 0.5))

             # Frequency Band Analysis
             if(bands==T){
                 freq.bands <- as.data.frame(freq.bands)
                 colnames(freq.bands)<-c("l.b", "u.b")
                 freq.len = diff(spectrum_value$freq.cycles.hour)[1]

                 f.results = matrix(, nrow = dim(freq.bands[1]), ncol = 3)
                 for (i in 1:dim(freq.bands)[1]){
                     f.results[i,1] <- freq.bands$l.b[i]
                     f.results[i,2] <- freq.bands$u.b[i]
                     f.sp = exp(predict.pss1)[ (spectrum_value$freq.cycles.hour >= freq.bands$l.b[i]) & (spectrum_value$freq.cycles.hour <= freq.bands$u.b[i]) ]
                     f.sp = exp(f.sp)
                     f.results[i,3] <- sum(f.sp)*freq.len
                 }

                 f.results = as.data.frame(f.results)
                 colnames(f.results) <- c("l.bound", "u.bound", "value")
                 objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"penalized_ss"=pss1,
                               "predicts.pss"=predict.pss1,"plot_log_pss"=plot_log_pss1, "freq.bands"=f.results)
                 return(objects)
             }

             objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"penalized_ss"=pss1,
                           "predicts.pss"=predict.pss1,"plot_log_pss"=plot_log_pss1)
             return(objects)

         }

         if(log==F){

             pss2<-gss::gssanova(spec ~ spectrum_value$frequency,family = "Gamma",...)
             freq1<-as.data.frame(spectrum_value$frequency)
             predict.pss2<-predict(pss2,freq1)

             # Plot natural spectrum and penalized smoothing spline

             plot_pss2<-ggplot2::ggplot(spectrum_value,aes(x=freq.cycles.hour, y=sp)) + ggplot2::geom_line(color="grey") +
                 ggplot2::geom_line(aes(x=spectrum_value$freq.cycles.hour, y=exp(predict.pss2)),color="blue",size=1) +
                 ggplot2::theme_bw() + ggplot2::labs(title="Natural scale spectrum and penalized smoothing spline", x="Cycles/Hour",y="Spectrum") +
                 ggplot2::theme(plot.title = element_text(hjust = 0.5))

             # Frequency Band Analysis
             if(bands==T){
                 freq.bands <- as.data.frame(freq.bands)
                 colnames(freq.bands)<-c("l.b", "u.b")
                 freq.len = diff(spectrum_value$freq.cycles.hour)[1]

                 f.results = matrix(, nrow = dim(freq.bands[1]), ncol = 3)
                 for (i in 1:dim(freq.bands)[1]){
                     f.results[i,1] <- freq.bands$l.b[i]
                     f.results[i,2] <- freq.bands$u.b[i]
                     f.sp = exp(predict.pss2)[ (spectrum_value$freq.cycles.hour >= freq.bands$l.b[i]) & (spectrum_value$freq.cycles.hour <= freq.bands$u.b[i]) ]
                     #f.sp = exp(f.sp)
                     f.results[i,3] <- sum(f.sp)*freq.len
                 }

                 f.results = as.data.frame(f.results)
                 colnames(f.results) <- c("l.bound", "u.bound", "value")
                 objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"penalized_ss"=pss2,
                               "predicts.pss"=predict.pss2,"plot_pss"=plot_pss2, "freq.bands"=f.results)
                 return(objects)
             }

             objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value,"penalized_ss"=pss2,
                           "predicts.pss"=predict.pss2,"plot_pss"=plot_pss2)

             return(objects)
         }

     }

 }

 objects<-list("resid"=df,"resid.ts"=resid.ts,"spectrum"=spectrum,"spectrum_value"=spectrum_value)

}
