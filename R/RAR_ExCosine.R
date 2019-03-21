#' RAR Extended Cosine Model
#'
#' This function fits sigmoidally transformed extended cosine model to activity data, as seen in  Marler et al. (2006). Activity data provided to the function should include data for only one subject. Function takes dataframe with columns that include time and raw activity counts.
#'
#' Outputs from this function include: coefficient estimates for baseline cosine model and user-specificed extended cosline model; starting values for baseline cosine model; dataframe with predicted values based on fitted user-specified extended cosine; and parameter estimates of interest.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time. Time must be in HH:MM:SS format and stored as a character, factor, or POSIX object.
#' @param act_column specifies the name of the column within df that contains the activity count data. RAR_ExCosine will do a log(activity + 1) transformation.
#' @param time_column specifies the name of the column that contains time of observation
#' @param transform specifies which transformation to apply. Options include Hill Function ("hill"), Anti-Logistic ("antilogit"), or Arctangent ("arctan")
#' @param plot logical specifying if plots should be outputted. If missing, defaults to FALSE.
#'
#' @seealso \code{nls} \code{dplyr}
#' @references 1. Marler M.R., Gehrman P., Martin J.L., Ancoli-Israel S. (2006) The sigmoidally transformed cosine curve: a mathematical model for circadian rhythms with symmetric non-sinusoidal shapes. Stat Med. Nov 30;25(22):3893-904.
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @examples
#' data(rar_data)
#' rar_cosine = RAR_ExCosine(rar_data, act, time, "antilogit", plot=TRUE)
#' rar_cosine$final_parameters
#' rar_cosine$plot_log.act
#' @export

RAR_ExCosine <- function(df, act_column, time_column, transform=c("hill", "antilogit", "arctan"),
                         plot=c(TRUE, FALSE)){

    df = as.data.frame(df)
    df = df[stats::complete.cases(df),] # Removing any NAs in data

    td<-NULL
    log.act<-NULL
    day<-NULL
    e_interp.pred<-NULL

    if(as.character(substitute(act_column)) %in% colnames(df) == F){
        stop("act_column must be in your datafame")
        }

    if(as.character(substitute(time_column)) %in% colnames(df) == F){
        stop("time_column must be in your datafame")
    }

    if(missing(plot)){
        plot=F
    }

    if(missing(transform)){
        stop("Choose cosine transformation: hill, antilogit, arctan")
    }

    if(transform=="hill"){
        model_type = "Hill Function"
    } else if(transform=="antilogit"){
        model_type = "Anti-logistic Function"
    } else if(transform=="arctan"){
        model_type = "Arctangent Function"
    }

    # Making activity and time available to function & activity transform
    store_column_name <- substitute(act_column) # Storing column name for use when plotting

    act_column <- eval(substitute(act_column), df, parent.frame()) # Storing activity column values
    df$log.act = log(act_column + 1)

    time_column <- eval(substitute(time_column), df, parent.frame())

    # Convering time to POSIXct & Creating Day Index
    day_index = 1
    df$day = 1


        # If time not already in POSIXct form
        if(!any(class(time_column) == c("POSIXt"))){
            df$time.posixct = as.POSIXct(as.character(time_column), format="%H:%M:%S", tz="EST") #, format="%H:%M:%S"
            df$tm = lubridate::hour(df$time.posixct)*60 + lubridate::minute(df$time.posixct) + lubridate::second(df$time.posixct)/60
            df$td = df$tm[1]

            for (i in 2:length(time_column) ){
                day_delta = as.numeric(df$tm[i] < df$tm[i-1])
                day_index = day_index + day_delta
                df$day[i] = day_index
                df$td[i] = (day_index - 1) * 1440 + df$tm[i] # number of minutes after first midnight
            }
        }

        # If time already POSIXct form just create Day Index
        if(any(class(time_column) == c("POSIXt"))){
            df$tm = lubridate::hour(time_column)*60 + lubridate::minute(time_column) + lubridate::second(time_column)/60
            df$td = df$tm[1]

            for (i in 2:length(time_column)){
                day_delta = as.numeric(df$tm[i] < df$tm[i-1])
                day_index = day_index + day_delta
                df$day[i] = day_index
                df$td[i] = (day_index - 1) * 1440 + df$tm[i] # number of minutes after first midnight
            }
        }

    df$n = seq(1, nrow(df)) # indexing rows

    obs.per.min = df$tm[2] - df$tm[1]

    ##### Fitting Initial Cosine & Extended cosine Model ####
    act_column_log <- df$log.act # Storing log(act + 1) as vector

    # Staring values for initial cosine model
    mMeanAct = mean(act_column_log)
    aStartAct = mMeanAct - min(act_column_log)
    start_cosine = as.data.frame(cbind(mMeanAct, aStartAct))

        # Initial Cosine Model
    cosine <- stats::nls(act_column_log ~  mes + amp*cos((tm/60 - phi)*pi/12), data=df, start=list(amp=aStartAct, mes=mMeanAct, phi=12),
                         algorithm="port", trace=F, lower=c(0, 0, -6), upper=c(Inf, Inf, 30))

    # Saving estimates of Initial Cosine Model to use as starting values for Fitted Extended Cosine Model
    cos.coefs = stats::coef(cosine)

    # Fitting Exteded Cosine Model

    if(transform=="antilogit"){

    ex_cosine <- stats::nls(act_column_log ~ mn + amp*(exp(beta*(cos((tm/60 - phi)*pi/12)- alpha))
                                                       /(1+exp(beta*(cos((tm/60 - phi)*pi/12) - alpha)))), data = df,
                            start=list(amp=cos.coefs[[1]], mn=cos.coefs[[2]], phi=cos.coefs[[3]], alpha=0, beta=2),
                            control = list(maxiter = 500, warnOnly=T), trace=F, algorithm = "port",
                            lower=c(0, 0, -6, -1, 0), upper=c(Inf, Inf, 30, 1, Inf))
    }

    if(transform=="arctan"){
    ex_cosine <- stats::nls(act_column_log ~ mn + amp* atan( beta*( cos((tm/60 - phi)*pi/12) - alpha) )
                                                        /pi + 1/2, data = df,
                                       start=list(amp=cos.coefs[[1]], mn=cos.coefs[[2]], phi=cos.coefs[[3]], alpha=0, beta=2),
                                       control = list(maxiter = 500, warnOnly=T), trace=F, algorithm = "port",
                                       lower=c(0, 0, -6, -1, 0), upper=c(Inf, Inf, 30, 1, Inf))
    }

    if(transform=="hill"){
    ex_cosine <- stats::nls(act_column_log ~ mn + amp* ((cos((tm/60 - phi)*pi/12) + 1 )^gam)
                                                        /(m_cons^gam + (cos((tm/60 - phi)*pi/12) + 1)^gam), data = df,
                                        start=list(amp=cos.coefs[[1]], mn=cos.coefs[[2]], phi=cos.coefs[[3]], m_cons=0.5, gam=1.4),
                                        control = list(maxiter = 500, warnOnly=T), trace=F, algorithm = "port",
                                        lower=c(0, 0, -6, 0, 0), upper=c(Inf, Inf, 30, Inf, Inf))
    }

    # Coefficient estimates of extended cosine
    final.ests = as.data.frame(t(stats::coef(ex_cosine)))

    ##### Predicted Values & Residuals #####
    exp_trans <- function(x){return(exp(x) - 1)} # Function for transforming predictions & residuals back to raw counts

        # Fit predicted values based on current, non-missing, data
        predicted = stats::predict(ex_cosine, data.frame = ( tm = df$tm ) ) # Predicted based on log(act + 1)
        df$predicted = as.numeric(predicted)

        # Residuals based on current data
        df$resid = stats::residuals(ex_cosine) # residuals of log(act + 1)

        # Predicting based on Interpolated Data
        interp.td = with(df, seq(min(td), max(td), by=obs.per.min)) # Create interpolated data
        interp.pred = as.data.frame(as.numeric(stats::predict(ex_cosine, newdata = list(tm = interp.td)))) # Predictions based on interpolated data
        colnames(interp.pred) <- "interp.pred"
        interp.pred$td = interp.td # number of minutes after first midnight

        # Predicted values and residuals exponentiated
        df$e_predicted = exp_trans(df$predicted)
        df$e_resid = act_column - df$e_predicted
        interp.pred$e_interp.pred = exp_trans(interp.pred$interp.pred)

        # Merge interpolated data with original data (for plotting purposes, keep all NAs)
        df_interp = merge(df, interp.pred, by="td", all.y = TRUE)
        #df_interp = df_interp[,c(2:8,1,9:15)]

    #### Calculating F Statistic ####
    # as described in Krafty et al. (2018)
    global.mean = mean(df$log.act) # global mean activity

        ### Calculating RSS
        diff.mean.sq = (df$predicted - global.mean)^2
        RSS = sum(diff.mean.sq)/4

        ### Calculating MSE
        diff.obs.sq = df$resid^2
        MSE = sum(diff.obs.sq)/(nrow(df)-5)

    F_stat  = RSS/MSE

    #### Calculating Parameters of Interest ####
    if(transform=="antilogit"){
    alpha = final.ests$alpha
    beta = final.ests$beta
    acrophase = final.ests$phi
    amp = exp(final.ests$amp)
    mesor = exp(final.ests$mn + final.ests$amp/2)
    log_amp = final.ests$amp
    log_mesor = final.ests$mn + final.ests$amp/2
    tLeft = final.ests$phi - acos(final.ests$alpha)/(2*pi/24)
    tRight = final.ests$phi + acos(final.ests$alpha)/(2*pi/24)

    parameters <- as.data.frame(cbind(alpha, beta, acrophase, amp, mesor, log_amp, log_mesor, tLeft, tRight, F_stat))
    } else if(transform=="arctan"){
    alpha = final.ests$alpha
    beta = final.ests$beta
    acrophase = final.ests$phi
    amp = exp(final.ests$amp)
    mesor = exp(final.ests$mn + final.ests$amp/2)
    log_amp = final.ests$a
    log_mesor = final.ests$mn + final.ests$amp/2
    tLeft = final.ests$phi - acos(final.ests$alpha)/(2*pi/24)
    tRight = final.ests$phi + acos(final.ests$alpha)/(2*pi/24)

    parameters <- as.data.frame(cbind(alpha, beta, acrophase, amp, mesor, log_amp, log_mesor, tLeft, tRight, F_stat))
    } else if(transform=="hill"){
    michaelis = final.ests$m_cons
    gamma = final.ests$gam
    acrophase = final.ests$phi
    amp = exp(final.ests$amp)
    mesor = exp(final.ests$mn + final.ests$amp/2)
    log_amp = final.ests$amp
    log_mesor = final.ests$mn + final.ests$amp/2
    tLeft = final.ests$phi - acos(2*final.ests$m_cons - 1)/(2*pi/24)
    tRight = final.ests$phi + acos(2*final.ests$m_cons - 1)/(2*pi/24)

    parameters <- as.data.frame(cbind(michaelis, gamma, acrophase, amp, mesor, log_amp, log_mesor, tLeft, tRight, F_stat))
    }

    #### Plotting RAR & Fitted Model ####
    # Generate labels for plots
    if(plot==T){
        day_labels = vector(mode="character", length=max(df$day))
            for (i in 1:max(df$day)){
                day_labels[i] <- paste0("Day ", i)
            }

    # Raw activity including from interpolated data (includes NAs)
    act_column_interp <- eval(store_column_name, df_interp, parent.frame())

    # Plot Log(Activity + 1)
    plot_log.act <- ggplot2::ggplot(df_interp, aes(x=td, y=log.act, group=1)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
       ggplot2::theme_bw() + ggplot2::geom_line(data=df_interp, aes(x=td, y=interp.pred, group=1, color=as.factor(is.na(day))), size=1) +
       ggplot2::labs(title=paste0("Observed and Predicted Activity over total observation period: ", model_type),
                      x="Days of Observation", y="Log(activity + 1)", col="Missing Data") +
        ggplot2::scale_x_continuous(breaks=seq(0,max(df_interp$td), by=1440), labels = day_labels) +
       ggplot2::theme(plot.title = element_text(hjust = 0.5))

    # Plot Raw Activity
    plot_act <- ggplot2::ggplot(df_interp, aes(x=td, y=act_column_interp)) + ggplot2::geom_point(size=1, color="grey20", shape=1) +
       ggplot2::theme_bw() + ggplot2::geom_line(data=df_interp, aes(x=td, y=e_interp.pred, group=1, color=as.factor(is.na(day))), size=1) +
       ggplot2::labs(title=paste0("Observed and Predicted Activity over total observation period: ", model_type),
                     x="Days of Observation", y="Activity (raw)", col="Missing Data") +
        ggplot2::scale_x_continuous(breaks=seq(0,max(df_interp$td), by=1440), labels = day_labels) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))


        objects <- list("cosine" = cosine, "cos_start_vals" = start_cosine,
                        "ex_cosine" = ex_cosine, "df_predicted" = df, "final_parameters" = parameters,
                        "plot_log.act" = plot_log.act, "plot_act" = plot_act, "df_interp"= df_interp)

        return(objects)
    }

    objects <- list("cosine" = cosine, "cos_start_vals" = start_cosine,
                    "ex_cosine" = ex_cosine, "df_predicted" = df, "final_parameters" = parameters, "df_interp"= df_interp)
        return(objects)
}
