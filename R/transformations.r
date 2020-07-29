#' Transformations
#'
#' Internal function used to fit sigmoidally transformed extended cosine model. As
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time.
#' @param transform specifies which transformation to use
#'
#' @export

transformations <- function(df, transform=c("antilogit", "arctan", "hill")){
  df <- df

  # Staring values for initial cosine model
  mMeanAct = mean(df$log.act)
  aStartAct = mMeanAct - min(df$log.act)
  start_cosine = as.data.frame(cbind(mMeanAct, aStartAct))

  ##### Initial Cosine Model
  cosine <- stats::nls(log.act ~  (amp + M) + amp*cos((tm/60 - phi)*pi/12), data=df,
                       start=list(amp=aStartAct, M=mMeanAct, phi=12), algorithm="port", control=list(warnOnly=T),
                       trace=F, lower=c(0, 0, -6), upper=c(Inf, Inf, 30))

  # Saving estimates of Initial Cosine Model to use as starting values for Fitted Extended Cosine Model
  cos.coefs <- stats::coef(cosine)

  if(transform=="antilogit"){
    ex_cosine <- stats::nls(log.act ~ mn + amp*(exp(beta*(cos((tm/60 - phi)*pi/12)- alpha))
                                                /(1+exp(beta*(cos((tm/60 - phi)*pi/12) - alpha)))), data = df,
                            start=list(amp=cos.coefs[[1]], mn=cos.coefs[[2]], phi=cos.coefs[[3]], alpha=0, beta=2),
                            control = list(maxiter = 500, warnOnly=T), trace=F, algorithm = "port",
                            lower=c(0, 0, -6, -1, 0), upper=c(Inf, Inf, 30, 1, Inf))
  } else if(transform=="arctan"){
    ex_cosine <- stats::nls(log.act ~ mn + amp* atan( beta*( cos((tm/60 - phi)*pi/12) - alpha) )/pi + 1/2, data = df,
                            start=list(amp=cos.coefs[[1]], mn=cos.coefs[[2]], phi=cos.coefs[[3]], alpha=0, beta=2),
                            control = list(maxiter = 500, warnOnly=T), trace=F, algorithm = "port",
                            lower=c(0, 0, -6, -1, 0), upper=c(Inf, Inf, 30, 1, Inf))
  } else if(transform=="hill"){
    ex_cosine <- stats::nls(log.act ~ mn + amp* ((cos((tm/60 - phi)*pi/12) + 1 )^gam)/(m_cons^gam + (cos((tm/60 - phi)*pi/12) + 1)^gam), data = df,
                            start=list(amp=cos.coefs[[1]], mn=cos.coefs[[2]], phi=cos.coefs[[3]], m_cons=0.5, gam=1.4),
                            control = list(maxiter = 500, warnOnly=T), trace=F, algorithm = "port",
                            lower=c(0, 0, -6, 0, 0), upper=c(Inf, Inf, 30, Inf, Inf))
  }

  # Coefficient estimates of extended cosine
  final.ests = as.data.frame(t(stats::coef(ex_cosine)))

  ##### Predicted Values & Residuals #####
  exp_trans <- function(x){return(exp(x) - 1)}

  # Fit predicted values based on observed non-missing data
  df$predicted <- as.numeric(stats::predict(ex_cosine, data.frame = ( tm = df$tm ) )) # Predicted based on log(act + 1)
  df$resid <- df$log.act - df$predicted # residuals of log(act + 1)

  # Predicting based on Interpolated Data
  obs.per.min <- df$tm[2] - df$tm[1]
  interp.td <- with(df, seq(min(td), max(td), by=obs.per.min)) # Create interpolated data
  interp.pred <- as.data.frame(as.numeric(stats::predict(ex_cosine, newdata = list(tm = interp.td)))) # Predictions based on interpolated data
  colnames(interp.pred) <- "interp.pred"
  interp.pred$td <- interp.td # number of minutes after first midnight

  # Predicted values and residuals exponentiated
  df$e_predicted <- exp_trans(df$predicted)
  df$e_resid <- exp_trans(df$log.act) - df$e_predicted
  interp.pred$e_interp.pred <- exp_trans(interp.pred$interp.pred)

  # Merge interpolated data with original data (for plotting purposes, keep all NAs)
  df_interp <- merge(df, interp.pred, by="td", all.y = TRUE)

  ##### Calculating F Statistic #####
  diff.mean.sq <- (df$predicted - mean(df$log.act))^2
  RSS <- sum(diff.mean.sq)/4
  MSE <- sum(df$resid^2)/(nrow(df)-5)

  F_stat <- RSS/MSE

  ##### Calculating Parameters ####
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

  # Objects to Return
  objects <- list("ex_cosine" = ex_cosine, "parameters" = parameters,
                  "df_predicted" = df, "df_interp" = df_interp, "transform" = transform)

  return(objects)
}
