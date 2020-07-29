#' Spectral Analysis of Residuals of Extended Cosine Model
#'
#' This function estimates and smooths the power spectrum of the residuals estimated from the extended cosine model (RAR()).
#'
#' Outputs from this function include: spectrum estimates as well as predicted smooth estimates.
#'
#' @author Haoyi Fu, Jessica Graves
#' @param rar_object default output from RAR().
#' @param method specifies the method of smoothing, penalized smoothing spline ("pss") or Whittle-likelihood ("whittle"). Default is "pss".
#' @param log_transform specifies if power spectrum should be estimated on log or natural scale. Default is TRUE (log scale).
#' @param id_column name of column containing id if multiple subjects exist in dataframe. Default is NULL.
#' @param ... additional parameters
#'
#' @export
#' @seealso \code{nls} \code{dplyr}
#' @references 1. Krafty, RT, Fu, H, Graves, JL, Bruce, SA, Hall, MH, & Smagula, SF (2019). Measuring Variability in Rest-Activity Rhythms from Actigraphy with Application to Characterizing Symptoms of Depression. Statistics in Biosciences, 1-20.
#' @references 2. Whittle, P (1953). Estimation and information in stationary time series. Arkiv för matematik, 2(5), 423–434.
#'
#' @examples
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' rar_ex <- RAR(d, act, date_time)
#' spec <- RAR_Spectrum(rar_ex, "pss", TRUE)
#' spec$spectrum_value # the estimated power spectrum
#' spec$predict.ss # the predicted smoothed power spectrum
#'
#' # Multiple subjects
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' spec4 <- RAR_Spectrum(rar_ex4, "pss", TRUE, id)
#' spec4$spectrum_value
#' spec4$predict.ss

RAR_Spectrum <- function(rar_object, method=c("pss", "whittle"), log_transform=c(TRUE, FALSE), id_column=NULL, ...){

  id <- . <- NULL

  options(scipen = 999)
  df<- as.data.frame(rar_object$df_predicted)
  df<- df[stats::complete.cases(df),] # Remove any NAs

  # Store residual and time column values
  # df$residuals<-eval(substitute(resid_column), df,parent.frame())
  # df$date_time<-eval(substitute(time_column), df, parent.frame())

  if(missing(log_transform)){log_transform <- TRUE}
  if(missing(method)){method<-"pss"}

  ### Spectral Analysis
  ## Single Subject
  if(missing(id_column)){
    return(spectrum_est(df, method, log_transform, ...))
  }

  ## Multiple Subjects
  if(!missing(id_column)){
    df$id <- as.factor(eval(substitute(id_column), df, parent.frame()))

    spec_multi <- df %>% dplyr::group_by(id) %>% dplyr::do(spec_results = spectrum_est(. , method, log_transform))

    spec_values <- NULL
    for(i in 1:length(unique(df$id))){
      spec_values[[i]] <- spec_multi$spec_results[[i]]$spectrum_value
      spec_values[[i]]$id <- as.character(unique(df$id)[i])
    }
    spec_values <- dplyr::bind_rows(spec_values)

    predict.ss <- NULL
    for(i in 1:length(unique(df$id))){
      predict.ss[[i]] <- spec_multi$spec_results[[i]]$predict.ss
      predict.ss[[i]]$id <- as.character(unique(df$id)[i])
    }
    predict.ss <- dplyr::bind_rows(predict.ss)

    objects <- list("spec_multi" = spec_multi, "spectrum_value" = spec_values, "predict.ss" = predict.ss,
                    "log_transform"=spec_multi$spec_results[[1]]$log_transform, "method"=spec_multi$spec_results[[1]]$method)
    return(objects)

  }
}
