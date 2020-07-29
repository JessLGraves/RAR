#' RAR_SpectrumBands
#'
#' Calculates area under predicted smoothed power spectrum based on user-specified frequency bands.
#'
#' @author Jessica Graves
#' @param rar_spectrum rar_spectrum default output from RAR_Spectrum().
#' @param freq.bands a matrix of frequency bands, e.g. (t(c(0, 2/24))), ranging between 0 and 60.
#' @param log_transform specifies if area should be measured on log or natural scale. If power spectrum was estimated on log scale, this should set to TRUE. Default is TRUE (log scale).
#' @param id_column name of column containing id if multiple subjects exist in dataframe. Default is NULL.
#'
#' @export
#'
#' @examples
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' rar_ex <- RAR(d, act, date_time, "antilogit")
#' spec <- RAR_Spectrum(rar_ex, "pss", TRUE)
#' RAR_SpectrumBands(spec, t(c(0, 2/24)), TRUE)
#'
#' # Multiple bands
#' f1 <- c(0, 2/24); f2 <- c(2/24, 25); f3 <- c(25, 60)
#' fs = as.data.frame(rbind(f1, f2, f3))
#' RAR_SpectrumBands(spec, fs, TRUE)
#'
#' # Multiple subjects, multiple bands
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' spec4 <- RAR_Spectrum(rar_ex4, "pss", TRUE, id)
#' RAR_SpectrumBands(spec4, fs, TRUE, id) # outputs bands for each participant

RAR_SpectrumBands <- function(rar_spectrum, freq.bands, log_transform=c(TRUE, FALSE), id_column=NULL){

  id <- . <- NULL

  df <- rar_spectrum$predict.ss

  if(missing(log_transform)){log_transform<-TRUE}

  if(missing(id_column)){
    return(band_analysis(df, freq.bands, log_transform))
  }

  if(!missing(id_column)){
    df$id <- as.factor(eval(substitute(id_column), df, parent.frame()))
    bands <- df %>% dplyr::group_by(id) %>% dplyr::do(bands = band_analysis(. , freq.bands, log_transform))

    bands_res <- NULL
    for(i in 1:length(unique(df$id))){
      bands_res[[i]] <- bands$bands[[i]]
      bands_res[[i]]$id <- as.character(unique(df$id)[i])
    }

    bands_res <- dplyr::bind_rows(bands_res)

    return(bands_res)
  }
}
