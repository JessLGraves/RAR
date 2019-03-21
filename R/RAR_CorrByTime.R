#' RAR Correlation by Localized Measures
#'
#' This function correlates localized RAR measures for an entire sample (mean, standard deviation, and relative activity) at each time bin against an outcome of interest. This function will take a dataframe that contains RAR_Localized() measures with an outcome of interest already merged in. Or, it will take two separate dataframes, one with the RAR_Localized() measures and one with the outcome. If using two dataframes, you must specifiy the ID column, which will be used to merge the two together within the funciton.
#'
#' @author Jessica Graves
#' @param df_measures dataframe containing columns from RAR_Localized() for each subject. Mean, Standard Deviation, and Relative Activity columns must be named mean.act, sd.act, and rel.act, respectively. This dataframe must also have an id column. If dataframe is already merged with outcome of interest, leave df_outcome
#' @param df_outcome dataframe containing outcome of interest. This dataframe must also have an id column, which is named the same as df_measures id column.
#' @param id_column specifies the column name in df_measures and df_outcome that corresponds to the id
#' @param second_var specifies the column name in the dataframe that contains the outcome of interest.
#' @param corr_type specifies the type of correlation, e.g. ("pearson", "kendall", "spearman"). Remaining options are defaults of cor.test with exact p-value = FALSE
#'
#' @importFrom magrittr %>%
#' @export

RAR_CorrByTime <- function(df_measures, df_outcome=NULL, id_column=NULL, second_var, corr_type=c("pearson", "kendall", "spearman")){

    df_measures = as.data.frame(df_measures)

    if(!missing(df_outcome) & !missing(id_column)){
        df_outcome = as.data.frame(df_outcome)
        ids_outcome <- as.character(unique(eval(substitute(id_column), df_outcome, parent.frame())))
        ids_measures <- as.character(unique(eval(substitute(id_column), df_measures, parent.frame())))

        df_outcome_final = df_outcome[ids_outcome %in% ids_measures,]
        df = merge(df_measures, df_outcome_final, by=as.character(substitute(id_column)))

        if((as.character(substitute(id_column)) %in% colnames(df_measures) & as.character(substitute(id_column)) %in% colnames(df_outcome)) == FALSE){
            stop("Must have matching id column in each dataframe.")
        }

        if (as.character(substitute(second_var)) %in% colnames(df_outcome)[which(lapply(df_outcome, is.numeric)==TRUE)] !=TRUE) {
            stop("second_var must be numeric")
        }

       # second_var <- dplyr::enquo(second_var)
    }

    if(missing(df_outcome) & missing(id_column)){
        df = as.data.frame(df_measures)
        #second_var <- dplyr::enquo(second_var)
    }

    if(missing(corr_type)){
        corr_type = "spearman"
    }

    # Correlation function to send to dplyr
    corfun<-function(x, y) {
        corr=(stats::cor.test(x, y, alternative="two.sided", method=corr_type, exact=F))
    }

    # Making var names readible to dplyr
    bins <- NULL
    mean.corr <- NULL
    sd.corr <- NULL
    relative.corr <- NULL
    mean.act <- NULL
    rel.act <- NULL
    sd.act <- NULL
    second_var <- dplyr::enquo(second_var)

    # Correlation estimates for mean activity vs second_var
    mean.corr = df %>%
        dplyr::group_by(bins) %>%
        dplyr::summarise(correlation = corfun(mean.act, !! second_var)$estimate,  p.value=corfun(mean.act, !! second_var)$p.value,
                         statistic=corfun(mean.act, !! second_var)$statistic,
                         alt=corfun(mean.act, !! second_var)$alternative)
    mean.corr = as.data.frame(mean.corr)
    colnames(mean.corr)[2] <- "Corr. Mean Est."

    # Correlation estimates for SD activity vs second_var
    sd.corr = df %>%
        dplyr::group_by(bins) %>%
        dplyr::summarise(correlation = corfun(sd.act, !! second_var)$estimate,  p.value=corfun(sd.act, !! second_var)$p.value,
                         statistic=corfun(sd.act, !! second_var)$statistic,
                         alt=corfun(sd.act, !! second_var)$alternative)
    sd.corr = as.data.frame(sd.corr)
    colnames(sd.corr)[2] <- "Corr. SD Est."

    # Correlation estimates for Relative activity vs second_var
    relative.corr = df %>%
        dplyr::group_by(bins) %>%
        dplyr::summarise(correlation = corfun(rel.act, !! second_var)$estimate,  p.value=corfun(rel.act, !! second_var)$p.value,
                         statistic=corfun(rel.act, !! second_var)$statistic,
                         alt=corfun(rel.act, !! second_var)$alternative)
    relative.corr = as.data.frame(relative.corr)
    colnames(relative.corr)[2] <- "Corr. Rel. Est."

    # Most pertinent results
    results = cbind(mean.corr[1:3], sd.corr[2:3], relative.corr[2:3])

    objects <- list("results"=results, "mean.corr.est" = mean.corr, "sd.corr.est" = sd.corr, "rel.corr.est" = relative.corr)
    return(objects)
}
