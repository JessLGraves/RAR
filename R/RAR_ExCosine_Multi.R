#' RAR Extended Cosine Model: For Files with Mulitple Participants
#'
#' This function peforms the same functionality as RAR_ExCosine(), excpet that it accepts a dataframe with a third column specifying ID to allow for iteration across subjects.
#'
#' Outputs from this function include a tibble object containing RAR_ExCosine_Out for each participant.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data, time, and ID column. Time must be in HH:MM:SS format and stored as a character, factor, or POSIX object.
#' @param act_column specifies the name of the column within df that contains the activity count data. RAR_ExCosine will do a log(activity + 1) transformation.
#' @param time_column specifies the name of the column that contains time of observation
#' @param transform specifies which transformation to apply. Options include Hill Function ("hill"), Anti-Logistic ("antilogit"), or Arctangent ("arctan")
#' @param id_column specifies the name of the column that contains ID information.
#' @param plot logical specifying if plots should be outputted. If missing, defaults to FALSE.
#'
#' @seealso \code{nls} \code{dplyr}
#' @references 1. Marler M.R., Gehrman P., Martin J.L., Ancoli-Israel S. (2006) The sigmoidally transformed cosine curve: a mathematical model for circadian rhythms with symmetric non-sinusoidal shapes. Stat Med. Nov 30;25(22):3893-904.
#' @importFrom magrittr %>%
#' @examples
#' data(rar_data_multi)
#' rar_multi = RAR_ExCosine_Multi(rar_data_multi, act, time, "antilogit", id, plot=TRUE)
#' rar_multi$RAR_Ouput # tibble object of participants and objects
#'
#' rar_multi$RAR_Output$RAR_ExCosine_Out[[1]]$final_parameters # individ participant estimates
#' rar_multi$RAR_Output$RAR_ExCosine_Out[[1]]$plot_log.act # individ participant plots
#'
#' rar_multi$final_parameters # df of parameter estimates for all participants
#' @export

RAR_ExCosine_Multi <- function(df, act_column, time_column, transform=c("hill", "antilogit", "arctan"), id_column, plot=c(TRUE, FALSE)){

    id <- NULL
    activity <- NULL
    . <- NULL
    time <- NULL

    df = as.data.frame(df)
    df = df[stats::complete.cases(df),] # Removing any NAs in data

    act_col <- eval(substitute(act_column), df, parent.frame())
    time_col <- eval(substitute(time_column), df, parent.frame())
    id_col <- eval(substitute(id_column), df, parent.frame())

    model_type = as.character(transform)
    plot_select = plot

    df$id = as.factor(id_col)
    df$time = time_col
    df$activity = act_col

    df = df[, c("time", "activity", "id")]

    RAR_Output <- df %>% dplyr::group_by(id) %>%
                dplyr::do(RAR_ExCosine_Out = RAR_ExCosine(df = . , act_column = activity,
                                                          time_column = time, transform = model_type, plot = plot_select))

    ids = unique(df$id)
    final_parameters <- vector(mode="list", length=length(ids))
    for (i in 1:length(final_parameters)){
        final_parameters[[i]] <- as.data.frame(RAR_Output$RAR_ExCosine_Out[[i]]$final_parameters)
        final_parameters[[i]]$id <- as.character(RAR_Output$id[[i]])
    }

    final_parameters <- dplyr::bind_rows(final_parameters)

    objects <- list("final_parameters" = final_parameters, "RAR_Output" = RAR_Output)
    return(objects)
}
