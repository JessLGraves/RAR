#' RAR: Extended Cosine Model
#'
#'This function fits sigmoidally transformed extended cosine model to activity data, as seen in  Marler et al. (2006).
#'
#' Outputs from this function include: coefficient estimates for baseline cosine model and user-specificed extended cosine transformation, predicted values, and parameter estimates of interest.
#'
#' @author Jessica Graves
#' @param df dataframe containing actigraphy data and time.
#' @param act_column name of the column within df that contains the activity count data.
#' @param time_column name of the column that contains date and time of observation. Time must be a POSIX object.
#' @param transform specifies which transformation to apply. Options include Hill Function ("hill"), Anti-Logistic ("antilogit"), or Arctangent ("arctan")
#' @param id_column name of column containing id if multiple subjects exist in dataframe.
#'
#' @export
#' @seealso \code{nls}
#' @references 1. Marler M.R., Gehrman P., Martin J.L., Ancoli-Israel S. (2006) The sigmoidally transformed cosine curve: a mathematical model for circadian rhythms with symmetric non-sinusoidal shapes. Stat Med. Nov 30;25(22):3893-904.
#' @examples
#' data(age_wise)
#' d <- age_wise[age_wise$id==1,]
#' rar_ex <- RAR(d, act, date_time)
#' rar_ex$parameters # parameter estimates
#' rar_ex$messages # convergence message
#'
#' # Multiple subjects
#' d4 <- age_wise[age_wise$id %in% c(1:4), ]
#' rar_ex4 <- RAR(d4, act, date_time, id_column=id)
#' rar_ex4$parameters # parameter estimates
#' rar_ex4$messages # convergence messages for each participant
#' rar_ex4$df_predicted # dataframe of obseved activity and predicted values
#' rar_ex4$df_interp # predictions based on interpolated data (i.e. missing data)

RAR <- function(df, act_column, time_column, transform=c("antilogit", "arctan", "hill"), id_column=NULL){

df <- df[stats::complete.cases(df), ]
df$date_time <- eval(substitute(time_column), df, parent.frame())
df$act <- eval(substitute(act_column), df, parent.frame())

id <- . <- NULL

if(missing(transform)){transform <- "antilogit"}

if(missing(id_column)){
  ##### Data Processing #####
  df <- processing(df)

  ##### Fitting Initial Cosine & Extended Cosine Model #####
  model <- transformations(df, transform)

  ##### Return #####
  objects <-list("model" = model, "parameters" = model$parameters, "transform" = transform,
                 "df_interp" = model$df_interp, "df_predicted" = model$df_predicted, "messages" = model$ex_cosine$message)
  return(objects)
}

if(!missing(id_column)){
  df$id <- as.factor(eval(substitute(id_column), df, parent.frame()))

  ##### Data Processing #####
  df <- df %>% dplyr::group_by(id) %>% dplyr::do(processing(. )) %>% as.data.frame()

  ##### Fitting Initial Cosine & Extended Cosine Model #####
  model <- df %>% dplyr::group_by(id) %>% dplyr::do(models = transformations(. , transform))

  parameters <- vector(mode="list", length=length(unique(df$id)))
  for(i in seq_along(parameters)){
    parameters[[i]] <- as.data.frame(model$models[[i]]$parameters)
    parameters[[i]]$id <- as.character(model$id[[i]])
  }
  parameters <- dplyr::bind_rows(parameters)

  df_pred <- NULL
  for(i in 1:length(unique(df$id))){
    df_pred[[i]] <- model$models[[i]]$df_predicted
    df_pred[[i]]$id <- rep(as.character(unique(df$id)[i]), nrow(df_pred[[i]]))
  }
  df_pred <- data.frame(dplyr::bind_rows(df_pred))

  df_interp <- NULL
  for(i in 1:length(unique(df$id))){
    df_interp[[i]] <- model$models[[i]]$df_interp
    df_interp[[i]]$id <- rep(as.character(unique(df$id)[i]), nrow(df_interp[[i]]))
  }
  df_interp <- data.frame(dplyr::bind_rows(df_interp))

  messages <- NULL
  ids <- NULL
  for(i in 1:length(unique(df$id))){
    ids <- rbind(ids, unique(df$id)[i])
    messages <- rbind(messages, model$models[[i]]$ex_cosine$message)
  }

  messages <- data.frame(cbind(ids, messages))
  colnames(messages) <- c("id", "message")

  ##### Return #####
  objects <- list("df" = df, "df_predicted" = df_pred, "df_interp" = df_interp, "model" = model,
                  "parameters" = parameters, "transform" = transform, "messages" = messages)
  return(objects)
}
}
