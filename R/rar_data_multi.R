#' Example Data for RAR_ExCosine_Multi()
#'
#' rar_data_multi.rda is an example dataframe illustrating the basic format RAR_ExCosine_Multi() will accept. Participant 1 is taken from rar_data3; Participant 2 is taken from rar_data2. Wake up hours are extracted from anti-logistic extended cosine model tLeft parameter estimates for each participant.
#'
#' Variables include:
#' \itemize{
#'   \item id: ID
#'   \item time: factor listing time of observation.
#'   \item act: observed activity (raw counts).
#' }
#' @docType data
#' @keywords datasets
#' @name rar_data_mutli_wake
#' @usage data(rar_data_multi)
#' @format A data frame with 51273 rows and 3 variables. Activity measurements are acquired for every minute (60-second epochs) for two participants (IDs = 1 and 2)
"rar_data_multi"

