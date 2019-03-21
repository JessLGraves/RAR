#' Example Data for RAR_Localized_Multi()
#'
#' rar_data_multi_wake.rda is an example dataframe illustrating the basic format RAR_Localized_Multi() will accept. Participant 1 is taken from rar_data3; Participant 2 is taken from rar_data2. Wake up hours are extracted from anti-logistic extended cosine model tLeft parameter estimates for each participant.
#'
#' Variables include:
#' \itemize{
#'   \item id: ID
#'   \item time: factor listing time of observation.
#'   \item act: observed activity (raw counts).
#'   \item wakehr: average wake up time for each participant, calculated as tLeft from RAR_ExCosine() using anti-logistic transformation
#' }
#' @docType data
#' @keywords datasets
#' @name rar_data_mutli_wake
#' @usage data(rar_data_multi_wake)
#' @format A data frame with 51273 rows and 4 variables. Activity measurements are acquired for every minute (60-second epochs) for two participants (IDs = 1 and 2)
"rar_data_multi_wake"

