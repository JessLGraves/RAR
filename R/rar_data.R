#' Example Data for RAR_ExCosine()
#'
#' rar_data.rda is an example dataframe illustrating the basic format RAR_ExCosine() will accept. Note that a day column is not needed.
#'
#' Variables include:
#' \itemize{
#'   \item time: factor listing time of observation.
#'   \item act: observed activity (raw counts).
#' }
#' @docType data
#' @keywords datasets
#' @name rar_data
#' @usage data(rar_data)
#' @format A data frame with 22905 rows and 2 variables. Activity measurements are acquired for every minute (60-second epochs). Number of observation days = 19.
"rar_data"

