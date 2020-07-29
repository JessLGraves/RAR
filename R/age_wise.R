#' Example dataset from AgeWise study
#'
#' age_wise.rda is baseline data  (n=57) from NIH-funded Aging Well, Sleeping Efficiently: Intervention Studies Program Project (P01 AG20677), also known as AgeWise. The primary goals of the study were intervention-based and emphasized caregiver stress management and sleep habits. Data for AgeWise was collected between November 2003 and June 2008.
#'
#' Variables include:
#' \itemize{
#'   \item date_time: date and time of observations, class: "POSIXct" "POSIXt"
#'   \item act: observed activity (raw counts).
#'   \item id: participant id
#' }
#' @docType data
#' @keywords datasets
#' @name age_wise
#' @usage data(age_wise)
#' @format A dataframe with 1,333,524 rows and 3 variables. Activity measurements are acquired for every minute (60-second epochs).
"age_wise"

