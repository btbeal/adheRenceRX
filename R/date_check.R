#' Restructuring Dates to Remove Overlap
#'
#' This is a function meant to be utilized within propagate_date() in order to adjust pharmaceutical claims dates to prevent
#' overlapping in adherence calculations, per Canfield SL, Zuckerman A, Anguiano RH, Jolly JA, DeClercq J.
#' Navigating the wild west of medication adherence reporting in specialty pharmacy. J Manag Care Spec Pharm. 2019;25(10):1073-77.
#'
#' @param df a claims data frame with a date of a medication claim and the corresponding days supply
#'
#' @import Rcpp
#' @return A new claims data frame with an appended column, "adjusted_date"
#' @export
#'
date_check <- function(df){
  adjusted_date <- df$date
  days_supply <- df$days_supply
  # length to loop over
  l_date <- length(adjusted_date)
  # Ensure multiple rows (else, no need dates to compare to)
  if(l_date < 2){
    return(df)
  } else {
    adjusted_date <- date_checkCpp(adjusted_date, days_supply)
    df$adjusted_date <- as_date(adjusted_date)
    return(df)
  }
}
