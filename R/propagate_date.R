#' Tidy Verb to Propagate Medical Claim Dates Forward
#'
#' To push supposedly overlapping dates forward for a specified group (e.g. patient ids or medication classes).
#' Used directly after a claims data frame has been grouped appropriately for adherence measurements per
#' Canfield SL, Zuckerman A, Anguiano RH, Jolly JA, DeClercq J.
#' Navigating the wild west of medication adherence reporting in specialty pharmacy. J Manag Care Spec Pharm. 2019;25(10):1073-77.
#'
#' @param .data Data to be piped into the function
#' @param date_var Date column (will default to 'date' if not specified)
#' @param days_supply_var Days supply column (will default to "days_supply" if none supplied)
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr map
#' @import lubridate
#' @importFrom rlang enquo !!
#'
#' @return A new claims data frame with an appended column, "adjusted_date"
#' @export
#'

propagate_date <- function(.data, date_var = date, days_supply_var = days_supply){

  # naming 'date_var' and 'days_supply_var' for consistent application
  date <- enquo(date_var)
  days_supply <- enquo(days_supply_var)

  .data %>%
    rename(date = !!date,
           days_supply = !!days_supply) %>%
    arrange(date) %>%
    nest(data = everything()) %>%
    mutate(propagated_date = map(.data$data, date_check)) %>%
    unnest(.data$propagated_date) %>%
    select(-.data$data)

}
