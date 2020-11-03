#' Tidy Verb to Forward-Adjusted Overlapping Pharmaceutical Claim Dates
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
#' @rawNamespace import(dplyr, except = data_frame)
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

  # Determine whether data is grouped to regroup at the end
  if(is_grouped_df(.data)){
    
    # determine the grouping
    groupingVariable <- syms(group_vars(.data))
    
    .data %>%
      # rename data appropriately to standardize with the rest of the functions
      # will consider more flexible applications at some point
      rename(date = !!date,
             days_supply = !!days_supply) %>%
      arrange(date, .by_group = TRUE) %>%
      group_nest() %>% 
      # apply date_check() to all groups
      mutate(propagated_date = map(.data$data, date_check)) %>%
      unnest(.data$propagated_date) %>%
      select(-.data$data) %>% 
      mutate(adjusted_date = if_else(is.na(.data$adjusted_date), .data$date, .data$adjusted_date)) %>% 
      # regroup by the grouping variables (to prevent constant grouping)
      # most of the time you're going to want to keep these groups
      group_by(!!!groupingVariable)
    
  } else {
    
    .data %>%
      rename(date = !!date,
             days_supply = !!days_supply) %>%
      arrange(date, .by_group = TRUE) %>%
      group_nest() %>% 
      mutate(propagated_date = map(.data$data, date_check)) %>%
      unnest(.data$propagated_date) %>%
      select(-.data$data) %>% 
      mutate(adjusted_date = if_else(is.na(.data$adjusted_date), .data$date, .data$adjusted_date))
  
  }
  

}
