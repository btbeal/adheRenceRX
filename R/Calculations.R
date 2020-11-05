#' Calculate Proportion Days Covered
#'
#' Calculate the proportion of days covered from a pharmaceutical claims database. This function is suggested only  
#' after one has properly adjusted their dates (\code{propagate_dates()}) and identified gaps in therapy
#' (\code{identify_gaps()}). Note that a proportion of days covered will never be greater than 1 and has a philosophy that 
#' overlapping fill dates should never count for gaps prior to. 
#' 
#'
#' @param .data data frame 
#' @param .max_date Character in year-month-date format, maximum date in consideration for adherence
#' @param .min_date Character in year-month-date format, minimum date in consideration for adherence
#' @param .summarise Logical value (defaulting to TRUE) indicating whether the output should be summarised or not
#' @return a summarised tibble, by default, with percentage of days a patient had medication coverage
#' @export
#'
#' @examples
#' library(adheRenceRX)
#' library(dplyr)
#' 
#' toy_claims %>% 
#'   group_by(ID) %>% 
#'   propagate_date() %>% 
#'   identify_gaps() %>% 
#'   calculate_pdc(.max_date = "2020-07-28", .min_date = "2020-01-01")
#' 

calculate_pdc <- function(.data, .max_date, .min_date, .summarise = TRUE){
  
  max <- ymd(.max_date)
  min <- ymd(.min_date)
  
  if(.summarise){
    .data %>% 
      filter(.data$adjusted_date >= min,
             .data$adjusted_date <= max) %>% 
      # shave gaps that extend past maximum date
      mutate(overlap = as.numeric(max - .data$adjusted_date),
             adjusted_gap = if_else(.data$overlap < .data$gap, .data$overlap, .data$gap)) %>% 
      summarise(adherence = 1 - sum(.data$gap)/as.numeric(max-min))
  } else {
    .data %>% 
      filter(.data$adjusted_date >= min,
             .data$adjusted_date <= max) %>% 
      # shave gaps that extend past maximum date
      mutate(overlap = as.numeric(max - .data$adjusted_date),
             adjusted_gap = if_else(.data$overlap < .data$gap, .data$overlap, .data$gap)) %>% 
      mutate(adherence = 1 - sum(.data$gap)/as.numeric(max-min))
  }
}




