#' Identify Gaps in A Patient's Prescription Claims History
#'
#' @description Compute gaps in a patient's prescription claims history from the end of their prior fill 
#' after one has adjusted dates with \code{propagate_date()}. This function assumes that one has
#' arranged the dates and grouped appropriately outside of the function. The length of any gap will be appended 
#' to the row after the gap has occured.
#' 
#' @param .data data frame
#'
#' @return A new claims tibble with an appended column, "gap"
#' @export
#'
#' @examples 
#' library(adheRenceRX)
#' library(dplyr)
#' 
#' toy_claims %>% 
#'   filter(ID == "D") %>% 
#'   propagate_date(.date_var = date, .days_supply_var = days_supply) %>% 
#'   identify_gaps()
#'
#'
identify_gaps <- function(.data){
  
  .data %>% 
    mutate(
      gap = as.numeric(.data$adjusted_date - lag(.data$adjusted_date)),
      gap = .data$gap - lag(.data$days_supply),
      gap = if_else(is.na(.data$gap), 0, .data$gap)
    )
}

#' Summarise Gaps in Therapy
#'
#' This function serves as a convenience wrapper of dplyr::summarise(), which takes the grouped variables and
#' summarises their gaps in therapy. This function is to be used after propagate_date().
#'
#' @param .data Data to be piped into the function
#'
#' @return A summary of gaps in therapy
#' @export
#'
summarise_gaps <- function(.data){

  .data %>%
    mutate(gap = as.numeric(.data$adjusted_date - lag(.data$adjusted_date)),
           gap = .data$gap - lag(.data$days_supply)) %>%
    summarise(Summary_Of_Gaps = sum(.data$gap, na.rm = TRUE))
}
