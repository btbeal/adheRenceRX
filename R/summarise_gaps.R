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
    mutate(gap = as.numeric(lead(.data$adjusted_date) - .data$adjusted_date),
           gap = .data$gap - .data$days_supply) %>%
    summarise(Summary_Of_Gaps = sum(.data$gap, na.rm = TRUE))
}
