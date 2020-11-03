#' Ranking Episodes of Care
#' 
#' This is a helper function to assist \code{rank_episodes}
#'
#' @param df a data frame with "gap", "initial_rank", and "permi_gap" columns appended from \code{identify_gaps()}
#'
#' @return a data frame with an "episode" column appended, which ranks episodes of care in time
#' @export
episode_check <- function(df){
  gap <- df$gap
  epi_rank <- unique(df$initial_rank)
  perm_gap <- unique(df$permi_gap)
  l_df <- nrow(df)
  epi_vec <- c()
  i <- 1
  if(l_df < 2){
    return(df)
  } else {
    epi_vec <- episode_checkCpp(gap, perm_gap, epi_rank)
    df$episode <- epi_vec
    return(df)
  }
}

#' Rank Episodes of Care from Claims Data
#'
#' This function identifies and labels all episodes of care for a given patient in chronological order. It is meant to be used after one has
#' appropriately adjusted dates (\code{propagate_date()}) and identified gaps (\code{identify_gaps()}) and at the minimum assumes that a data frame has 
#' an adjusted_date column, which identifies the date after being adjusted for overlaps, and a gap column, which identifies gaps in therapy. Both of these will be generated
#' by the aforementioned functions within this package!
#' 
#' @param .data Data frame with "gap", "initial_rank", and "permi_gap" columns appended from \code{identify_gaps()}
#' @param .permissible_gap Integer value suggesting the maximum gap allowed before labeling a new episode of care
#' @param .initial_rank Integer value to identify what the first rank should be (defaults to 1). 
#'
#' @return a data frame with an "episode" column appended, which ranks episodes of care in time
#' @export
#'
#' @examples 
#' library(adheRenceRX)
#' library(dplyr)
#' 
#' toy_claims %>% 
#'   filter(ID == "D") %>% 
#'   propagate_date() %>% 
#'   identify_gaps() %>% 
#'   rank_episodes(.permissible_gap = 20, .initial_rank = 1)
#'   
rank_episodes <- function(.data, .permissible_gap = NULL, .initial_rank = 1){
  perm_gap <- .permissible_gap
  epi_rank <- .initial_rank
  
  if(is_grouped_df(.data)){
    # determine the grouping
    groupingVariable <- syms(group_vars(.data))
    
    .data %>% 
      arrange(.data$adjusted_date, .by_group = TRUE) %>%
      # Add columns to pass to episode_check 
      # -- Not as familiar with map below but could probably do that another way
      mutate(permi_gap = perm_gap,
             initial_rank = epi_rank) %>% 
      # Group nest the grouped df
      group_nest() %>% 
      # apply episode_check() to all groups
      mutate(episode = map(.data$data, episode_check)) %>% 
      unnest(.data$episode) %>%
      select(-.data$data) %>% 
      mutate(episode = if_else(is.na(.data$episode), .data$initial_rank, .data$episode)) %>% 
      # regroup by the grouping variables (to prevent constant grouping)
      # most of the time you're going to want to keep these groups
      group_by(!!!groupingVariable) %>% 
      select(-.data$permi_gap)
      
    
  } else {
    
    .data %>% 
      arrange(.data$adjusted_date, .by_group = TRUE) %>%
      # Add columns to pass to episode_check 
      # -- Not as familiar with map below but could probably do that another way
      mutate(permi_gap = perm_gap,
             initial_rank = epi_rank) %>% 
      nest(data = everything()) %>% 
      # apply episode_check() to all groups
      mutate(episode = map(.data$data, episode_check)) %>% 
      unnest(.data$episode) %>%
      select(-.data$data) %>% 
      mutate(episode = if_else(is.na(.data$episode), .data$initial_rank, .data$episode)) %>% 
      select(-.data$permi_gap)
    
    
  }
}

  






