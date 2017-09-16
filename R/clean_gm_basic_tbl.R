#' Clean Basic Box Score Table
#'
#' This is a helper function that takes a scraped html_table with basic box
#' score data for a game and returns a cleaned tibble with more readable/usable
#' names, etc.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
clean_gm_basic_tbl = function(df) {
  # names of columns
  basic_names = c(
    "Player", "MP", "FGM", "FGA", "FG_Per", "FG3M", "FG3A", "FG3_Per", "FTM",
    "FTA", "FT_Per", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF",
    "PTS", "Plus_Minus"
  )

  df %<>% slice(2:n())
  names(df) = basic_names
  df %<>%
    filter(!(Player %in% c("Starters", "Reserves"))) %>%
    mutate_at(3:21, as.numeric)

  return(df)
}
