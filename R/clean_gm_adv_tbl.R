#' Clean Advanced Box Score Table
#'
#' This is a helper function that takes a scraped html_table with advanced box
#' score data for a game and returns a cleaned tibble with more readable/usable
#' names, etc.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
clean_gm_adv_tbl = function(df) {
  # names of columns
  adv_names = c(
    "Player", "MP", "TS", "eFG", "FG3A_Rate", "FT_Rate", "ORB_Per", "DRB_Per",
    "TRB_Per", "AST_Per", "STL_Per", "BLK_Per", "TOV_Per", "USG_Per", "ORTG",
    "DRTG"
  )

  df %<>% slice(2:n())
  names(df) = adv_names
  df %<>%
    filter(!(Player %in% c("Starters", "Reserves"))) %>%
    mutate_at(3:15, as.numeric)

  return(df)
}
