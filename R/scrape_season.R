#' Scrape Season's Data
#'
#' Currently, this function is simply a wrapper funrction for
#' scrape_season_results, though in the future it will also serve as a wrapper
#' function for calls to scrape individual game data.
#'
#' @param year A numerical value representing the season's data that should be
#'   scraped. NBA seasons typically span multiple calendar years, so use the
#'   latter of the two years. For example, if you want the data for the 2016-
#'   2017 season, pass 2017 to the function.
#'
#' @return A data frame with all of the results for a season.
#' @export
#'
#' @examples
#'
scrape_season = function(year) {
  df_game_results = scrape_season_results(year)

  game_results = scrape_game(df_game_results$game_url_id[1])
  game_results$game_box_scores
}
