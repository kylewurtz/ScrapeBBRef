#' Scrape Season's Data
#'
#' @param year A numerical value representing the season's data that should be
#'   scraped. NBA seasons typically span multiple calendar years, so use the
#'   latter of the two years. For example, if you want the data for the 2016-
#'   2017 season, pass 2017 to the function.
#'
#' @return
#' @export
#'
#' @examples
#'
scrape_season = function(year) {
  df_game_results = scrape_season_results(year)
}
