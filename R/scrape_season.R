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

  df_game_box_scores = tibble(
    game_url_id = character(),
    box_basic_away = list(),
    box_basic_home = list(),
    box_adv_away = list(),
    box_adv_home = list(),
    four_factors = list()
  )

  for (i in 1:nrow(df_game_results)) {
    game_url_id = df_game_results$game_url_id[i]
    game_results = scrape_game(game_url_id)
    game_box_scores = game_results$game_box_scores
    df_game_box_scores = rbind(
      df_game_box_scores,
      tibble(
        game_url_id = game_url_id,
        box_basic_away = game_box_scores[1],
        box_basic_home = game_box_scores[2],
        box_adv_away = game_box_scores[3],
        box_adv_home = game_box_scores[4],
        four_factors = game_box_scores[5]
      )
    )
  }

  return(list(
    df_game_results = df_game_results,
    df_game_box_scores = df_game_box_scores
  ))

}
