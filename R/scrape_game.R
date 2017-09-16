#' Scrape Data for A Game
#'
#' This is a wrapper function for scrape_game_box_scores and
#' scrape_game_play_by_play (not implemented yet) functions.
#'
#' @param game_url_id The 12-digit code used in the Basketball Reference URL
#'   for the game. e.g., "201701010ATL"
#'
#' @return
#' @export
#'
#' @examples
scrape_game = function(game_url_id) {

  list_game_box_scores = scrape_game_box_scores(game_url_id)
  # scrape_game_play_by_play(game_url_id)

  return(list(
    list_game_box_scores = list_game_box_scores
  ))
}
