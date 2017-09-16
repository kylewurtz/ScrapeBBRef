#' Scrape Box Scores for a Game
#'
#' This function scrape's basic and advanced box score tables for the away and
#' home teams for a given game and returns them as data frames. It also
#' calculates the four factors for the teams and returns those results as a
#' data frame.
#'
#' Note that the Free Throw Rate calculation in the four factors table differs
#' from the Basketball Reference calculation. This calculation uses free throws
#' attempted in the numerator, as opposed to free throws made.
#'
#' @param game_url_id The 12-digit code used in the Basketball Reference URL
#'   for the game. e.g., "201701010ATL"
#'
#' @return A list of five data frames containing the basic and advanced box
#'   score results for both teams and the four factor results. The data frames
#'   are named df_box_basic_away, df_box_basic_home, df_box_adv_away,
#'   df_box_adv_home, and df_four_factors.
#' @export
#'
#' @examples
#' box_scores_201612150DEN = scrape_game_box_scores("201612150DEN")
scrape_game_box_scores = function(game_url_id) {
  url = paste0("https://www.basketball-reference.com/boxscores/", game_url_id, ".html")

  page = read_html(url)

  # get home and away abbreviations (needed for div)
  txt_desc = page %>%
    html_node("strong span") %>%
    html_text()
  txt_away = gsub(" at.*", "", txt_desc)
  txt_away = tolower(team_abbrevs$Team_Abbrev[team_abbrevs$Team_Name == txt_away])
  txt_home = gsub(".*at ", "", txt_desc)
  txt_home = gsub(" Box Score.*", "", txt_home)
  txt_home = tolower(team_abbrevs$Team_Abbrev[team_abbrevs$Team_Name == txt_home])

  # get basic stats
  # away basic
  df_basic_away = page %>%
    html_nodes(paste0("#box_", txt_away, "_basic")) %>%
    html_table(header = FALSE)
  df_basic_away = as_tibble(df_basic_away[[1]]) %>%
    clean_gm_basic_tbl()
  # home basic
  df_basic_home = page %>%
    html_nodes(paste0("#box_", txt_home, "_basic")) %>%
    html_table(header = FALSE)
  df_basic_home = as_tibble(df_basic_home[[1]]) %>%
    clean_gm_basic_tbl()


  # get advanced stats
  # away advanced
  df_adv_away = page %>%
    html_nodes(paste0("#box_", txt_away, "_advanced")) %>%
    html_table(header = FALSE)
  df_adv_away = as_tibble(df_adv_away[[1]]) %>%
    clean_gm_adv_tbl()
  # home advanced
  df_adv_home = page %>%
    html_nodes(paste0("#box_", txt_home, "_advanced")) %>%
    html_table(header = FALSE)
  df_adv_home = as_tibble(df_adv_home[[1]]) %>%
    clean_gm_adv_tbl()

  # calculate four factors
  # can't be scraped directly from BBRef, but can be calculated from box stats
  df_four_factors = tibble(
    Team = c(toupper(txt_away), toupper(txt_home)),
    eFG = c(
      df_adv_away$eFG[nrow(df_adv_away)],
      df_adv_home$eFG[nrow(df_adv_home)]
    ),
    TOV_Per = c(
      df_adv_away$TOV_Per[nrow(df_adv_away)],
      df_adv_home$TOV_Per[nrow(df_adv_home)]
    ),
    ORB_Per = c(
      df_adv_away$ORB_Per[nrow(df_adv_away)],
      df_adv_home$ORB_Per[nrow(df_adv_home)]
    ),
    FTA_to_FGA = c(
      df_basic_away$FTA[nrow(df_basic_away)] /
        df_basic_away$FGA[nrow(df_basic_away)],
      df_basic_home$FTA[nrow(df_basic_home)] /
        df_basic_home$FGA[nrow(df_basic_home)]
    )
  )

  return(
    list(
      df_box_basic_away = df_basic_away,
      df_box_basic_home = df_basic_home,
      df_box_adv_away = df_adv_away,
      df_box_adv_home = df_adv_home,
      df_four_factors = df_four_factors
    )
  )
}
