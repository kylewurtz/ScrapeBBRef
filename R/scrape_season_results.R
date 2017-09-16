#' Scrape Results of Games for a Season
#'
#' This function scrapes the results for all games (regular season and playoffs)
#' for a given season.
#'
#' @param year A numerical value representing the season's data that should be
#'   scraped. NBA seasons typically span multiple calendar years, so use the
#'   latter of the two years. For example, if you want the data for the 2016-
#'   2017 season, pass 2017 to the function.
#' @param months A character vector of the months during which games were
#'   played for the season.
#'
#' @return
#' @export
#'
#' @examples
#' df_game_results_2017 = scrape_season_results(2017)
#' df_game_results_2017
scrape_season_results = function(
  year,
  months = c("october", "november", "december", "january",
             "february", "march", "april", "may", "june")
) {
  df_game_results = tibble(
    Day_of_Week = character(),
    Month = character(),
    Day = numeric(),
    Year = numeric(),
    Start_ET = character(),
    Away_Team = character(),
    Away_Score = numeric(),
    Home_Team = character(),
    Home_Score = numeric(),
    Away_Abbrev = character(),
    Home_Abbrev = character(),
    game_id = character(),
    game_url_id = character()
  )

  urls = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", months, ".html")
  for (url in urls) {
    page = read_html(url)
    df_month_results = page %>%
      html_nodes("#schedule") %>%
      html_table() %>%
      .[[1]]
    names(df_month_results) = c(
      "Date", "Start_ET", "Away_Team", "Away_Score",
      "Home_Team", "Home_Score", "Box", "OT_Ind", "Notes"
    )
    df_month_results %<>%
      select(-(Box:Notes)) %>%
      filter(Date != "Playoffs") %>%
      separate(Date, c("Day_of_Week", "Month_Day", "Year"), sep = ",") %>%
      mutate(Month_Day = str_trim(Month_Day)) %>%
      separate(Month_Day, c("Month", "Day")) %>%
      mutate(Day = as.numeric(Day),
             Year = as.numeric(Year),
             Away_Score = as.numeric(Away_Score),
             Home_Score = as.numeric(Home_Score)) %>%
      as_tibble() %>%
      left_join(team_abbrevs, by = c("Away_Team" = "Team_Name")) %>%
      rename(Away_Abbrev = Team_Abbrev) %>%
      left_join(team_abbrevs, by = c("Home_Team" = "Team_Name")) %>%
      rename(Home_Abbrev = Team_Abbrev) %>%
      mutate(
        Win_Team = ifelse(
          Away_Score > Home_Score, Away_Abbrev, Home_Abbrev
        ),
        Loss_Team = ifelse(
          Away_Score > Home_Score, Home_Abbrev, Away_Abbrev
        )
      ) %>%
      mutate(
        game_id = paste0(
          Year,
          str_pad(match(Month, month.abb), 2, "left", 0),
          str_pad(Day, 2, "left", 0),
          Away_Abbrev,
          Home_Abbrev
        ),
        game_url_id = paste0(
          Year,
          str_pad(match(Month, month.abb), 2, "left", 0),
          str_pad(Day, 2, "left", 0),
          0,
          Home_Abbrev
        )
      )

    df_game_results %<>% rbind(., df_month_results)
  }


  return(df_game_results)
}
