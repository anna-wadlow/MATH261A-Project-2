# NHL Data Extraction and Cleaning
# MATH261A Project 2
# Anna Wadlow


data_dir <- here::here("MATH261A-Project-2")
# ------------- libraries -------------
library(here)
library(nhlscraper)
library(dplyr)
library(tidyverse)
library(tidyr)
library(purrr)

# ------------- pull all games and team details -------------

all_games <- games() # extract details of all games in NHL history
all_teams <- teams() # extract details of all teams in NHL history

# ------------- by game scores summarized by player and season ------------- 

  ## -------------get by-player by-game details for seasons 2018-2019 through 2024-2025 ------------- 
scoring_per_game <- rbind(skater_game_report(season = 20242025, game_type = 2, category = "scoringpergame"),
                          skater_game_report(season = 20232024, game_type = 2, category = "scoringpergame"),
                          skater_game_report(season = 20222023, game_type = 2, category = "scoringpergame"),
                          skater_game_report(season = 20212022, game_type = 2, category = "scoringpergame"),
                          skater_game_report(season = 20202021, game_type = 2, category = "scoringpergame"),
                          skater_game_report(season = 20192020, game_type = 2, category = "scoringpergame"),
                          skater_game_report(season = 20182019, game_type = 2, category = "scoringpergame"))

  ## ------------ append season id ------------
scoring_per_game <- scoring_per_game %>%
  mutate(
    seasonId = as.numeric(paste0(substr(scoring_per_game$gameId,1,4),
                    as.numeric(substr(scoring_per_game$gameId,1,4))+1)
  ))


  ## ------------ summarize all data by player position, team, and season ------------
season_points_summary <- scoring_per_game %>%
  mutate(
    position_type = case_when(
      positionCode %in% c("C", "L", "R") ~ "fwd",
      positionCode %in% c("D") ~ "def",
      TRUE ~ "other"
    )
  ) %>%
  group_by(teamAbbrev, position_type, seasonId) %>%
  summarise(
    points    = sum(points, na.rm = TRUE),
    goals     = sum(goals, na.rm = TRUE),
    primary   = sum(totalPrimaryAssists, na.rm = TRUE),
    secondary = sum(totalSecondaryAssists, na.rm = TRUE),
    assists   = sum(assists, na.rm = TRUE),
    .groups   = "drop"
    ) %>%
  pivot_wider(
    names_from = position_type,
    values_from = c(points, goals, primary, secondary, assists),
    names_glue = "{.value}_{position_type}",
    values_fill = 0
  ) %>%
  mutate(
    team_points    = points_def + points_fwd,
    pct_points_def    = points_def    / team_points
  )
# ------------- goals for/goals against by team and season ----------------------------------
gfga_byteam_season <- rbind(
  team_season_report(season = 20242025, game_type = 2, category = "goalsforbystrength"),
  team_season_report(season = 20232024, game_type = 2, category = "goalsforbystrength"),
  team_season_report(season = 20222023, game_type = 2, category = "goalsforbystrength"),
  team_season_report(season = 20212022, game_type = 2, category = "goalsforbystrength"),
  team_season_report(season = 20202021, game_type = 2, category = "goalsforbystrength"),
  team_season_report(season = 20192020, game_type = 2, category = "goalsforbystrength"),
  team_season_report(season = 20182019, game_type = 2, category = "goalsforbystrength"))

# add team abbreviation
gfga_byteam_season$teamAbbrev <- all_teams$triCode[match(gfga_byteam_season$teamFullName, all_teams$fullName)]

# subset to columns of interest for control variables
gfga_small <- subset(gfga_byteam_season, 
                     select = c(seasonId, teamAbbrev, points, wins, losses, otLosses, gamesPlayed, goalsFor, goalsAgainst))

# ------------- save percentage by by team and season ----------------------------------
# extract save percentages and other relevant variables for regular season games
save_pct <- rbind(
  team_season_report(season = 20242025, game_type = 2, category = "savePercentage"),
  team_season_report(season = 20232024, game_type = 2, category = "savePercentage"),
  team_season_report(season = 20222023, game_type = 2, category = "savePercentage"),
  team_season_report(season = 20212022, game_type = 2, category = "savePercentage"),
  team_season_report(season = 20202021, game_type = 2, category = "savePercentage"),
  team_season_report(season = 20192020, game_type = 2, category = "savePercentage"),
  team_season_report(season = 20182019, game_type = 2, category = "savePercentage"))

# add team abbreviation
save_pct$teamAbbrev <- all_teams$triCode[match(save_pct$teamFullName, all_teams$fullName)]

# subset to columns of interest for control variables
save_pct_small <- subset(save_pct, 
                         select = c(seasonId, teamAbbrev, savePct, saves, shotsAgainst))


# -------------- calculate Fenwick For% ----------------

  ## ------------ r subset all_game ids to seasons 18-19 through 24-25 ------------
game_id_18_25 <- all_games %>%
  mutate(
    seasonFactor = as.factor(season)) %>%
  filter(as.numeric(seasonFactor) > 101)

game_id_18_25$homeTeamAbbrev <- all_teams$triCode[match(game_id_18_25$homeTeamId, all_teams$id)]
game_id_18_25$visitingTeamAbbrev <- all_teams$triCode[match(game_id_18_25$visitingTeamId, all_teams$id)]

  ## ------------ get game center play-by-play data for seasons 20182019 thru 20242025 ------------

gc_1819 <- gc_play_by_plays(season = 20182019)
gc_1920 <- gc_play_by_plays(season = 20192020)
gc_2021 <- gc_play_by_plays(season = 20202021)
gc_2122 <- gc_play_by_plays(season = 20212022)
gc_2223 <- gc_play_by_plays(season = 20222023)
gc_2324 <- gc_play_by_plays(season = 20232024)
gc_2425 <- gc_play_by_plays(season = 20242025)

  ## ------------ add flag for home ------------
flagged_gc_1819 <- flag_is_home(gc_1819)
flagged_gc_1920 <- flag_is_home(gc_1920)
flagged_gc_2021 <- flag_is_home(gc_2021)
flagged_gc_2122 <- flag_is_home(gc_2122)
flagged_gc_2223 <- flag_is_home(gc_2223)
flagged_gc_2324 <- flag_is_home(gc_2324)
flagged_gc_2425 <- flag_is_home(gc_2425)

  ## ------------ add counts for fenwick, corsi, etc ------------
count_gc_1819 <- count_goals_shots(flagged_gc_1819)
count_gc_1920 <- count_goals_shots(flagged_gc_1920)
count_gc_2021 <- count_goals_shots(flagged_gc_2021)
count_gc_2122 <- count_goals_shots(flagged_gc_2122)
count_gc_2223 <- count_goals_shots(flagged_gc_2223)
count_gc_2324 <- count_goals_shots(flagged_gc_2324)
count_gc_2425 <- count_goals_shots(flagged_gc_2425)

  ## ------------ append with season ------------
count_gc_1819$seasonId <- 20182019
count_gc_1920$seasonId <- 20192020
count_gc_2021$seasonId <- 20202021
count_gc_2122$seasonId <- 20212022
count_gc_2223$seasonId <- 20222023
count_gc_2324$seasonId <- 20232024
count_gc_2425$seasonId <- 20242025

  ## ------------ add home and visiting team abbreviations. add game type ------------
fenwick1819 <- count_gc_1819 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_1819$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_1819$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_1819$gameId, game_id_18_25$id)])

fenwick1920 <- count_gc_1920 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_1920$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_1920$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_1920$gameId, game_id_18_25$id)])

fenwick2021 <- count_gc_2021 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_2021$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_2021$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_2021$gameId, game_id_18_25$id)])

fenwick2122 <- count_gc_2122 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_2122$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_2122$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_2122$gameId, game_id_18_25$id)])

fenwick2223 <- count_gc_2223 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_2223$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_2223$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_2223$gameId, game_id_18_25$id)])

fenwick2324 <- count_gc_2324 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_2324$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_2324$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_2324$gameId, game_id_18_25$id)])

fenwick2425 <- count_gc_2425 %>%
  mutate(
    homeTeamAbbrev = game_id_18_25$homeTeamAbbrev[match(count_gc_2425$gameId, game_id_18_25$id)],
    awayTeamAbbrev = game_id_18_25$visitingTeamAbbrev[match(count_gc_2425$gameId, game_id_18_25$id)],
    gameType = game_id_18_25$gameType[match(count_gc_2425$gameId, game_id_18_25$id)])

  ## ------------ filter to regular season games, add teamAbbrev column, sum fenwick for/against ------------ 
# ------ 18-19 Season
fenwick1819_summary <- fenwick1819 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick1819 <- fenwick1819_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))

# ------ 19-20 season
fenwick1920_summary <- fenwick1920 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick1920 <- fenwick1920_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))
# ------ 20-21 season
fenwick2021_summary <- fenwick2021 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick2021 <- fenwick2021_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))

# ------ 21-22 season
fenwick2122_summary <- fenwick2122 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick2122 <- fenwick2122_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))

# ------ 22-23 season
fenwick2223_summary <- fenwick2223 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick2223 <- fenwick2223_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))

# ------ 23-24 season
fenwick2324_summary <- fenwick2324 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick2324 <- fenwick2324_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))

# ------ 24-25 season
fenwick2425_summary <- fenwick2425 %>%
  filter(!is.na(isHome),
         gameType == 2) %>%
  mutate(
    teamAbbrev = if_else(isHome, homeTeamAbbrev, awayTeamAbbrev))

team_fenwick2425 <- fenwick2425_summary %>%
  group_by(teamAbbrev, seasonId) %>%
  summarise(
    FF = sum(fenwickFor, na.rm = TRUE),
    FA = sum(fenwickAgainst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    FF_percent = FF/(FF + FA))

  ## ------------ final combined fenwick % ------------ 
fenwick <- rbind(team_fenwick1819,
                 team_fenwick1920,
                 team_fenwick2021,
                 team_fenwick2122,
                 team_fenwick2223,
                 team_fenwick2324,
                 team_fenwick2425)
fenwick$seasonId <- as.numeric(fenwick$seasonId)

# ------------- powerplay by team and season ----------------------------------

pp_stat <- rbind(
  team_season_report(season = 20182019, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")],
  team_season_report(season = 20192020, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")],
  team_season_report(season = 20202021, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")],
  team_season_report(season = 20212022, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")],
  team_season_report(season = 20222023, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")],
  team_season_report(season = 20232024, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")],
  team_season_report(season = 20242025, game_type = 2, category = "powerplay")[,c("seasonId", "teamFullName","powerPlayPct", "ppOpportunitiesPerGame", "shGoalsAgainstPerGame","ppNetGoalsPerGame")])

pp_stat$teamAbbrev <- all_teams$triCode[match(pp_stat$teamFullName, all_teams$fullName)]

# ------------- penaltyKill by team and season ----------------------------------

pk_stat <- rbind(
  subset(team_season_report(season = 20182019, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName)),
  subset(team_season_report(season = 20192020, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName)),
  subset(team_season_report(season = 20202021, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName)),
  subset(team_season_report(season = 20212022, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName)),
  subset(team_season_report(season = 20222023, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName)),
  subset(team_season_report(season = 20232024, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName)),
  subset(team_season_report(season = 20242025, game_type = 2, category = "penaltykill"), select = c(penaltyKillPct, timesShorthandedPerGame, seasonId, teamFullName))
  )
  
pk_stat$teamAbbrev <- all_teams$triCode[match(pk_stat$teamFullName, all_teams$fullName)]


#  ------------- USAT unblocked shot attempts (fenwick)  --------------------------
usat_count <- rbind(
  team_season_report(season = 20182019, game_type = 2, category = "summaryshooting"),
  team_season_report(season = 20192020, game_type = 2, category = "summaryshooting"),
  team_season_report(season = 20202021, game_type = 2, category = "summaryshooting"),
  team_season_report(season = 20212022, game_type = 2, category = "summaryshooting"),
  team_season_report(season = 20222023, game_type = 2, category = "summaryshooting"),
  team_season_report(season = 20232024, game_type = 2, category = "summaryshooting"),
  team_season_report(season = 20242025, game_type = 2, category = "summaryshooting"))

usat_pct <- rbind(
  team_season_report(season = 20182019, game_type = 2, category = "percentages"),
  team_season_report(season = 20192020, game_type = 2, category = "percentages"),
  team_season_report(season = 20202021, game_type = 2, category = "percentages"),
  team_season_report(season = 20212022, game_type = 2, category = "percentages"),
  team_season_report(season = 20222023, game_type = 2, category = "percentages"),
  team_season_report(season = 20232024, game_type = 2, category = "percentages"),
  team_season_report(season = 20242025, game_type = 2, category = "percentages"))


usat_count_small <- subset(usat_count, select = c(usatAgainst, usatFor, usatTotal, seasonId, teamFullName))
usat_pct_small <- subset(usat_pct, select = c(seasonId, teamFullName, usatPct))

usat <- full_join(usat_count_small, usat_pct_small, by = c("seasonId", "teamFullName"))
usat$teamAbbrev <- all_teams$triCode[match(usat$teamFullName, all_teams$fullName)]

# ------------- combine data ----------------------------------

# combine all dataframes to a list
data_merge <- list(season_points_summary, fenwick, save_pct_small, gfga_small, subset(pp_stat, select = -teamFullName), pk_stat, subset(usat, select = -teamFullName))

# merge all dataframes
full_df <- reduce(.x = data_merge, merge, by = c('teamAbbrev', 'seasonId'), all = T)

# reorganize columns
full_df <- full_df %>% relocate("teamFullName", .before = "seasonId") %>%
  relocate("teamAbbrev", .after = "teamFullName")

# clean up column formatting
names(full_df) <- snakecase::to_snake_case(names(full_df))

# scale all 
full_df_scaled <- full_df %>%
  mutate(
    across(
      where(is.numeric) & !c(team_abbrev, team_full_name, season_id, points),
      ~ as.numeric(scale(.x))
    )
  )
#  ------------- write to csv  ------------- -------------
write_csv(full_df, "NHL_18to25_stats.csv")
write_csv(full_df_scaled, "NHL_18to25_stats_scaled.csv")




