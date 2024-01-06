# Create data frame that gets player coordinates for team tackle modeling
create_coords <- function(tracking) {
  # Change coordinate location
  data <- tracking %>%
    mutate(x = ifelse(playDirection == "left", 120 - x, x),
           y = ifelse(playDirection == "left", (160/3) - y, y)) %>%
    filter(displayName != "football") %>%
    left_join(players, by = c("nflId", "displayName")) %>%
    left_join(plays, by = c("gameId", "playId")) %>%
    group_by(frameId) %>%
    left_join(tackles, by = c("nflId", "gameId", "playId")) %>%
    mutate(tackle_made = ifelse(event == "tackle" & tackle == 1, 1, 0),
           tackle_made = ifelse(is.na(tackle_made), 0, tackle_made)
    ) %>%
    ungroup()
  
  # Get football location data
  football <- tracking %>%
    filter(displayName == "football") %>%
    select(gameId, playId, frameId, football_x = x, football_y = y, football_s = s, 
           football_a = a, football_dis = dis)
  
  def_coords <- data %>%
    left_join(tackle_rates, by = c("nflId")) %>%
    filter(club == defensiveTeam) %>%
    select(gameId, playId, nflId, displayName, position, height, weight, frameId, 
           x, y, s, a, dis, o, dir, tackle_rate) %>%
    group_by(gameId, playId, frameId) %>%
    arrange(displayName) %>%
    mutate(player_number = row_number()) %>%
    separate(height, c("player_ft", "player_in"), sep = "-") %>%
    mutate(inch = as.numeric(player_ft) * 12 + as.numeric(player_in)) %>%
    pivot_wider(
      id_cols = c("gameId", "playId", "frameId"),
      names_from = "player_number",
      values_from = c("inch", "weight", "x", "y", "s", "a", "dis", "o", "dir", "tackle_rate"),
      names_sep = "_",
      names_glue = "def_{.name}"
    ) %>%
    ungroup() %>%
    rename(
      game_id = gameId,
      play_id = playId,
      frame_id = frameId
    )
  
  # Create a data frame with tracking data
  df <- tracking %>%
    # Join columns into one large data set
    inner_join(plays, by = c("gameId", "playId")) %>%
    inner_join(games, by = c("gameId")) %>%
    inner_join(football, by = c("gameId", "playId", "frameId")) %>%
    mutate(def_team_wp = ifelse(homeTeamAbbr == defensiveTeam, preSnapHomeTeamWinProbability,
                                preSnapVisitorTeamWinProbability)) %>%
    summarise(game_id = gameId, play_id = playId, frame_id = frameId, event, desc = playDescription, gameClock,
              qtr = quarter, down, ydstogo = yardsToGo,
              off_team = possessionTeam, def_team = defensiveTeam, def_team_wp, defendersInTheBox, passProbability,
              football_x, football_y, football_s, football_a, football_dis, absoluteYardlineNumber,
              pass = ifelse(passResult == "C", 1, 0),
              scramble = ifelse(passResult == "R", 1, 0),
              rush = ifelse(is.na(passResult), 1, 0),
              shotgun = ifelse(offenseFormation == "SHOTGUN", 1, 0),
              empty = ifelse(offenseFormation == "EMPTY", 1, 0),
              iform = ifelse(offenseFormation == "I_FORM", 1, 0),
              singleback = ifelse(offenseFormation == "SINGLEBACK", 1, 0),
              pistol = ifelse(offenseFormation == "PISTOL", 1, 0),
              jumbo = ifelse(offenseFormation == "JUMBO", 1, 0),
              wildcat = ifelse(offenseFormation == "WILDCAT", 1, 0)) %>%
    inner_join(def_coords, by = c("game_id", "play_id", "frame_id")) %>%
    mutate(
      team_tackle_made = ifelse(event == "tackle", 1, 0),
      team_tackle_made = ifelse(is.na(team_tackle_made), 0, team_tackle_made)
    ) %>%
    distinct(game_id, play_id, frame_id, .keep_all = TRUE) %>%
    # Remove duplicate game/play/frame observations with changing vars
    separate(gameClock, c("min", "sec"), ":") %>%
    mutate(qtr_secs = as.numeric(min) * 60 + as.numeric(sec)) %>%
    group_by(game_id, play_id) %>%
    mutate(play_duration = frame_id) %>%
    ungroup() %>%
    filter(!grepl("FUMBLES|FUMBLE|PENALTY|Penalty", desc)) # Remove penalties and fumbles 
}