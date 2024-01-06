# Create a data frame that calculates "tackle accuracy" - rate of tackles made given tackles and misses
tackle_rates <- tackles %>%
  group_by(nflId) %>% # Group by nflId (player)
  summarise(
    # Calculate the sum of tackles and assists
    tackles = sum(tackle), # Solo tackles
    assists = sum(assist), # Assisted tackles/tackle assists
    total_tackles = tackles + assists,
    tackles_missed = sum(pff_missedTackle),
    tackle_attempts = total_tackles + tackles_missed,
    tackle_rate = total_tackles/tackle_attempts # Calculate tackle rate
  ) %>%
  select(nflId, tackle_rate, tackle_attempts) %>% # Select only these two columns
  ungroup() # Remove group constraints

create_data <- function(tracking) {
  # Create a data frame with just football location
  football <- tracking %>%
    filter(displayName == "football") %>%
    select(gameId, playId, frameId, x, y, s, a, dis)
  
  # Create a data frame with tracking data
  df <- tracking %>%
    mutate(x = ifelse(playDirection == "left", 120 - x, x),
           y = ifelse(playDirection == "left", (160/3) - y, y)) %>%
    group_by(frameId) %>%
    left_join(tackles, by = c("gameId", "playId", "nflId")) %>%
    mutate(player_tackle_indicator = ifelse(tackle == 1 | assist == 1, 1, 0)) %>%
    # Create an indicator variable to detect the tackler
    # Join columns into one large data set
    inner_join(plays, by = c("gameId", "playId")) %>%
    inner_join(games, by = c("gameId")) %>%
    left_join(football, by = c("gameId", "playId", "frameId")) %>%
    ungroup() %>%
    rename( # Change player coordinate names
      player_x = x.x,
      player_y = y.x,
      player_s = s.x,
      player_a = a.x,
      player_dis = dis.x,
      player_o = o, 
      player_dir = dir
    ) %>%
    rename( # Change football coordinate names
      football_x = x.y,
      football_y = y.y,
      football_s = s.y,
      football_a = a.y,
      football_dis = dis.y
    ) %>%
    # Create defensive win probability column
    mutate(def_team_wp = ifelse(homeTeamAbbr == defensiveTeam, preSnapHomeTeamWinProbability,
                                preSnapVisitorTeamWinProbability)) %>%
    filter(club == defensiveTeam) %>% # Filter only defensive players
    group_by(gameId, playId, frameId) %>% # Group by id's
    # Join data frames
    left_join(players, by = c("nflId", "displayName")) %>%
    left_join(players, by = c("ballCarrierId" = "nflId", "ballCarrierDisplayName" = "displayName")) %>%
    ungroup() %>%
    left_join(tackle_rates, by = c("nflId")) %>%
    summarise( # Create a data set with selected variables
      event, player_tackle_indicator, 
      game_id = gameId, play_id = playId, frame_id = frameId, playDescription,
      week, quarter, down, ydstogo = yardsToGo,
      player_id = nflId, player = displayName, club, 
      player_x, player_y, player_s, player_a, player_dis, player_o, player_dir, tackle_rate, tackle_attempts,
      player_ht = height.x, player_wt = weight.x,
      football_x, football_y, football_s, football_a, football_dis, 
      off_player_id = ballCarrierId, off_player = ballCarrierDisplayName, off_player_ht = height.y,
      off_player_wt = weight.y,
      off_team = possessionTeam, def_team = defensiveTeam, def_team_wp,
      absoluteYardlineNumber, gameClock = as.character(gameClock), 
      pass = ifelse(passResult == "C", 1, 0),
      scramble = ifelse(passResult == "R", 1, 0),
      rush = ifelse(is.na(passResult), 1, 0), 
      shotgun = ifelse(offenseFormation == "SHOTGUN", 1, 0),
      empty = ifelse(offenseFormation == "EMPTY", 1, 0),
      iform = ifelse(offenseFormation == "I_FORM", 1, 0),
      singleback = ifelse(offenseFormation == "SINGLEBACK", 1, 0),
      pistol = ifelse(offenseFormation == "PISTOL", 1, 0),
      jumbo = ifelse(offenseFormation == "JUMBO", 1, 0),
      wildcat = ifelse(offenseFormation == "WILDCAT", 1, 0),
      defendersInTheBox, passProbability
    ) %>%
    # Separate height (into inches) and clock variables (into seconds)
    separate(player_ht, c("player_ft", "player_in"), sep = "-") %>%
    separate(off_player_ht, c("off_player_ft", "off_player_in"), sep = "-") %>%
    separate(gameClock, c("min", "sec"), ":") %>%
    mutate(
      player_in = as.numeric(player_ft) * 12 + as.numeric(player_in),   
      off_player_in = as.numeric(off_player_ft) * 12 + as.numeric(off_player_in),
      player_wt = as.numeric(player_wt), off_player_wt = as.numeric(off_player_wt),
      qtr_secs = as.numeric(min) * 60 + as.numeric(sec),
      tackle_made = ifelse(event == "tackle" & player_tackle_indicator == 1, 1, 0), 
      tackle_made = ifelse(is.na(tackle_made), 0, tackle_made)
    ) %>% 
    select(-c(event, player_tackle_indicator, player_ft, off_player_ft, min, sec)) %>%
    group_by(game_id, play_id) %>%
    mutate(play_duration = frame_id) %>%
    ungroup() %>%
    filter(!grepl("FUMBLES|FUMBLE|PENALTY|Penalty", playDescription)) # Filter out fumbles and penalties
}