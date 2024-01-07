# Create model data frames
coords_wk1 <- create_coords(tracking_wk1)
coords_wk2 <- create_coords(tracking_wk2)
coords_wk3 <- create_coords(tracking_wk3)
coords_wk4 <- create_coords(tracking_wk4)
coords_wk5 <- create_coords(tracking_wk5)
coords_wk6 <- create_coords(tracking_wk6)
coords_wk7 <- create_coords(tracking_wk7)
coords_wk8 <- create_coords(tracking_wk8)
coords_wk9 <- create_coords(tracking_wk9)
# Create train/test data sets based on season weeks
train_data_team <- bind_rows(coords_wk1, coords_wk2, coords_wk3)
train_ind_team <- sample(1:nrow(train_data_team), .6 * nrow(train_data_team))
coords_train <- train_data_team %>%
  dplyr::slice(train_ind_team) %>%
  select(-c(game_id, play_id, frame_id, event, desc, min, sec, off_team, def_team, team_tackle_made)) %>% 
  as.matrix() %>%
  xgb.DMatrix(label = train_data_team$team_tackle_made[train_ind_team])
coords_test <- train_data_team %>% 
  dplyr::slice(-train_ind_team) %>%
  select(-c(game_id, play_id, frame_id, event, desc, min, sec, off_team, def_team, team_tackle_made)) %>% 
  as.matrix() %>%
  xgb.DMatrix(label = train_data_team$team_tackle_made[-train_ind_team])
test_data_team <- bind_rows(coords_wk4, coords_wk5, coords_wk6, coords_wk7, coords_wk8, coords_wk9)
test_data_team_matrix <- test_data_team %>%
  select(-c(game_id, play_id, frame_id, event, desc, min, sec, off_team, def_team, team_tackle_made)) %>%
  as.matrix() %>%
  xgb.DMatrix(label = test_data_team$team_tackle_made)
# Create tackle xgb model
team_tackle_model <- xgb.train(
  params = list(
    # Given the size of the data set:
    eta = .1, # Set a low shrinkage parameter because the data set is so large
    max_depth = 400, # Set the depth to 400 to account for various split depths
    min_child_weight = 100, # Set the min_child_weight at 100 to account for all the columns
    objective = "binary:logistic", # Binary logistic regression model
    eval_metric = "logloss"
    # Set other parameters to defaults
  ),
  data = coords_train,
  nrounds = 200, # Set the number of rounds to 200
  watchlist = list( # Measure the model via train and test error
    train = coords_train,
    test = coords_test
  )
)
# Make predictions and combine into the data frame
team_tackle_preds <- predict(team_tackle_model, test_data_team_matrix)
team_preds_df <- bind_cols(test_data_team, team_tackle_preds) %>% rename(tackle_prob = ...145)
saveRDS(team_tackle_model, file = "team_tackle_model.rds")
