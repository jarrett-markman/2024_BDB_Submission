# Create model data frames
wk1 <- create_data(tracking_wk1)
wk2 <- create_data(tracking_wk2)
wk3 <- create_data(tracking_wk3)
wk4 <- create_data(tracking_wk4)
wk5 <- create_data(tracking_wk5)
wk6 <- create_data(tracking_wk6)
wk7 <- create_data(tracking_wk7)
wk8 <- create_data(tracking_wk8)
wk9 <- create_data(tracking_wk9)
# Find optimal number of tackle attempts to include in the model
find_attempts <- function(num) {
  train_data <- bind_rows(wk1, wk2, wk3)
  att <- train_data %>%
    filter(tackle_attempts > num)
  train_ind <- sample(1:nrow(att), .6 * nrow(att))
  # Create separate train/test data sets
  tackle_train <- att %>%
    dplyr::slice(train_ind) %>%
    select(-c(game_id, play_id, frame_id, playDescription, player_id, player, club,
              off_player_id, off_player, off_team, def_team, tackle_made)) %>%
    as.matrix() %>%
    xgb.DMatrix(label = att$tackle_made[train_ind])
  # Create separate test sets for 3 models
  tackle_test <- att %>%
    dplyr::slice(-train_ind) %>%
    select(-c(game_id, play_id, frame_id, playDescription, player_id, player, club,
              off_player_id, off_player, off_team, def_team, tackle_made, tackle_attempts)) %>%
    as.matrix() %>%
    xgb.DMatrix(label = att$tackle_made[-train_ind])
  xgb.train(
    params = list(
      # Given the size of the data set:
      max_depth = 20, # Set the depth to 20 to account for various split depths
      min_child_weight = 50, # Set the min_child_weight at 50 to account for the large sample size
      colsample_bytree = .75, # Set colsample_by tree to .75 to prevent the model from overfitting columns
      # Set lambda = 3 to make the model more conservative
      # Found lambda for 2, 3, and 4 - 3 had lowest test-logloss
      lambda = 3,
      objective = "binary:logistic", # Binary logistic regression model
      eval_metric = "logloss"
      # Set other parameters to defaults
    ),
    data = tackle_train,
    nrounds = 200, # Set the number of rounds to 200
    watchlist = list( # Measure the model via train and test error
      train = tackle_train,
      test = tackle_test
    )
  )$evaluation_log %>%
    select(test_logloss) %>%
    slice_tail() %>%
    unlist()
}
optim_val <- optimize(find_attempts, c(10, 25))$minimum
min_tackle_attempts <- floor(optim_val) # Find the lower bound (floor) of the optimized value
# Create train/test matrices based on a random sample of observations in the first 3 weeks
train_data <- bind_rows(wk1, wk2, wk3) %>%
  filter(tackle_attempts > min_tackle_attempts)
train_ind <- sample(1:nrow(train_data), .6 * nrow(train_data))
# Create separate train/test data sets
tackle_train <- train_data %>%
  dplyr::slice(train_ind) %>%
  select(-c(game_id, play_id, frame_id, playDescription, player_id, player, club, 
            off_player_id, off_player, off_team, def_team, tackle_made, tackle_attempts)) %>%
  as.matrix() %>%
  xgb.DMatrix(label = train_data$tackle_made[train_ind])
# Create separate test sets for 3 models
tackle_test <- train_data %>%
  dplyr::slice(-train_ind) %>%
  select(-c(game_id, play_id, frame_id, playDescription, player_id, player, club, 
            off_player_id, off_player, off_team, def_team, tackle_made, tackle_attempts)) %>%
  as.matrix() %>%
  xgb.DMatrix(label = train_data$tackle_made[-train_ind])
# Create a test data frame with weeks 4-9
test_data <- bind_rows(wk4, wk5, wk6, wk7, wk8, wk9) %>%
  filter(tackle_attempts > min_tackle_attempts)
tackle_pred_matrix <- test_data %>%
  select(-c(game_id, play_id, frame_id, playDescription, player_id, player, club,
            off_player_id, off_player, off_team, def_team, tackle_made, tackle_attempts)) %>%
  as.matrix() %>%
  xgb.DMatrix(label = test_data$tackle_made)
# Create tackle xgb model
tackle_model <- xgb.train(
  params = list(
    # Given the size of the data set:
    max_depth = 20, # Set the depth to 20 to account for various split depths
    min_child_weight = 50, # Set the min_child_weight at 50 to account for the large sample size
    colsample_bytree = .75, # Set colsample_by tree to .75 to prevent the model from overfitting columns
    # Set lambda = 3 to make the model more conservative
    lambda = 3,
    objective = "binary:logistic", # Binary logistic regression model
    eval_metric = "logloss"
    # Set other parameters to defaults
  ),
  data = tackle_train,
  nrounds = 200, # Set the number of rounds to 200
  watchlist = list( # Measure the model via train and test error
    train = tackle_train,
    test = tackle_test
  )
)
# Make predictions and combine into the data frame
tackle_preds <- predict(tackle_model, tackle_pred_matrix)
preds_df <- bind_cols(test_data, tackle_preds) %>% rename(tackle_prob = ...51)