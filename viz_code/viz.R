# VIZ 1
# Variable importance in the individual tackle model
var_imp <- vip(tackle_model, num_features = 20)$data
var_imp_plot <- var_imp %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_fivethirtyeight() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(x = "",
       title = "Variable Importance Plot",
       subtitle = "20 Most Important Model Variables") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        legend.position = "none")
ggsave("Individual Model Variable Importance.png", var_imp_plot)

# Get player headshots
headshots <- nflreadr::load_rosters(2022) %>%
  summarise(gsis_it_id = as.double(gsis_it_id), headshot_url, team)
teams <- nflreadr::load_teams()
# Get defensive player tackle nums
player_plot_df <- preds_df %>%
  group_by(player, player_id) %>%
  summarise(
    total_tackles = sum(tackle_made),
    total_x_tackles = sum(tackle_prob),
    tackles_over_x = total_tackles - total_x_tackles
  ) %>%
  ungroup() %>%
  left_join(headshots, by = c("player_id" = "gsis_it_id")) %>%
  left_join(teams, by = c("team" = "team_abbr")) 



# VIZ 2
player_table_top_x <- player_plot_df %>%
  arrange(-total_x_tackles) %>%
  mutate(rank = row_number()) %>%
  head(20) %>%
  arrange(-tackles_over_x) %>%
  select(rank, player, headshot_url, total_tackles, total_x_tackles, tackles_over_x) %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  data_color(columns = c(tackles_over_x), colors = scales::col_numeric(c("red3", "green3"), 
                                                                       domain = NULL)) %>%
  cols_label(
    rank = "Rank",
    player = "Player",
    headshot_url = "",
    total_tackles = "Tackles",
    total_x_tackles = "Expected Tackles",
    tackles_over_x = "Tackles Made Over Expected"
  ) %>%
  gt_img_rows(headshot_url) %>%
  gt_theme_538() %>%
  tab_header(title = "Defensive Tackles and Expected Tackles Leaders",
             subtitle = "Weeks 4-9 of 2022") %>%
  tab_source_note(
    source_note = "Jarrett Markman & Isabel Alfonso | NFL NFL Big Data Bowl 2024"
  )
gtsave(player_table_top_x, "Defensive Tackles and Expected Tackles Leaders (sorted).png")


# VIZ 3
# Plot w/ tackles and x tackles 
def_plot <- player_plot_df %>%
  ggplot(aes(x = total_x_tackles, y = total_tackles, color = team_color, label = player)) +
  geom_point() +
  geom_hline(yintercept = mean(player_plot_df$total_tackles), linetype = "dashed") +
  geom_vline(xintercept = mean(player_plot_df$total_x_tackles), linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, aes(color = NULL), color = "black") +
  ggrepel::geom_text_repel(
    box.padding = .25, 
    point.padding = .75,
    segment.color = "white",
    size = 2
  ) +
  scale_color_identity() +
  theme_bw() +
  labs(
    x = "Expected Tackles",
    y = "Tackles",
    title = "Defensive Player Tackle Security",
    subtitle = "Through Weeks 4-9 of the 2022 NFL Season",
    caption = "Jarrett Markman & Isabel Alfonso | NFL Big Data Bowl 2024"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5)
  )
ggsave("Defensive Players Tackle and Expected Tackle Plot.png", def_plot)


# VIZ 4
# Get best defensive players
def_toe_leaders <- player_plot_df %>%
  arrange(-tackles_over_x) %>%
  mutate(rank = row_number()) %>%
  head(20) %>%
  select(rank, player, headshot_url, total_tackles, total_x_tackles, tackles_over_x) %>%
  gt() %>%
  cols_align(align = "center", columns = everything()) %>%
  data_color(columns = c(tackles_over_x), colors = scales::col_numeric(c("red3", "green3"), 
                                                                       domain = NULL)) %>%
  cols_label(
    rank = "Rank",
    player = "Player",
    headshot_url = "",
    total_tackles = "Tackles",
    total_x_tackles = "Expected Tackles",
    tackles_over_x = "Tackles Over Expected"
  ) %>%
  gt_img_rows(headshot_url) %>%
  gt_theme_538() %>%
  tab_header(title = "Best Defensive Tacklers",
             subtitle = "Weeks 4-9 of 2022") %>%
  tab_source_note(
    source_note = "Jarrett Markman & Isabel Alfonso | NFL NFL Big Data Bowl 2024"
  )
gtsave(def_toe_leaders, "Defensive Tackles Over Expected Leaders.png")


# Get offensive player tackle nums
off_plot_df <- preds_df %>%
  group_by(off_player, off_player_id) %>%
  summarise(
    total_tackled = sum(tackle_made),
    total_x_tackled = sum(tackle_prob),
    tackled_under_x = total_x_tackled - total_tackled
  ) %>%
  ungroup() %>%
  filter(total_tackled > 20) %>%
  left_join(headshots, by = c("off_player_id" = "gsis_it_id")) %>%
  left_join(teams, by = c("team" = "team_abbr")) 


# VIZ 5
# Plot off nums
off_plot <- off_plot_df %>%
  ggplot(aes(x = total_tackled, y = total_x_tackled, color = team_color, label = off_player)) +
  geom_point() +
  geom_hline(yintercept = mean(off_plot_df$total_x_tackled), linetype = "dashed") +
  geom_vline(xintercept = mean(off_plot_df$total_tackled), linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, aes(color = NULL), color = "black") +
  ggrepel::geom_text_repel(
    box.padding = .25, 
    point.padding = .75,
    segment.color = "white",
    size = 2
  ) +
  lims(x = c(0, 150), y = c(0, 150)) +
  scale_color_identity() +
  labs(
    x = "Tackled",
    y = "Expected Tackled",
    title = "Offensive Player Tackle Avoidance",
    subtitle = "Through Weeks 4-9 of the 2022 NFL Season",
    caption = "Jarrett Markman & Isabel Alfonso | NFL Big Data Bowl 2024"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5)
  )
ggsave("Offensive Players Tackled Plot.png", off_plot)


# VIZ 6
# Create a df that calculates individual total tackles avoided
off_taoe_leaders <- off_plot_df %>%
  arrange(-tackled_under_x) %>%
  ungroup() %>%
  mutate(rank = row_number()) %>%
  head(20) %>%
  select(rank, off_player, headshot_url, total_tackled, total_x_tackled, tackled_under_x) %>%
  gt() %>%
  cols_align(align = "center") %>%
  data_color(columns = vars(tackled_under_x), colors = scales::col_numeric(c("red3", "green3"), 
                                                                           domain = NULL)) %>%
  cols_label(
    rank = "Rank",
    off_player = "Player",
    headshot_url = "",
    total_tackled = "Tackled",
    total_x_tackled = "Expected Tackled",
    tackled_under_x = "Tackles Avoided Over Expected"
  ) %>%
  gt_img_rows(headshot_url) %>%
  gt_theme_538() %>%
  tab_header(title = "Best Offensive Players at Avoiding Tackles",
             subtitle = "Weeks 4-9 of 2022")
gtsave(off_taoe_leaders, "Offensive Players Tackled Table.png")

# Get team offensive/defensive probs
off_team_probs <- team_preds_df %>%
  group_by(off_team) %>%
  summarise(
    tackled = sum(team_tackle_made),
    x_tackled = sum(tackle_prob),
    tackled_under_x = x_tackled - tackled
  )
def_team_probs <- team_preds_df %>%
  group_by(def_team) %>%
  summarise(
    tackles = sum(team_tackle_made),
    x_tackles = sum(tackle_prob),
    tackles_over_x = tackles - x_tackles
  )
team_probs <- inner_join(off_team_probs, def_team_probs, by = c("off_team" = "def_team")) %>% 
  rename(team = off_team)

# VIZ 7
team_plot <- team_probs %>% 
  ggplot(aes(x = tackles_over_x, y = tackled_under_x)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .05) +
  geom_hline(yintercept = mean(team_probs$tackled_under_x), linetype = "dashed") +
  geom_vline(xintercept = mean(team_probs$tackles_over_x), linetype = "dashed") +
  theme_bw() +
  labs(
    x = "Defensive Tackles Made Over Expected",
    y = "Offensive Tackles Avoided Over Expected",
    title = "Team Offensive and Defensive Tackle Effectiveness",
    subtitle = "Weeks 4-9 of 2022",
    caption = "Jarrett Markman & Isabel Alfonso | NFL Big Data Bowl 2024"
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5))
ggsave("Team Tackles Plot.png", team_plot)
# Viz 8
dist <- preds_df %>%
  ggplot(aes(x = tackle_prob)) +
  geom_density() +
  labs(
    x = "Tackle Probability",
    y = "Kernel Density",
    title = "Tackle Probability Distribution",
    caption = "Jarrett Markman & Isabel Alfonso | 2024 NFL Big Data Bowl"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggsave("Tackle Density Plot.png", dist)
# Extra viz
team_dist <- team_preds_df %>%
  ggplot(aes(x = tackle_prob)) +
  geom_density() +
  labs(
    x = "Team Tackle Probability",
    y = "Kernel Density",
    title = "Team Tackle Probability Distribution",
    caption = "Jarrett Markman & Isabel Alfonso | 2024 NFL Big Data Bowl"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggsave("Team Tackle Density Plot.png", team_dist)
