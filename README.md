# Tackling the Numbers: Evaluating Player Tackle Probabilities

## Check out our published notebook:
- [Tackling the Numbers](https://www.kaggle.com/code/jarrettmarkman/tackling-the-numbers?scriptVersionId=158222662)

## How to replicate our work:

- Download the datasets from [kaggle](https://www.kaggle.com/competitions/nfl-big-data-bowl-2024)
- Use the following libraries
```{r}
library(tidyverse)
library(xgboost)
library(gganimate)
library(ggridges)
library(beeswarm)
library(vip)
library(gt)
library(gtExtras)
library(ggthemes)
```
- Read in the data
```{r}
games <- read_csv("games.csv")
players <- read_csv("players.csv")
plays <- read_csv("plays.csv")
tackles <- read_csv("tackles.csv")
tracking_wk1 <- read_csv("tracking_week_1.csv")
tracking_wk2 <- read_csv("tracking_week_2.csv")
tracking_wk3 <- read_csv("tracking_week_3.csv")
tracking_wk4 <- read_csv("tracking_week_4.csv")
tracking_wk5 <- read_csv("tracking_week_5.csv")
tracking_wk6 <- read_csv("tracking_week_6.csv")
tracking_wk7 <- read_csv("tracking_week_7.csv")
tracking_wk8 <- read_csv("tracking_week_8.csv")
tracking_wk9 <- read_csv("tracking_week_9.csv")
```
- Set the seed
```
set.seed(1234)
```
- Run the files for create_ind_data and create_team_data
- Run the files for create_ind_xg and create_team_xg
- To create the visuals, run the files within "viz_code"

## How to use our model:

Download the files within models folder:
- individual_tackle_model.rds
- team_tackle_model.rds
- Run the following code (assuming by default the files are stored to default wd in R)

```R
individual_model <- readRDS("individual_tackle_model.rds")
team_model <- readRDS("team_tackle_model.rds")
```

## References

- **NFL Big Data Bowl 2024**
  - *Authors:* Michael Lopez, Thompson Bliss, Ally Blake, Andrew Patton, Jonathan McWilliams, Addison Howard, Will Cukierski
  - *Year:* 2023
  - *Publisher:* Kaggle
  - *URL:* [https://kaggle.com/competitions/nfl-big-data-bowl-2024](https://kaggle.com/competitions/nfl-big-data-bowl-2024)
