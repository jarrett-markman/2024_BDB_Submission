# Tackling the Numbers: Evaluating Player Tackle Probabilities

## Check out our published project:
- [Tackling the Numbers](URL)

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
