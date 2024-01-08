# Tackling the Numbers: Evaluating Player Tackle Probabilities

In order to store both the individual and tackle model in R, download the files (within models folder):
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
