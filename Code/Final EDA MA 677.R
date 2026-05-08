library(tidyverse)
library(scales)
library(readr)
library(janitor)

#Folders
model_data_dir <- "outputs/model_data"
summary_data_dir <- "outputs/summaries"

#CSV loader
load_csv_safe <- function(path) {
  if (!file.exists(path)) {
    stop(paste("Missing file:", path))
  }
  
  read_csv(path, show_col_types = FALSE) %>%
    clean_names()
}

#Load model data
player_season_model <- load_csv_safe(
  file.path(model_data_dir, "player_season_model_archetypes.csv")
)

#Optional summary files
season_thresholds <- load_csv_safe(
  file.path(model_data_dir, "season_thresholds.csv")
)

#Fix archetype column
if ("broad_archetype_tuned" %in% names(player_season_model)) {
  player_season_model <- player_season_model %>%
    mutate(broad_archetype_tuned = as.character(broad_archetype_tuned))
} else if ("broad_archetype" %in% names(player_season_model)) {
  player_season_model <- player_season_model %>%
    mutate(broad_archetype_tuned = as.character(broad_archetype))
} else {
  stop("No broad archetype column found. Expected broad_archetype or broad_archetype_tuned.")
}

#Archetype order
archetype_order <- c(
  "Complete",
  "Power-based",
  "Contact/discipline-based",
  "Middle-profile",
  "Low offensive profile"
)

#Create working dataset
player_season_survival_tuned <- player_season_model %>%
  mutate(
    age_int = floor(age),
    
    age_plot = case_when(
      age_int <= 22 ~ 22,
      age_int >= 40 ~ 40,
      TRUE ~ age_int
    ),
    
    age_group_label = case_when(
      age_int <= 22 ~ "22 and under",
      age_int >= 40 ~ "40+",
      TRUE ~ as.character(age_int)
    ),
    
    age_band = case_when(
      age <= 24 ~ "24 and under",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 ~ "35+",
      TRUE ~ NA_character_
    ),
    
    age_band = factor(
      age_band,
      levels = c("24 and under", "25-29", "30-34", "35+")
    ),
    
    broad_archetype_tuned = factor(
      broad_archetype_tuned,
      levels = archetype_order
    ),
    
    pa_bucket_eda = case_when(
      plate_appearances < 200 ~ "Under 200 PA",
      plate_appearances >= 200 & plate_appearances < 400 ~ "200-399 PA",
      plate_appearances >= 400 & plate_appearances < 600 ~ "400-599 PA",
      plate_appearances >= 600 ~ "600+ PA",
      TRUE ~ NA_character_
    ),
    
    pa_bucket_eda = factor(
      pa_bucket_eda,
      levels = c("Under 200 PA", "200-399 PA", "400-599 PA", "600+ PA")
    )
  )

#Quick checks
player_season_survival_tuned %>%
  count(broad_archetype_tuned, sort = TRUE)

player_season_survival_tuned %>%
  count(age_band, sort = TRUE)

player_season_survival_tuned %>%
  count(pa_bucket_eda, sort = TRUE)


#Basic summary
data_summary <- player_season_survival_tuned %>%
  summarise(
    player_seasons = n(),
    unique_players = n_distinct(batter),
    first_season = min(season, na.rm = TRUE),
    last_season = max(season, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    avg_chase_rate = mean(chase_rate, na.rm = TRUE),
    avg_whiff_rate = mean(whiff_rate, na.rm = TRUE),
    avg_zone_contact_rate = mean(zone_contact_rate, na.rm = TRUE)
  )

data_summary


#Season summary
data_by_season <- player_season_survival_tuned %>%
  group_by(season) %>%
  summarise(
    player_seasons = n(),
    unique_players = n_distinct(batter),
    avg_age = mean(age, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    .groups = "drop"
  )

#Player-seasons by season
ggplot(data_by_season, aes(x = season, y = player_seasons)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = sort(unique(data_by_season$season))) +
  labs(
    title = "Player-Seasons by MLB Season",
    subtitle = "Each observation is one hitter-season",
    x = "Season",
    y = "Player-seasons"
  ) +
  theme_minimal()

#Unique hitters by season
ggplot(data_by_season, aes(x = season, y = unique_players)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = sort(unique(data_by_season$season))) +
  labs(
    title = "Unique Hitters by Season",
    subtitle = "Number of distinct hitters observed in each season",
    x = "Season",
    y = "Unique hitters"
  ) +
  theme_minimal()


#Age summary
data_by_age <- player_season_survival_tuned %>%
  group_by(age_plot, age_group_label) %>%
  summarise(
    player_seasons = n(),
    unique_players = n_distinct(batter),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    avg_chase_rate = mean(chase_rate, na.rm = TRUE),
    avg_whiff_rate = mean(whiff_rate, na.rm = TRUE),
    avg_zone_contact_rate = mean(zone_contact_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(age_plot)

#Player-seasons by age
ggplot(data_by_age, aes(x = age_plot, y = player_seasons)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  labs(
    title = "Player-Seasons by Age",
    subtitle = "The observed sample is concentrated around typical prime-age seasons",
    x = "Age",
    y = "Player-seasons"
  ) +
  theme_minimal()

#Unique hitters by age
ggplot(data_by_age, aes(x = age_plot, y = unique_players)) +
  geom_col(alpha = 0.8) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  labs(
    title = "Unique Hitters by Age",
    subtitle = "Fewer hitters remain observable at older ages",
    x = "Age",
    y = "Unique hitters"
  ) +
  theme_minimal()


#PA distribution
ggplot(
  player_season_survival_tuned,
  aes(x = plate_appearances)
) +
  geom_histogram(bins = 40, alpha = 0.8) +
  labs(
    title = "Distribution of Plate Appearances",
    subtitle = "Plate appearances measure role size and opportunity",
    x = "Plate appearances",
    y = "Player-seasons"
  ) +
  theme_minimal()

#PA buckets
pa_distribution <- player_season_survival_tuned %>%
  count(pa_bucket_eda, name = "player_seasons") %>%
  mutate(
    share = player_seasons / sum(player_seasons, na.rm = TRUE)
  )

ggplot(
  pa_distribution,
  aes(x = pa_bucket_eda, y = share)
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(pa_distribution$share, na.rm = TRUE) + 0.05)
  ) +
  labs(
    title = "Distribution of Plate Appearance Buckets",
    subtitle = "Most hitter-seasons are not full-time everyday roles",
    x = "Plate appearance bucket",
    y = "Share of player-seasons"
  ) +
  theme_minimal()

#Average PA by age
ggplot(
  data_by_age,
  aes(x = age_plot, y = avg_pa)
) +
  geom_point(alpha = 0.8) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  labs(
    title = "Average Plate Appearances by Age",
    subtitle = "Playing time reflects both performance and continued opportunity",
    x = "Age",
    y = "Average plate appearances"
  ) +
  theme_minimal()

#Observed xwOBA by age
ggplot(
  data_by_age,
  aes(x = age_plot, y = avg_xwoba)
) +
  geom_point(alpha = 0.8) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  labs(
    title = "Observed Average xwOBA by Age",
    subtitle = "Observed performance only includes hitters still present in the data",
    x = "Age",
    y = "Average xwOBA"
  ) +
  theme_minimal()


#Metric distributions
core_metric_long <- player_season_survival_tuned %>%
  select(
    xwoba,
    ev_p90,
    chase_rate,
    whiff_rate,
    zone_contact_rate
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    metric = recode(
      metric,
      xwoba = "xwOBA",
      ev_p90 = "90th percentile exit velocity",
      chase_rate = "Chase rate",
      whiff_rate = "Whiff rate",
      zone_contact_rate = "Zone contact rate"
    )
  )

ggplot(
  core_metric_long,
  aes(x = value)
) +
  geom_histogram(bins = 35, alpha = 0.8) +
  facet_wrap(~ metric, scales = "free", ncol = 2) +
  labs(
    title = "Distributions of Core Hitter Metrics",
    subtitle = "Statcast and plate-discipline variables used to describe hitter skill",
    x = "Value",
    y = "Player-seasons"
  ) +
  theme_minimal()

#Metric averages by age
metric_by_age_long <- data_by_age %>%
  select(
    age_plot,
    avg_xwoba,
    avg_ev_p90,
    avg_chase_rate,
    avg_whiff_rate,
    avg_zone_contact_rate
  ) %>%
  pivot_longer(
    cols = -age_plot,
    names_to = "metric",
    values_to = "average_value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      avg_xwoba = "xwOBA",
      avg_ev_p90 = "90th percentile exit velocity",
      avg_chase_rate = "Chase rate",
      avg_whiff_rate = "Whiff rate",
      avg_zone_contact_rate = "Zone contact rate"
    )
  )

ggplot(
  metric_by_age_long,
  aes(x = age_plot, y = average_value)
) +
  geom_point(alpha = 0.8) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(22, 40, by = 3)) +
  labs(
    title = "Average Hitter Metrics by Age",
    subtitle = "Observed averages are conditional on players remaining in the dataset",
    x = "Age",
    y = "Average value"
  ) +
  theme_minimal()


#Archetype composition
archetype_composition <- player_season_survival_tuned %>%
  count(broad_archetype_tuned, name = "player_seasons") %>%
  mutate(
    share = player_seasons / sum(player_seasons, na.rm = TRUE)
  )

ggplot(
  archetype_composition,
  aes(
    x = reorder(broad_archetype_tuned, share),
    y = share
  )
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = percent(share, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(archetype_composition$share, na.rm = TRUE) + 0.05)
  ) +
  labs(
    title = "Distribution of Hitter Archetypes",
    subtitle = "Each player-season is assigned to one offensive profile",
    x = "Hitter archetype",
    y = "Share of player-seasons"
  ) +
  theme_minimal()

#Archetype counts
ggplot(
  archetype_composition,
  aes(
    x = reorder(broad_archetype_tuned, player_seasons),
    y = player_seasons
  )
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = comma(player_seasons)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    limits = c(0, max(archetype_composition$player_seasons, na.rm = TRUE) * 1.12)
  ) +
  labs(
    title = "Player-Seasons by Hitter Archetype",
    subtitle = "Raw count of player-seasons in each offensive profile",
    x = "Hitter archetype",
    y = "Player-seasons"
  ) +
  theme_minimal()


#Archetype by age band
archetype_by_age_band <- player_season_survival_tuned %>%
  count(age_band, broad_archetype_tuned, name = "player_seasons") %>%
  group_by(age_band) %>%
  mutate(
    share = player_seasons / sum(player_seasons, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(
  archetype_by_age_band,
  aes(
    x = age_band,
    y = share,
    fill = broad_archetype_tuned
  )
) +
  geom_col(position = "fill", alpha = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Hitter Archetype Composition by Age Band",
    subtitle = "Older age groups contain a more selected mix of hitter profiles",
    x = "Age band",
    y = "Share of player-seasons",
    fill = "Archetype"
  ) +
  theme_minimal()

#Archetype counts by age band
ggplot(
  archetype_by_age_band,
  aes(
    x = age_band,
    y = player_seasons,
    fill = broad_archetype_tuned
  )
) +
  geom_col(alpha = 0.9) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Hitter Archetype Counts by Age Band",
    subtitle = "Raw player-season counts by offensive profile and age group",
    x = "Age band",
    y = "Player-seasons",
    fill = "Archetype"
  ) +
  theme_minimal()


#Archetype by age
archetype_by_age <- player_season_survival_tuned %>%
  group_by(age_plot, broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    .groups = "drop"
  ) %>%
  group_by(age_plot) %>%
  mutate(
    share = player_seasons / sum(player_seasons, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(
  archetype_by_age,
  aes(
    x = age_plot,
    y = share,
    fill = broad_archetype_tuned
  )
) +
  geom_col(position = "fill", alpha = 0.9) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Hitter Archetype Composition by Age",
    subtitle = "Archetype shares vary across the observed aging distribution",
    x = "Age",
    y = "Share of player-seasons",
    fill = "Archetype"
  ) +
  theme_minimal()


#Archetype metric summary
archetype_metric_summary <- player_season_survival_tuned %>%
  group_by(broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    avg_age = mean(age, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    avg_chase_rate = mean(chase_rate, na.rm = TRUE),
    avg_whiff_rate = mean(whiff_rate, na.rm = TRUE),
    avg_zone_contact_rate = mean(zone_contact_rate, na.rm = TRUE),
    .groups = "drop"
  )

#xwOBA by archetype
ggplot(
  archetype_metric_summary,
  aes(
    x = reorder(broad_archetype_tuned, avg_xwoba),
    y = avg_xwoba
  )
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = round(avg_xwoba, 3)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, max(archetype_metric_summary$avg_xwoba, na.rm = TRUE) + 0.04)
  ) +
  labs(
    title = "Average xwOBA by Hitter Archetype",
    subtitle = "Archetypes separate hitters by expected offensive production",
    x = "Hitter archetype",
    y = "Average xwOBA"
  ) +
  theme_minimal()

#PA by archetype
ggplot(
  archetype_metric_summary,
  aes(
    x = reorder(broad_archetype_tuned, avg_pa),
    y = avg_pa
  )
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = round(avg_pa, 0)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = comma,
    limits = c(0, max(archetype_metric_summary$avg_pa, na.rm = TRUE) * 1.12)
  ) +
  labs(
    title = "Average Plate Appearances by Hitter Archetype",
    subtitle = "Playing time differs across offensive profiles",
    x = "Hitter archetype",
    y = "Average plate appearances"
  ) +
  theme_minimal()


#Metrics by archetype
archetype_metric_long <- archetype_metric_summary %>%
  select(
    broad_archetype_tuned,
    avg_xwoba,
    avg_ev_p90,
    avg_chase_rate,
    avg_whiff_rate,
    avg_zone_contact_rate
  ) %>%
  pivot_longer(
    cols = -broad_archetype_tuned,
    names_to = "metric",
    values_to = "average_value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      avg_xwoba = "xwOBA",
      avg_ev_p90 = "90th percentile exit velocity",
      avg_chase_rate = "Chase rate",
      avg_whiff_rate = "Whiff rate",
      avg_zone_contact_rate = "Zone contact rate"
    )
  )

ggplot(
  archetype_metric_long,
  aes(
    x = broad_archetype_tuned,
    y = average_value
  )
) +
  geom_col(alpha = 0.8) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Average Hitter Metrics by Archetype",
    subtitle = "Each archetype captures a different offensive skill profile",
    x = "Hitter archetype",
    y = "Average value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


#Age by archetype
ggplot(
  player_season_survival_tuned,
  aes(
    x = age,
    fill = broad_archetype_tuned
  )
) +
  geom_histogram(bins = 25, alpha = 0.8) +
  facet_wrap(~ broad_archetype_tuned, scales = "free_y") +
  labs(
    title = "Age Distribution by Hitter Archetype",
    subtitle = "The age mix differs across offensive profiles",
    x = "Age",
    y = "Player-seasons",
    fill = "Archetype"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


#Median table
median_summary <- player_season_survival_tuned %>%
  summarise(
    n_player_seasons = n(),
    n_players = n_distinct(batter),
    median_pa = median(plate_appearances, na.rm = TRUE),
    q1_pa = quantile(plate_appearances, 0.25, na.rm = TRUE),
    q3_pa = quantile(plate_appearances, 0.75, na.rm = TRUE),
    median_xwoba = median(xwoba, na.rm = TRUE),
    q1_xwoba = quantile(xwoba, 0.25, na.rm = TRUE),
    q3_xwoba = quantile(xwoba, 0.75, na.rm = TRUE),
    median_ev_p90 = median(ev_p90, na.rm = TRUE),
    median_chase = median(chase_rate, na.rm = TRUE),
    median_whiff = median(whiff_rate, na.rm = TRUE),
    median_zone_contact = median(zone_contact_rate, na.rm = TRUE)
  )

median_summary


#Save EDA outputs
dir.create("outputs/eda_tables", recursive = TRUE, showWarnings = FALSE)

write_csv(data_summary, "outputs/eda_tables/data_summary.csv")
write_csv(data_by_season, "outputs/eda_tables/data_by_season.csv")
write_csv(data_by_age, "outputs/eda_tables/data_by_age.csv")
write_csv(pa_distribution, "outputs/eda_tables/pa_distribution.csv")
write_csv(archetype_composition, "outputs/eda_tables/archetype_composition.csv")
write_csv(archetype_by_age_band, "outputs/eda_tables/archetype_by_age_band.csv")
write_csv(archetype_by_age, "outputs/eda_tables/archetype_by_age.csv")
write_csv(archetype_metric_summary, "outputs/eda_tables/archetype_metric_summary.csv")
write_csv(median_summary, "outputs/eda_tables/median_summary.csv")
