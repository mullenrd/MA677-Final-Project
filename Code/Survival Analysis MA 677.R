library(tidyverse)
library(broom)
library(nnet)
library(scales)
library(mice)
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

#Load main scrape outputs
player_season_model <- load_csv_safe(
  file.path(model_data_dir, "player_season_model_archetypes.csv")
)

season_thresholds <- load_csv_safe(
  file.path(model_data_dir, "season_thresholds.csv")
)

#Load supporting files if needed
player_info_clean <- load_csv_safe(
  file.path(model_data_dir, "player_info_clean.csv")
)

player_info_by_season <- load_csv_safe(
  file.path(model_data_dir, "player_info_by_season.csv")
)

oaa_by_season <- load_csv_safe(
  file.path(model_data_dir, "oaa_by_season.csv")
)

sprint_by_season <- load_csv_safe(
  file.path(model_data_dir, "sprint_by_season.csv")
)

#Fix archetype column name
if ("broad_archetype_tuned" %in% names(player_season_model)) {
  player_season_model <- player_season_model %>%
    mutate(broad_archetype_tuned = as.character(broad_archetype_tuned))
} else if ("broad_archetype" %in% names(player_season_model)) {
  player_season_model <- player_season_model %>%
    mutate(broad_archetype_tuned = as.character(broad_archetype))
} else {
  stop("No broad archetype column found. Expected broad_archetype or broad_archetype_tuned.")
}

#Create working dataset
player_season_survival_tuned <- player_season_model %>%
  mutate(
    pa_bucket = case_when(
      plate_appearances >= 50 & plate_appearances < 200 ~ "50-199 PA",
      plate_appearances >= 200 & plate_appearances < 400 ~ "200-399 PA",
      plate_appearances >= 400 & plate_appearances < 600 ~ "400-599 PA",
      plate_appearances >= 600 ~ "600+ PA",
      TRUE ~ NA_character_
    ),
    
    pa_bucket = factor(
      pa_bucket,
      levels = c("50-199 PA", "200-399 PA", "400-599 PA", "600+ PA")
    )
  ) %>%
  arrange(batter, season) %>%
  group_by(batter) %>%
  mutate(
    next_observed_season_raw = lead(season),
    next_archetype_raw = lead(broad_archetype_tuned),
    next_pa_raw = lead(plate_appearances),
    
    survived_next_year = if_else(
      !is.na(next_observed_season_raw) &
        next_observed_season_raw == season + 1,
      1L,
      0L
    ),
    
    next_state_tuned = case_when(
      is.na(next_observed_season_raw) ~ "Exited MLB",
      
      next_observed_season_raw != season + 1 ~ "Exited MLB",
      
      survived_next_year == 1 &
        !is.na(next_pa_raw) &
        next_pa_raw < 200 ~ "Reduced/limited role",
      
      survived_next_year == 1 &
        !is.na(next_archetype_raw) ~ next_archetype_raw,
      
      TRUE ~ "Exited MLB"
    )
  ) %>%
  ungroup()

#Quick checks
player_season_survival_tuned %>%
  count(broad_archetype_tuned, sort = TRUE)

player_season_survival_tuned %>%
  count(pa_bucket, sort = TRUE)

player_season_survival_tuned %>%
  count(next_state_tuned, sort = TRUE)

#Helper functions
weighted_mean_safe <- function(x, w) {
  if (all(is.na(x)) || all(is.na(w)) || sum(w, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(x, w, na.rm = TRUE)
}

age_min <- 21
age_max <- 39
pa_threshold <- 100

#Missing-season panel
observed_player_ages <- player_season_survival_tuned %>%
  mutate(
    age_int = floor(age)
  ) %>%
  filter(
    age_int >= age_min,
    age_int <= age_max
  ) %>%
  group_by(batter, age_int) %>%
  summarise(
    first_season = min(season, na.rm = TRUE),
    xwoba = weighted_mean_safe(xwoba, plate_appearances),
    plate_appearances = sum(plate_appearances, na.rm = TRUE),
    ev_p90 = weighted_mean_safe(ev_p90, plate_appearances),
    chase_rate = weighted_mean_safe(chase_rate, plate_appearances),
    whiff_rate = weighted_mean_safe(whiff_rate, plate_appearances),
    zone_contact_rate = weighted_mean_safe(zone_contact_rate, plate_appearances),
    .groups = "drop"
  ) %>%
  mutate(
    observed_season = if_else(
      !is.na(xwoba) & plate_appearances >= pa_threshold,
      1,
      0
    )
  )

player_age_panel <- observed_player_ages %>%
  distinct(batter) %>%
  crossing(age_int = age_min:age_max) %>%
  left_join(
    observed_player_ages,
    by = c("batter", "age_int")
  ) %>%
  group_by(batter) %>%
  mutate(
    ever_observed = as.integer(any(!is.na(xwoba))),
    observed_season = replace_na(observed_season, 0),
    missing_player_season = if_else(is.na(xwoba), 1, 0)
  ) %>%
  ungroup() %>%
  filter(ever_observed == 1)

#Observed aging curve
observed_aging_curve <- player_age_panel %>%
  filter(
    observed_season == 1,
    !is.na(xwoba)
  ) %>%
  group_by(age_int) %>%
  summarise(
    observed_players = n_distinct(batter),
    observed_mean_xwoba = mean(xwoba, na.rm = TRUE),
    .groups = "drop"
  )

observed_aging_curve

#Player baseline values
player_baselines <- observed_player_ages %>%
  group_by(batter) %>%
  summarise(
    career_mean_xwoba = mean(xwoba, na.rm = TRUE),
    career_mean_pa = mean(plate_appearances, na.rm = TRUE),
    career_mean_ev_p90 = mean(ev_p90, na.rm = TRUE),
    career_mean_chase = mean(chase_rate, na.rm = TRUE),
    career_mean_whiff = mean(whiff_rate, na.rm = TRUE),
    career_mean_zone_contact = mean(zone_contact_rate, na.rm = TRUE),
    first_observed_age = min(age_int, na.rm = TRUE),
    last_observed_age = max(age_int, na.rm = TRUE),
    .groups = "drop"
  )

imputation_data <- player_age_panel %>%
  left_join(player_baselines, by = "batter") %>%
  mutate(
    age_centered_imp = age_int - mean(age_int, na.rm = TRUE),
    age_centered_sq_imp = age_centered_imp^2
  ) %>%
  select(
    batter,
    age_int,
    age_centered_imp,
    age_centered_sq_imp,
    xwoba,
    observed_season,
    missing_player_season,
    career_mean_xwoba,
    career_mean_pa,
    career_mean_ev_p90,
    career_mean_chase,
    career_mean_whiff,
    career_mean_zone_contact,
    first_observed_age,
    last_observed_age
  )

#Run imputation
impute_input <- imputation_data %>%
  select(
    xwoba,
    age_centered_imp,
    age_centered_sq_imp,
    observed_season,
    career_mean_xwoba,
    career_mean_pa,
    career_mean_ev_p90,
    career_mean_chase,
    career_mean_whiff,
    career_mean_zone_contact,
    first_observed_age,
    last_observed_age
  )

init <- mice(
  impute_input,
  maxit = 0,
  printFlag = FALSE
)

method <- init$method
pred <- init$predictorMatrix

method[] <- ""
method["xwoba"] <- "pmm"

pred[,] <- 0
pred["xwoba", c(
  "age_centered_imp",
  "age_centered_sq_imp",
  "observed_season",
  "career_mean_xwoba",
  "career_mean_pa",
  "career_mean_ev_p90",
  "career_mean_chase",
  "career_mean_whiff",
  "career_mean_zone_contact",
  "first_observed_age",
  "last_observed_age"
)] <- 1

set.seed(123)

xwoba_imp <- mice(
  impute_input,
  m = 5,
  maxit = 30,
  method = method,
  predictorMatrix = pred,
  printFlag = FALSE
)

#Build imputed curve
imputed_long <- map_dfr(
  1:5,
  function(i) {
    complete(xwoba_imp, action = i) %>%
      bind_cols(
        imputation_data %>%
          select(batter, age_int, missing_player_season)
      ) %>%
      mutate(imputation = i)
  }
)

imputed_aging_curves <- imputed_long %>%
  group_by(imputation, age_int) %>%
  summarise(
    imputed_mean_xwoba = mean(xwoba, na.rm = TRUE),
    .groups = "drop"
  )

combined_imputed_aging_curve <- imputed_aging_curves %>%
  group_by(age_int) %>%
  summarise(
    imputed_mean_xwoba = mean(imputed_mean_xwoba, na.rm = TRUE),
    imputed_sd = sd(imputed_mean_xwoba, na.rm = TRUE),
    imputed_low = imputed_mean_xwoba - 1.96 * imputed_sd,
    imputed_high = imputed_mean_xwoba + 1.96 * imputed_sd,
    .groups = "drop"
  )

aging_curve_compare <- observed_aging_curve %>%
  full_join(
    combined_imputed_aging_curve,
    by = "age_int"
  )

aging_curve_compare

#Plot imputed vs observed
aging_curve_plot_data <- aging_curve_compare %>%
  select(age_int, observed_mean_xwoba, imputed_mean_xwoba) %>%
  pivot_longer(
    cols = c(observed_mean_xwoba, imputed_mean_xwoba),
    names_to = "curve",
    values_to = "mean_xwoba"
  ) %>%
  mutate(
    curve = recode(
      curve,
      observed_mean_xwoba = "Observed only",
      imputed_mean_xwoba = "Imputed missing player-seasons"
    )
  )

ggplot(
  aging_curve_plot_data,
  aes(
    x = age_int,
    y = mean_xwoba,
    color = curve
  )
) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_x_continuous(breaks = seq(age_min, age_max, by = 2)) +
  labs(
    title = "Observed vs Imputed xwOBA Aging Curve",
    subtitle = "Imputation approximates unobserved player-seasons caused by dropout",
    x = "Age",
    y = "Mean xwOBA",
    color = "Curve"
  ) +
  theme_minimal()

#Missingness by age
missingness_by_age <- player_age_panel %>%
  group_by(age_int) %>%
  summarise(
    total_player_age_slots = n(),
    observed_player_seasons = sum(observed_season == 1, na.rm = TRUE),
    missing_player_seasons = sum(missing_player_season == 1, na.rm = TRUE),
    observed_rate = observed_player_seasons / total_player_age_slots,
    missing_rate = missing_player_seasons / total_player_age_slots,
    .groups = "drop"
  )

missingness_by_age

ggplot(
  missingness_by_age,
  aes(
    x = age_int,
    y = missing_rate
  )
) +
  geom_point(alpha = 0.75) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_x_continuous(breaks = seq(age_min, age_max, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Missing Player-Season Rate by Age",
    subtitle = "Missingness increases when players are no longer observed in MLB data",
    x = "Age",
    y = "Missing player-season rate"
  ) +
  theme_minimal()

#Archetype order
archetype_order <- c(
  "Complete",
  "Power-based",
  "Contact/discipline-based",
  "Middle-profile",
  "Low offensive profile"
)

next_state_order <- c(
  "Complete",
  "Power-based",
  "Contact/discipline-based",
  "Middle-profile",
  "Low offensive profile",
  "Reduced/limited role",
  "Exited MLB"
)

movement_order <- c(
  "Upgraded archetype",
  "Same archetype",
  "Downgraded archetype",
  "Reduced/limited role",
  "Exited MLB",
  "Unknown"
)

archetype_rank_lookup <- tibble(
  archetype = c(
    "Low offensive profile",
    "Middle-profile",
    "Contact/discipline-based",
    "Power-based",
    "Complete"
  ),
  archetype_rank = c(1, 2, 3, 4, 5)
)

#Future survival setup
player_season_survival_tuned <- player_season_survival_tuned %>%
  select(
    -any_of(c(
      "archetype_rank",
      "next_state_rank",
      "next_state_lookup",
      "archetype_movement",
      "rank_change",
      "next_observed_season",
      "seasons_until_reappearance",
      "survived_future",
      "reappeared_after_gap",
      "next_observed_archetype",
      "next_state_analysis",
      "next_state_analysis_rank",
      "reduced_role_next_year"
    ))
  ) %>%
  mutate(
    broad_archetype_tuned = factor(
      as.character(broad_archetype_tuned),
      levels = archetype_order
    ),
    
    next_state_tuned = factor(
      as.character(next_state_tuned),
      levels = next_state_order
    ),
    
    age_int = floor(age),
    
    age_group = case_when(
      age_int <= 22 ~ "22 and under",
      age_int >= 23 & age_int <= 39 ~ as.character(age_int),
      age_int >= 40 ~ "40+",
      TRUE ~ NA_character_
    ),
    
    age_group = factor(
      age_group,
      levels = c(
        "22 and under",
        as.character(23:39),
        "40+"
      )
    ),
    
    age_plot = case_when(
      age_int <= 22 ~ 22,
      age_int >= 40 ~ 40,
      TRUE ~ age_int
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
    
    age_centered = age - mean(age, na.rm = TRUE),
    age_centered_sq = age_centered^2,
    age_5 = age / 5
  ) %>%
  arrange(batter, season) %>%
  group_by(batter) %>%
  mutate(
    next_observed_season = lead(season),
    seasons_until_reappearance = next_observed_season - season,
    
    survived_next_year = if_else(
      !is.na(next_observed_season) & seasons_until_reappearance == 1,
      1L,
      0L
    ),
    
    survived_future = if_else(
      !is.na(next_observed_season),
      1L,
      0L
    ),
    
    reappeared_after_gap = if_else(
      !is.na(seasons_until_reappearance) & seasons_until_reappearance > 1,
      1L,
      0L
    ),
    
    next_observed_archetype = lead(as.character(broad_archetype_tuned))
  ) %>%
  ungroup() %>%
  mutate(
    reduced_role_next_year = if_else(
      as.character(next_state_tuned) == "Reduced/limited role",
      1L,
      0L
    ),
    
    next_state_analysis = case_when(
      reduced_role_next_year == 1 ~ "Reduced/limited role",
      survived_future == 0 ~ "Exited MLB",
      
      survived_next_year == 1 &
        as.character(next_state_tuned) %in% archetype_order ~
        as.character(next_state_tuned),
      
      survived_future == 1 &
        !is.na(next_observed_archetype) ~
        next_observed_archetype,
      
      TRUE ~ "Unknown"
    ),
    
    next_state_analysis = factor(
      next_state_analysis,
      levels = c(next_state_order, "Unknown")
    )
  ) %>%
  left_join(
    archetype_rank_lookup,
    by = c("broad_archetype_tuned" = "archetype")
  ) %>%
  left_join(
    archetype_rank_lookup %>%
      transmute(
        next_state_analysis = archetype,
        next_state_analysis_rank = archetype_rank
      ),
    by = "next_state_analysis"
  ) %>%
  mutate(
    archetype_movement = case_when(
      as.character(next_state_analysis) == "Exited MLB" ~ "Exited MLB",
      as.character(next_state_analysis) == "Reduced/limited role" ~ "Reduced/limited role",
      
      !is.na(next_state_analysis_rank) &
        next_state_analysis_rank > archetype_rank ~ "Upgraded archetype",
      
      !is.na(next_state_analysis_rank) &
        next_state_analysis_rank == archetype_rank ~ "Same archetype",
      
      !is.na(next_state_analysis_rank) &
        next_state_analysis_rank < archetype_rank ~ "Downgraded archetype",
      
      TRUE ~ "Unknown"
    ),
    
    archetype_movement = factor(
      archetype_movement,
      levels = movement_order
    ),
    
    rank_change = case_when(
      !is.na(next_state_analysis_rank) ~ next_state_analysis_rank - archetype_rank,
      TRUE ~ NA_real_
    )
  )

#Reduced role check
player_season_survival_tuned %>%
  summarise(
    reduced_role_original = mean(as.character(next_state_tuned) == "Reduced/limited role", na.rm = TRUE),
    reduced_role_fixed = mean(reduced_role_next_year == 1, na.rm = TRUE)
  )

#Survival summaries
survival_by_age_archetype <- player_season_survival_tuned %>%
  group_by(age_plot, age_group, broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    survived = sum(survived_future == 1, na.rm = TRUE),
    exited = sum(survived_future == 0, na.rm = TRUE),
    survival_rate = mean(survived_future, na.rm = TRUE),
    exit_rate = 1 - survival_rate,
    next_year_survival_rate = mean(survived_next_year, na.rm = TRUE),
    gap_return_rate = mean(reappeared_after_gap == 1, na.rm = TRUE),
    reduced_role_rate = mean(reduced_role_next_year == 1, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    avg_chase_rate = mean(chase_rate, na.rm = TRUE),
    avg_whiff_rate = mean(whiff_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    small_cell = player_seasons < 10
  ) %>%
  arrange(
    age_plot,
    broad_archetype_tuned
  )

survival_by_age_archetype

#PA and archetype summaries
survival_by_age_pa_archetype <- player_season_survival_tuned %>%
  group_by(age_plot, age_group, pa_bucket, broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    survived = sum(survived_future == 1, na.rm = TRUE),
    exited = sum(survived_future == 0, na.rm = TRUE),
    survival_rate = mean(survived_future, na.rm = TRUE),
    exit_rate = 1 - survival_rate,
    next_year_survival_rate = mean(survived_next_year, na.rm = TRUE),
    gap_return_rate = mean(reappeared_after_gap == 1, na.rm = TRUE),
    reduced_role_rate = mean(reduced_role_next_year == 1, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    small_cell = player_seasons < 10
  ) %>%
  arrange(
    age_plot,
    pa_bucket,
    broad_archetype_tuned
  )

survival_by_age_pa_archetype

#Transition matrix
transition_by_age_archetype <- player_season_survival_tuned %>%
  count(
    age_plot,
    age_group,
    current_archetype = broad_archetype_tuned,
    next_state = next_state_analysis,
    name = "n"
  ) %>%
  group_by(age_plot, age_group, current_archetype) %>%
  mutate(
    transition_prob = n / sum(n),
    small_cell = sum(n) < 10
  ) %>%
  ungroup() %>%
  mutate(
    current_archetype = factor(
      as.character(current_archetype),
      levels = archetype_order
    ),
    
    next_state = factor(
      as.character(next_state),
      levels = c(next_state_order, "Unknown")
    )
  ) %>%
  arrange(age_plot, current_archetype, next_state)

transition_by_age_archetype

transition_by_age_archetype_wide <- transition_by_age_archetype %>%
  select(age_plot, age_group, current_archetype, next_state, transition_prob) %>%
  pivot_wider(
    names_from = next_state,
    values_from = transition_prob,
    values_fill = 0
  ) %>%
  arrange(age_plot, current_archetype)

transition_by_age_archetype_wide

#Movement summary
movement_summary <- player_season_survival_tuned %>%
  group_by(broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    upgrade_rate = mean(archetype_movement == "Upgraded archetype", na.rm = TRUE),
    same_rate = mean(archetype_movement == "Same archetype", na.rm = TRUE),
    downgrade_rate = mean(archetype_movement == "Downgraded archetype", na.rm = TRUE),
    reduced_role_rate = mean(reduced_role_next_year == 1, na.rm = TRUE),
    exit_rate = mean(archetype_movement == "Exited MLB", na.rm = TRUE),
    gap_return_rate = mean(reappeared_after_gap == 1, na.rm = TRUE),
    avg_rank_change = mean(rank_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(broad_archetype_tuned)

movement_summary

movement_summary_by_age <- player_season_survival_tuned %>%
  group_by(age_plot, age_group, broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    upgrade_rate = mean(archetype_movement == "Upgraded archetype", na.rm = TRUE),
    same_rate = mean(archetype_movement == "Same archetype", na.rm = TRUE),
    downgrade_rate = mean(archetype_movement == "Downgraded archetype", na.rm = TRUE),
    reduced_role_rate = mean(reduced_role_next_year == 1, na.rm = TRUE),
    exit_rate = mean(archetype_movement == "Exited MLB", na.rm = TRUE),
    gap_return_rate = mean(reappeared_after_gap == 1, na.rm = TRUE),
    avg_rank_change = mean(rank_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    small_cell = player_seasons < 10
  ) %>%
  arrange(age_plot, broad_archetype_tuned)

movement_summary_by_age

movement_counts <- player_season_survival_tuned %>%
  count(
    broad_archetype_tuned,
    archetype_movement,
    name = "n"
  ) %>%
  group_by(broad_archetype_tuned) %>%
  mutate(
    movement_prob = n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(broad_archetype_tuned, archetype_movement)

movement_counts

movement_counts_wide <- movement_counts %>%
  select(broad_archetype_tuned, archetype_movement, movement_prob) %>%
  pivot_wider(
    names_from = archetype_movement,
    values_from = movement_prob,
    values_fill = 0
  ) %>%
  arrange(broad_archetype_tuned)

movement_counts_wide

#Age summary
age_state_summary <- player_season_survival_tuned %>%
  group_by(age_plot, age_group) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    survival_rate = mean(survived_future, na.rm = TRUE),
    exit_rate = mean(next_state_analysis == "Exited MLB", na.rm = TRUE),
    next_year_survival_rate = mean(survived_next_year, na.rm = TRUE),
    gap_return_rate = mean(reappeared_after_gap == 1, na.rm = TRUE),
    reduced_role_rate = mean(reduced_role_next_year == 1, na.rm = TRUE),
    stayed_classified_rate = mean(
      survived_future == 1 & reduced_role_next_year == 0,
      na.rm = TRUE
    ),
    upgrade_rate = mean(archetype_movement == "Upgraded archetype", na.rm = TRUE),
    same_rate = mean(archetype_movement == "Same archetype", na.rm = TRUE),
    downgrade_rate = mean(archetype_movement == "Downgraded archetype", na.rm = TRUE),
    avg_rank_change = mean(rank_change, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    avg_whiff_rate = mean(whiff_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    small_cell = player_seasons < 20
  ) %>%
  arrange(age_plot)

age_state_summary

#Stability among survivors
archetype_stability_by_age <- player_season_survival_tuned %>%
  filter(survived_future == 1) %>%
  filter(reduced_role_next_year == 0) %>%
  mutate(
    same_broad_archetype_tuned = if_else(
      as.character(broad_archetype_tuned) == as.character(next_state_analysis),
      1,
      0
    )
  ) %>%
  group_by(age_plot, age_group, broad_archetype_tuned) %>%
  summarise(
    survived_classified_future = n(),
    same_group_future = sum(same_broad_archetype_tuned == 1, na.rm = TRUE),
    changed_group_future = sum(same_broad_archetype_tuned == 0, na.rm = TRUE),
    stability_rate = mean(same_broad_archetype_tuned, na.rm = TRUE),
    transition_rate = 1 - stability_rate,
    upgrade_rate = mean(archetype_movement == "Upgraded archetype", na.rm = TRUE),
    downgrade_rate = mean(archetype_movement == "Downgraded archetype", na.rm = TRUE),
    avg_rank_change = mean(rank_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    small_cell = survived_classified_future < 10
  ) %>%
  arrange(age_plot, broad_archetype_tuned)

archetype_stability_by_age

#Common transitions
common_transitions_by_age <- player_season_survival_tuned %>%
  filter(survived_future == 1) %>%
  count(
    age_plot,
    age_group,
    from = broad_archetype_tuned,
    to = next_state_analysis,
    archetype_movement,
    name = "n"
  ) %>%
  group_by(age_plot, age_group, from) %>%
  mutate(
    transition_share = n / sum(n),
    small_cell = sum(n) < 10
  ) %>%
  ungroup() %>%
  arrange(age_plot, from, to)

common_transitions_by_age

#Age-band summaries
survival_by_age_band_archetype <- player_season_survival_tuned %>%
  group_by(age_band, broad_archetype_tuned) %>%
  summarise(
    player_seasons = n(),
    players = n_distinct(batter),
    survival_rate = mean(survived_future, na.rm = TRUE),
    exit_rate = mean(next_state_analysis == "Exited MLB", na.rm = TRUE),
    next_year_survival_rate = mean(survived_next_year, na.rm = TRUE),
    gap_return_rate = mean(reappeared_after_gap == 1, na.rm = TRUE),
    reduced_role_rate = mean(reduced_role_next_year == 1, na.rm = TRUE),
    upgrade_rate = mean(archetype_movement == "Upgraded archetype", na.rm = TRUE),
    same_rate = mean(archetype_movement == "Same archetype", na.rm = TRUE),
    downgrade_rate = mean(archetype_movement == "Downgraded archetype", na.rm = TRUE),
    avg_rank_change = mean(rank_change, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(age_band, broad_archetype_tuned)

survival_by_age_band_archetype

#Model data
survival_model_data_age <- player_season_survival_tuned %>%
  mutate(
    sprint_speed_missing = if_else(is.na(sprint_speed), 1, 0),
    oaa_missing = if_else(is.na(oaa), 1, 0),
    
    sprint_speed_imp = if_else(
      is.na(sprint_speed),
      median(sprint_speed, na.rm = TRUE),
      sprint_speed
    ),
    
    oaa_imp = if_else(
      is.na(oaa),
      median(oaa, na.rm = TRUE),
      oaa
    ),
    
    survived_future = as.integer(survived_future),
    survived_next_year = as.integer(survived_next_year),
    
    xwoba_050 = xwoba / 0.050,
    ev_p90_5 = ev_p90 / 5,
    chase_10 = chase_rate / 0.10,
    whiff_10 = whiff_rate / 0.10,
    zone_contact_10 = zone_contact_rate / 0.10,
    sprint_1 = sprint_speed_imp,
    oaa_5 = oaa_imp / 5
  ) %>%
  filter(
    !is.na(survived_future),
    !is.na(survived_next_year),
    !is.na(broad_archetype_tuned),
    !is.na(age),
    !is.na(age_group),
    !is.na(age_band),
    !is.na(pa_bucket),
    !is.na(xwoba),
    !is.na(ev_p90),
    !is.na(chase_rate),
    !is.na(whiff_rate),
    !is.na(zone_contact_rate)
  )

#Baseline survival model
survival_logit_age_band <- glm(
  survived_future ~ broad_archetype_tuned +
    age_band +
    pa_bucket,
  data = survival_model_data_age,
  family = binomial()
)

summary(survival_logit_age_band)

survival_logit_age_band_or <- broom::tidy(
  survival_logit_age_band,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

survival_logit_age_band_or

#Skill survival model
survival_logit_age_band_skills <- glm(
  survived_future ~ broad_archetype_tuned +
    age_band +
    pa_bucket +
    xwoba_050 +
    ev_p90_5 +
    chase_10 +
    whiff_10 +
    zone_contact_10,
  data = survival_model_data_age,
  family = binomial()
)

summary(survival_logit_age_band_skills)

survival_logit_age_band_skills_or <- broom::tidy(
  survival_logit_age_band_skills,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

survival_logit_age_band_skills_or

#Age interaction model
survival_logit_age_band_interaction <- glm(
  survived_future ~ broad_archetype_tuned * age_band +
    pa_bucket,
  data = survival_model_data_age,
  family = binomial()
)

summary(survival_logit_age_band_interaction)

survival_logit_age_band_interaction_or <- broom::tidy(
  survival_logit_age_band_interaction,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

survival_logit_age_band_interaction_or

#Nonlinear age model
survival_logit_age_curve <- glm(
  survived_future ~ broad_archetype_tuned +
    pa_bucket +
    age_centered +
    age_centered_sq +
    xwoba_050 +
    ev_p90_5 +
    whiff_10,
  data = survival_model_data_age,
  family = binomial()
)

summary(survival_logit_age_curve)

survival_logit_age_curve_or <- broom::tidy(
  survival_logit_age_curve,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

survival_logit_age_curve_or

#Next-year model
survival_logit_next_year <- glm(
  survived_next_year ~ broad_archetype_tuned +
    age_band +
    pa_bucket,
  data = survival_model_data_age,
  family = binomial()
)

summary(survival_logit_next_year)

survival_logit_next_year_or <- broom::tidy(
  survival_logit_next_year,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

survival_logit_next_year_or

#Movement models
movement_model_data <- survival_model_data_age %>%
  mutate(
    upgraded_future = if_else(archetype_movement == "Upgraded archetype", 1, 0),
    downgraded_future = if_else(archetype_movement == "Downgraded archetype", 1, 0)
  ) %>%
  filter(
    !is.na(upgraded_future),
    !is.na(downgraded_future)
  )

upgrade_logit <- glm(
  upgraded_future ~ broad_archetype_tuned +
    age_centered +
    age_centered_sq +
    pa_bucket +
    xwoba_050 +
    ev_p90_5 +
    whiff_10,
  data = movement_model_data,
  family = binomial()
)

upgrade_logit_or <- broom::tidy(
  upgrade_logit,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

upgrade_logit_or

downgrade_logit <- glm(
  downgraded_future ~ broad_archetype_tuned +
    age_centered +
    age_centered_sq +
    pa_bucket +
    xwoba_050 +
    ev_p90_5 +
    whiff_10,
  data = movement_model_data,
  family = binomial()
)

downgrade_logit_or <- broom::tidy(
  downgrade_logit,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  arrange(desc(estimate))

downgrade_logit_or

#Multinomial model
transition_model_data_age <- survival_model_data_age %>%
  filter(!is.na(next_state_analysis)) %>%
  mutate(
    next_state_analysis = factor(
      as.character(next_state_analysis),
      levels = c(next_state_order, "Unknown")
    )
  )

transition_multinom_age <- nnet::multinom(
  next_state_analysis ~ broad_archetype_tuned +
    pa_bucket +
    age_centered +
    age_centered_sq +
    xwoba_050 +
    ev_p90_5 +
    chase_10 +
    whiff_10 +
    zone_contact_10,
  data = transition_model_data_age,
  trace = FALSE
)

summary(transition_multinom_age)

transition_probs_age <- predict(
  transition_multinom_age,
  newdata = transition_model_data_age,
  type = "probs"
)

transition_probs_age <- as_tibble(transition_probs_age)

avg_transition_probs_age <- transition_probs_age %>%
  summarise(
    across(everything(), function(x) mean(x, na.rm = TRUE))
  )

avg_transition_probs_age

#Survival plot
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order
      )
    ),
  aes(
    x = age_plot,
    y = survival_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Future MLB Survival by Age and Hitter Archetype",
    subtitle = "Survival equals reappearing in any later MLB season in the dataset",
    x = "Age",
    y = "Future survival rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Next-year survival plot
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order
      )
    ),
  aes(
    x = age_plot,
    y = next_year_survival_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Next-Year MLB Survival by Age and Hitter Archetype",
    subtitle = "Strict measure requiring appearance in the immediately following season",
    x = "Age",
    y = "Next-year survival rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Exit plot
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order
      )
    ),
  aes(
    x = age_plot,
    y = exit_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Future MLB Exit Rate by Age and Hitter Archetype",
    subtitle = "Exit means no later MLB season appears in the dataset",
    x = "Age",
    y = "Exit rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Gap-return plot
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order
      )
    ),
  aes(
    x = age_plot,
    y = gap_return_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Return After Missed Season by Age and Hitter Archetype",
    x = "Age",
    y = "Gap-return rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Reduced-role plot
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order
      )
    ),
  aes(
    x = age_plot,
    y = reduced_role_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Reduced-Role Rate by Age and Hitter Archetype",
    x = "Age",
    y = "Reduced-role rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Upgrade plot
ggplot(
  movement_summary_by_age %>%
    filter(!small_cell) %>%
    filter(broad_archetype_tuned != "Complete") %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order[archetype_order != "Complete"]
      )
    ),
  aes(
    x = age_plot,
    y = upgrade_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order[archetype_order != "Complete"]) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Archetype Upgrade Rate by Age",
    x = "Age",
    y = "Upgrade rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Downgrade plot
ggplot(
  movement_summary_by_age %>%
    filter(!small_cell) %>%
    filter(broad_archetype_tuned != "Low offensive profile") %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order[archetype_order != "Low offensive profile"]
      )
    ),
  aes(
    x = age_plot,
    y = downgrade_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order[archetype_order != "Low offensive profile"]) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Archetype Downgrade Rate by Age",
    x = "Age",
    y = "Downgrade rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Stability plot
ggplot(
  archetype_stability_by_age %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = archetype_order
      )
    ),
  aes(
    x = age_plot,
    y = stability_rate,
    color = plot_archetype
  )
) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
  scale_color_discrete(limits = archetype_order) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Archetype Stability by Age",
    x = "Age",
    y = "Stability rate",
    color = "Archetype"
  ) +
  theme_minimal()

#Movement heatmap
ggplot(
  movement_counts %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = rev(archetype_order)
      )
    ),
  aes(
    x = archetype_movement,
    y = plot_archetype,
    fill = movement_prob
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = percent(movement_prob, accuracy = 1)),
    size = 3
  ) +
  scale_y_discrete(
    labels = rev(archetype_order)
  ) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Future Archetype Movement by Current Archetype",
    x = "Future movement",
    y = "Current archetype",
    fill = "Probability"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Exit heatmap
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = rev(archetype_order)
      )
    ),
  aes(
    x = age_plot,
    y = plot_archetype,
    fill = exit_rate
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = percent(exit_rate, accuracy = 1)),
    size = 3
  ) +
  scale_y_discrete(
    labels = rev(archetype_order)
  ) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Future Exit Rate by Age and Hitter Archetype",
    x = "Age",
    y = "Archetype",
    fill = "Exit rate"
  ) +
  theme_minimal()

#Reduced-role heatmap
ggplot(
  survival_by_age_archetype %>%
    filter(!small_cell) %>%
    mutate(
      plot_archetype = factor(
        as.character(broad_archetype_tuned),
        levels = rev(archetype_order)
      )
    ),
  aes(
    x = age_plot,
    y = plot_archetype,
    fill = reduced_role_rate
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = percent(reduced_role_rate, accuracy = 1)),
    size = 3
  ) +
  scale_x_continuous(breaks = seq(22, 40, by = 2)) +
  scale_y_discrete(
    labels = rev(archetype_order)
  ) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Reduced-Role Rate by Age and Hitter Archetype",
    x = "Age",
    y = "Archetype",
    fill = "Reduced-role rate"
  ) +
  theme_minimal()

#Save outputs
dir.create("outputs/analysis_tables", recursive = TRUE, showWarnings = FALSE)

write_csv(aging_curve_compare, "outputs/analysis_tables/aging_curve_compare.csv")
write_csv(missingness_by_age, "outputs/analysis_tables/missingness_by_age.csv")
write_csv(survival_by_age_archetype, "outputs/analysis_tables/survival_by_age_archetype.csv")
write_csv(survival_by_age_pa_archetype, "outputs/analysis_tables/survival_by_age_pa_archetype.csv")
write_csv(transition_by_age_archetype, "outputs/analysis_tables/transition_by_age_archetype.csv")
write_csv(transition_by_age_archetype_wide, "outputs/analysis_tables/transition_by_age_archetype_wide.csv")
write_csv(movement_summary, "outputs/analysis_tables/movement_summary.csv")
write_csv(movement_summary_by_age, "outputs/analysis_tables/movement_summary_by_age.csv")
write_csv(movement_counts, "outputs/analysis_tables/movement_counts.csv")
write_csv(movement_counts_wide, "outputs/analysis_tables/movement_counts_wide.csv")
write_csv(age_state_summary, "outputs/analysis_tables/age_state_summary.csv")
write_csv(archetype_stability_by_age, "outputs/analysis_tables/archetype_stability_by_age.csv")
write_csv(common_transitions_by_age, "outputs/analysis_tables/common_transitions_by_age.csv")
write_csv(survival_by_age_band_archetype, "outputs/analysis_tables/survival_by_age_band_archetype.csv")
write_csv(survival_logit_age_band_or, "outputs/analysis_tables/survival_logit_age_band_or.csv")
write_csv(survival_logit_age_band_skills_or, "outputs/analysis_tables/survival_logit_age_band_skills_or.csv")
write_csv(survival_logit_age_band_interaction_or, "outputs/analysis_tables/survival_logit_age_band_interaction_or.csv")
write_csv(survival_logit_age_curve_or, "outputs/analysis_tables/survival_logit_age_curve_or.csv")
write_csv(survival_logit_next_year_or, "outputs/analysis_tables/survival_logit_next_year_or.csv")
write_csv(upgrade_logit_or, "outputs/analysis_tables/upgrade_logit_or.csv")
write_csv(downgrade_logit_or, "outputs/analysis_tables/downgrade_logit_or.csv")
write_csv(avg_transition_probs_age, "outputs/analysis_tables/avg_transition_probs_age.csv")
