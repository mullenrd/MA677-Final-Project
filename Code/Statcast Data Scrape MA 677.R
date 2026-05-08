library(tidyverse)
library(lubridate)
library(data.table)
library(httr)
library(jsonlite)
library(baseballr)
library(janitor)
library(readr)
library(stringr)

dir.create("statcast_data", showWarnings = FALSE)

# seasons to pull (PLEASE CHANGE BEFORE RUNNING IT TAKES 2+ HOURS TO PULL ALL SEASONS)
seasons <- 2015:2025

# Statcast url function
build_statcast_url <- function(start_date, end_date, season) {
  
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?",
    "all=true",
    "&hfPT=",
    "&hfAB=",
    "&hfBBT=",
    "&hfPR=",
    "&hfZ=",
    "&stadium=",
    "&hfBBL=",
    "&hfNewZones=",
    "&hfGT=R%7CPO%7CS%7C",
    "&hfSea=", season, "%7C",
    "&player_type=batter",
    "&game_date_gt=", start_date,
    "&game_date_lt=", end_date,
    "&type=details"
  )
}

make_chunks <- function(season, days = 7) {
  
  start <- as.Date(paste0(season, "-03-15"))
  end   <- as.Date(paste0(season, "-10-01"))
  
  tibble(
    start_date = seq(start, end, by = days)
  ) %>%
    mutate(
      end_date = pmin(start_date + days(days - 1), end)
    )
}

pull_season <- function(season) {
  
  message("Starting season ", season)
  
  chunks <- make_chunks(season)
  
  season_list <- list()
  
  for(i in 1:nrow(chunks)) {
    
    start_date <- chunks$start_date[i]
    end_date   <- chunks$end_date[i]
    
    message(start_date, " -> ", end_date)
    
    url <- build_statcast_url(start_date, end_date, season)
    
    try({
      
      tmp <- fread(url)
      
      tmp$season <- season
      
      season_list[[i]] <- tmp
      
      Sys.sleep(1)
      
    }, silent = TRUE)
  }
  
  final <- rbindlist(season_list, fill = TRUE)
  
  fwrite(
    final,
    paste0("statcast_data/statcast_", season, ".csv")
  )
  
  return(final)
}

# pulls all years
all_data <- lapply(seasons, pull_season)

statcast <- rbindlist(all_data, fill = TRUE)

dir.create("data/player_bios", recursive = TRUE, showWarnings = FALSE)

#Isolate Player IDS
player_ids <- statcast %>%
  distinct(batter) %>%
  filter(!is.na(batter)) %>%
  pull(batter)

player_info_list <- list()

# MLB API Bio function
for (id in player_ids) {
  
  message("Pulling player ID: ", id)
  
  url <- paste0("https://statsapi.mlb.com/api/v1/people/", id)
  
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    
    player_data <- jsonlite::fromJSON(
      httr::content(response, "text", encoding = "UTF-8")
    )
    
    if (!is.null(player_data$people) && nrow(player_data$people) > 0) {
      
      player <- player_data$people[1, ]
      
      player_info_temp <- data.frame(
        batter = id,
        full_name = if (!is.null(player$fullName)) player$fullName else NA,
        birth_date = if (!is.null(player$birthDate)) player$birthDate else NA,
        height = if (!is.null(player$height)) player$height else NA,
        weight = if (!is.null(player$weight)) player$weight else NA,
        current_age = if (!is.null(player$currentAge)) player$currentAge else NA,
        bat_side = if (!is.null(player$batSide$code)) player$batSide$code else NA,
        throw_hand = if (!is.null(player$pitchHand$code)) player$pitchHand$code else NA,
        primary_position = if (!is.null(player$primaryPosition$abbreviation)) {
          player$primaryPosition$abbreviation
        } else {
          NA
        },
        stringsAsFactors = FALSE
      )
      
      player_info_list[[length(player_info_list) + 1]] <- player_info_temp
    }
  }
  
  Sys.sleep(0.05)
}

player_info <- bind_rows(player_info_list)

#Height helper
height_to_inches <- function(height) {
  feet <- as.numeric(str_extract(height, "^\\d+"))
  inches <- as.numeric(str_extract(height, "(?<=')\\s*\\d+"))
  feet * 12 + inches
}


player_info_clean <- player_info %>%
  mutate(
    birth_date = as.Date(birth_date),
    height_inches = height_to_inches(height),
    weight = as.numeric(weight)
  )

player_info_by_season <- expand_grid(
  batter = player_ids,
  season = seasons
) %>%
  left_join(player_info_clean, by = "batter") %>%
  mutate(
    age = season - year(birth_date)
  )

#Function to grab outs above average
pull_oaa <- function(season) {
  
  message("Pulling OAA for ", season)
  
  tryCatch({
    
    oaa <- baseballr::statcast_leaderboards(
      leaderboard = "outs_above_average",
      year = season,
      min_field = "q"
    )
    
    oaa %>%
      mutate(season = season) %>%
      rename(batter = player_id) %>%
      select(
        batter,
        season,
        oaa = outs_above_average
      )
    
  }, error = function(e) {
    
    message("OAA failed for ", season)
    
    tibble(
      batter = numeric(),
      season = numeric(),
      oaa = numeric()
    )
  })
}

oaa_by_season <- map_dfr(seasons, pull_oaa)

#Fucntion to pull sprint speed
pull_sprint_speed <- function(season) {
  
  message("Pulling sprint speed for ", season)
  
  tryCatch({
    
    sprint <- baseballr::statcast_leaderboards(
      leaderboard = "sprint_speed",
      year = season,
      min_field = "q"
    )
    
    sprint %>%
      mutate(season = season) %>%
      rename(batter = player_id) %>%
      select(
        batter,
        season,
        sprint_speed
      )
    
  }, error = function(e) {
    
    message("Sprint speed failed for ", season)
    
    tibble(
      batter = numeric(),
      season = numeric(),
      sprint_speed = numeric()
    )
  })
}

sprint_by_season <- map_dfr(seasons, pull_sprint_speed)

##Events/Plate Discipline Definitions
in_zone = zone %in% 1:9
out_zone = zone %in% c(11, 12, 13, 14)

swing_descriptions <- c(
  "swinging_strike",
  "swinging_strike_blocked",
  "foul",
  "foul_tip",
  "hit_into_play",
  "hit_into_play_no_out",
  "hit_into_play_score"
)

contact_descriptions <- c(
  "foul",
  "foul_tip",
  "hit_into_play",
  "hit_into_play_no_out",
  "hit_into_play_score"
)

whiff_descriptions <- c(
  "swinging_strike",
  "swinging_strike_blocked"
)

take_descriptions <- c(
  "ball",
  "blocked_ball",
  "called_strike",
  "hit_by_pitch"
)

called_strike_descriptions <- c(
  "called_strike"
)

ball_descriptions <- c(
  "ball",
  "blocked_ball"
)


# Aggregation Helpers
rate_num_denom <- function(num, denom) {
  if (sum(denom, na.rm = TRUE) == 0) return(NA_real_)
  sum(num & denom, na.rm = TRUE) / sum(denom, na.rm = TRUE)
}

count_safe <- function(x) {
  sum(x, na.rm = TRUE)
}

min_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

max_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

mean_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

q_safe <- function(x, p) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, probs = p, na.rm = TRUE))
}

sd_safe <- function(x) {
  if (sum(!is.na(x)) <= 1) return(NA_real_)
  sd(x, na.rm = TRUE)
}
rate_safe <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

#Variable Aggregation by player-season
player_season_statcast <- statcast %>%
  mutate(
    in_zone = zone %in% 1:9,
    out_zone = zone %in% c(11, 12, 13, 14),
    
    swing = description %in% swing_descriptions,
    contact = description %in% contact_descriptions,
    whiff = description %in% whiff_descriptions,
    take = description %in% take_descriptions,
    called_strike = description %in% called_strike_descriptions,
    ball_called = description %in% ball_descriptions
  ) %>%
  group_by(batter, season) %>%
  summarise(
    player_name = first(player_name),
    
    # Playing time
    pitches_seen = n(),
    plate_appearances = sum(!is.na(events), na.rm = TRUE),
    batted_balls = sum(!is.na(launch_speed), na.rm = TRUE),
    
    # Zone counts
    pitches_in_zone = count_safe(in_zone),
    pitches_out_zone = count_safe(out_zone),
    
    # Overall plate discipline
    swing_rate = rate_safe(swing),
    contact_rate = rate_num_denom(contact, swing),
    whiff_rate = rate_num_denom(whiff, swing),
    take_rate = rate_safe(take),
    
    # Zone swing 
    zone_swing_rate = rate_num_denom(swing, in_zone),
    chase_rate = rate_num_denom(swing, out_zone),
    
    # Zone contact 
    zone_contact_rate = rate_num_denom(contact, swing & in_zone),
    out_zone_contact_rate = rate_num_denom(contact, swing & out_zone),
    
    # Whiff rates by zone
    zone_whiff_rate = rate_num_denom(whiff, swing & in_zone),
    out_zone_whiff_rate = rate_num_denom(whiff, swing & out_zone),
    
    # Takes by zone
    zone_take_rate = rate_num_denom(take, in_zone),
    out_zone_take_rate = rate_num_denom(take, out_zone),
    
    # Called-strike / ball 
    called_strike_rate = rate_safe(called_strike),
    called_strike_in_zone_rate = rate_num_denom(called_strike, in_zone),
    ball_out_zone_rate = rate_num_denom(ball_called, out_zone),
    
    # Simple discipline  
    chase_minus_zone_swing = chase_rate - zone_swing_rate,
    contact_gap = zone_contact_rate - out_zone_contact_rate,
    
    # Exit velocity 
    ev_mean = mean_safe(launch_speed),
    ev_sd = sd_safe(launch_speed),
    ev_min = min_safe(launch_speed),
    ev_p10 = q_safe(launch_speed, 0.10),
    ev_p25 = q_safe(launch_speed, 0.25),
    ev_median = q_safe(launch_speed, 0.50),
    ev_p75 = q_safe(launch_speed, 0.75),
    ev_p90 = q_safe(launch_speed, 0.90),
    ev_p95 = q_safe(launch_speed, 0.95),
    ev_max = max_safe(launch_speed),
    ev_iqr = ev_p75 - ev_p25,
    
    # Contact quality rates
    weak_contact_rate = rate_safe(launch_speed < 75),
    medium_contact_rate = rate_safe(launch_speed >= 75 & launch_speed < 95),
    hard_hit_rate = rate_safe(launch_speed >= 95),
    elite_ev_rate = rate_safe(launch_speed >= 100),
    top_end_ev_rate = rate_safe(launch_speed >= 105),
    
    # Launch angle distribution
    la_mean = mean_safe(launch_angle),
    la_sd = sd_safe(launch_angle),
    la_min = min_safe(launch_angle),
    la_p10 = q_safe(launch_angle, 0.10),
    la_p25 = q_safe(launch_angle, 0.25),
    la_median = q_safe(launch_angle, 0.50),
    la_p75 = q_safe(launch_angle, 0.75),
    la_p90 = q_safe(launch_angle, 0.90),
    la_max = max_safe(launch_angle),
    la_iqr = la_p75 - la_p25,
    
    # Batted ball 
    ground_ball_rate = rate_safe(launch_angle < 10),
    line_drive_rate = rate_safe(launch_angle >= 10 & launch_angle < 25),
    fly_ball_rate = rate_safe(launch_angle >= 25 & launch_angle < 50),
    popup_rate = rate_safe(launch_angle >= 50),
    
    # Ideal contact windows
    sweet_spot_rate = rate_safe(launch_angle >= 8 & launch_angle <= 32),
    hard_sweet_spot_rate = rate_safe(
      launch_speed >= 95 & launch_angle >= 8 & launch_angle <= 32
    ),
    
    # Distance distribution
    distance_mean = mean_safe(hit_distance_sc),
    distance_sd = sd_safe(hit_distance_sc),
    distance_p75 = q_safe(hit_distance_sc, 0.75),
    distance_p90 = q_safe(hit_distance_sc, 0.90),
    distance_max = max_safe(hit_distance_sc),
    
    # Expected and observed offense
    xba = mean_safe(estimated_ba_using_speedangle),
    xwoba = mean_safe(estimated_woba_using_speedangle),
    woba = mean_safe(woba_value),
    run_value = sum(delta_run_exp, na.rm = TRUE),
    run_value_per_pa = if_else(
      plate_appearances > 0,
      run_value / plate_appearances,
      NA_real_
    ),
    
    # Contact classification
    barrel_rate = rate_safe(launch_speed_angle == 6),
    solid_contact_rate = rate_safe(launch_speed_angle == 5),
    flare_burner_rate = rate_safe(launch_speed_angle == 4),
    poorly_hit_rate = rate_safe(launch_speed_angle %in% c(1, 2, 3)),
    
    # Swing-level metrics
    bat_speed_mean = mean_safe(bat_speed),
    bat_speed_sd = sd_safe(bat_speed),
    bat_speed_p75 = q_safe(bat_speed, 0.75),
    bat_speed_p90 = q_safe(bat_speed, 0.90),
    
    swing_length_mean = mean_safe(swing_length),
    swing_length_sd = sd_safe(swing_length),
    swing_length_p75 = q_safe(swing_length, 0.75),
    swing_length_p90 = q_safe(swing_length, 0.90),
    
    .groups = "drop"
  )


#Data Join
player_season_full <- player_season_statcast %>%
  left_join(player_info_by_season, by = c("batter", "season")) %>%
  left_join(oaa_by_season, by = c("batter", "season")) %>%
  left_join(sprint_by_season, by = c("batter", "season"))

#Data Filter
player_season_full <- player_season_full %>%
  mutate(
    age = as.numeric(age),
    height_inches = as.numeric(height_inches),
    weight = as.numeric(weight),
    oaa = as.numeric(oaa),
    sprint_speed = as.numeric(sprint_speed)
  ) %>%
  filter(
    primary_position != "P",
    plate_appearances >= 50
  )

#Add Survival Element
player_season_full <- player_season_full %>%
  group_by(batter) %>%
  arrange(season, .by_group = TRUE) %>%
  mutate(
    next_observed_season = lead(season),
    survived_next_year = if_else(next_observed_season == season + 1, 1, 0)
  ) %>%
  ungroup()


player_season_model <- player_season_full %>%
  filter(season < max(season))

#Remove missing
player_season_model <- player_season_model %>%
  filter(
    plate_appearances >= 50,
    batted_balls >= 25
  ) %>%
  filter(
    !is.na(xwoba),
    !is.nan(xwoba),
    !is.na(ev_p90),
    !is.nan(ev_p90),
    !is.na(hard_hit_rate),
    !is.nan(hard_hit_rate),
    !is.na(chase_rate),
    !is.nan(chase_rate),
    !is.na(whiff_rate),
    !is.nan(whiff_rate),
    !is.na(zone_contact_rate),
    !is.nan(zone_contact_rate)
  )

#Define player archetype thresholds
season_thresholds <- player_season_model %>%
  group_by(season) %>%
  summarise(
    # Contact quality / power thresholds
    ev_p90_hi = quantile(ev_p90, 0.67, na.rm = TRUE),
    ev_p90_elite = quantile(ev_p90, 0.90, na.rm = TRUE),
    
    hard_hit_hi = quantile(hard_hit_rate, 0.67, na.rm = TRUE),
    barrel_hi = quantile(barrel_rate, 0.67, na.rm = TRUE),
    xwoba_hi = quantile(xwoba, 0.67, na.rm = TRUE),
    
    # Plate discipline thresholds
    chase_low = quantile(chase_rate, 0.33, na.rm = TRUE),
    chase_high = quantile(chase_rate, 0.67, na.rm = TRUE),
    
    zone_contact_hi = quantile(zone_contact_rate, 0.67, na.rm = TRUE),
    out_zone_contact_hi = quantile(out_zone_contact_rate, 0.67, na.rm = TRUE),
    
    whiff_low = quantile(whiff_rate, 0.33, na.rm = TRUE),
    whiff_high = quantile(whiff_rate, 0.67, na.rm = TRUE),
    
    # Speed / defense thresholds
    sprint_hi = quantile(sprint_speed, 0.67, na.rm = TRUE),
    sprint_low = quantile(sprint_speed, 0.33, na.rm = TRUE),
    
    oaa_hi = quantile(oaa, 0.67, na.rm = TRUE),
    oaa_low = quantile(oaa, 0.33, na.rm = TRUE),
    
    .groups = "drop"
  )


player_season_model <- player_season_model %>%
  left_join(season_thresholds, by = "season") %>%
  mutate(
    # Contact quality / power indicators
    high_top_end_ev = ev_p90 >= ev_p90_hi,
    elite_top_end_ev = ev_p90 >= ev_p90_elite,
    high_hard_hit = hard_hit_rate >= hard_hit_hi,
    high_barrel = barrel_rate >= barrel_hi,
    high_xwoba = xwoba >= xwoba_hi,
    
    # Plate discipline indicators
    low_chase = chase_rate <= chase_low,
    high_chase = chase_rate >= chase_high,
    high_zone_contact = zone_contact_rate >= zone_contact_hi,
    high_out_zone_contact = out_zone_contact_rate >= out_zone_contact_hi,
    low_whiff = whiff_rate <= whiff_low,
    high_whiff = whiff_rate >= whiff_high,
    
    # Speed / defense indicators
    fast_runner = sprint_speed >= sprint_hi,
    slow_runner = sprint_speed <= sprint_low,
    strong_defender = oaa >= oaa_hi,
    poor_defender = oaa <= oaa_low
  )

# Apply hitter type based on scores
player_season_model <- player_season_model %>%
  mutate(
    # Contact quality / power score
    power_score =
      as.integer(high_top_end_ev) +
      as.integer(high_hard_hit) +
      as.integer(high_barrel) +
      as.integer(high_xwoba),
    
    # Plate discipline / contact score
    discipline_score =
      as.integer(low_chase) +
      as.integer(high_zone_contact) +
      as.integer(low_whiff),
    
    # Chase-contact score
    chase_contact_score =
      as.integer(high_chase) +
      as.integer(high_out_zone_contact) +
      as.integer(!high_whiff),
    
    # Speed / defense score
    speed_defense_score =
      as.integer(fast_runner) +
      as.integer(strong_defender),
    
    # Weak offensive profile score
    low_skill_score =
      as.integer(!high_top_end_ev) +
      as.integer(!high_xwoba) +
      as.integer(high_chase) +
      as.integer(high_whiff),
    
    # Middle-profile indicators
    some_power = power_score == 1,
    some_discipline = discipline_score == 1,
    mixed_power_contact = power_score >= 1 & discipline_score >= 1,
    neutral_profile = power_score <= 1 & discipline_score <= 1 & low_skill_score <= 2
  ) %>%
  rowwise() %>%
  mutate(
    max_skill_score = max(
      power_score,
      discipline_score,
      chase_contact_score,
      speed_defense_score,
      low_skill_score,
      na.rm = TRUE
    ),
    
    hitter_archetype = case_when(
      # Strong all-around offensive profile
      power_score >= 3 & discipline_score >= 2 ~
        "Complete hitter",
      
      # Power groups
      power_score == max_skill_score & power_score >= 2 & high_whiff ~
        "Power with swing-and-miss",
      
      power_score == max_skill_score & power_score >= 2 ~
        "Power hitter",
      
      # Discipline/contact groups
      discipline_score == max_skill_score & discipline_score >= 2 ~
        "Contact-discipline hitter",
      
      chase_contact_score == max_skill_score & chase_contact_score >= 2 ~
        "Chase-contact hitter",
      
      # Defense/speed group
      speed_defense_score == max_skill_score & speed_defense_score >= 1 & !high_xwoba ~
        "Speed-defense specialist",
      
      # Low offensive profile
      low_skill_score == max_skill_score & low_skill_score >= 3 ~
        "Low-skill offensive profile",
      
      # Middle-profile groups
      mixed_power_contact ~
        "Balanced power-contact hitter",
      
      some_discipline & !some_power ~
        "Average contact-control hitter",
      
      some_power & !some_discipline ~
        "Average power-leaning hitter",
      
      neutral_profile ~
        "Average regular profile",
      
      TRUE ~
        "Unclassified middle profile"
    )
  ) %>%
  ungroup()

# Aggregate more
player_season_model <- player_season_model %>%
  mutate(
    broad_archetype = case_when(
      hitter_archetype == "Complete hitter" ~
        "Complete",
      
      hitter_archetype %in% c(
        "Power hitter",
        "Power with swing-and-miss"
      ) ~
        "Power-based",
      
      hitter_archetype %in% c(
        "Contact-discipline hitter",
        "Chase-contact hitter"
      ) ~
        "Contact/discipline-based",
      
      hitter_archetype == "Speed-defense specialist" ~
        "Speed/defense-based",
      
      hitter_archetype == "Low-skill offensive profile" ~
        "Low offensive profile",
      
      hitter_archetype %in% c(
        "Balanced power-contact hitter",
        "Average contact-control hitter",
        "Average power-leaning hitter",
        "Average regular profile",
        "Unclassified middle profile"
      ) ~
        "Middle-profile",
      
      TRUE ~
        "Middle-profile"
    )
  )

# Check yearly distribution
player_season_model %>%
  count(season, hitter_archetype) %>%
  arrange(season, desc(n))

# Check overall detailed distribution
player_season_model %>%
  count(hitter_archetype, sort = TRUE)

# Check overall broad  distribution
player_season_model %>%
  count(broad_archetype, sort = TRUE)

# Profile Summary
archetype_profile_check <- player_season_model %>%
  group_by(hitter_archetype) %>%
  summarise(
    n = n(),
    players = n_distinct(batter),
    avg_age = mean(age, na.rm = TRUE),
    avg_pa = mean(plate_appearances, na.rm = TRUE),
    avg_batted_balls = mean(batted_balls, na.rm = TRUE),
    avg_xwoba = mean(xwoba, na.rm = TRUE),
    avg_ev_p90 = mean(ev_p90, na.rm = TRUE),
    avg_hard_hit_rate = mean(hard_hit_rate, na.rm = TRUE),
    avg_barrel_rate = mean(barrel_rate, na.rm = TRUE),
    avg_chase_rate = mean(chase_rate, na.rm = TRUE),
    avg_whiff_rate = mean(whiff_rate, na.rm = TRUE),
    avg_zone_contact_rate = mean(zone_contact_rate, na.rm = TRUE),
    avg_sprint_speed = mean(sprint_speed, na.rm = TRUE),
    avg_oaa = mean(oaa, na.rm = TRUE),
    survival_rate = mean(survived_next_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

archetype_profile_check

#CSV Outputs
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/model_data", showWarnings = FALSE)
dir.create("outputs/summaries", showWarnings = FALSE)

# Statcast data
fwrite(
  statcast,
  "outputs/model_data/statcast_raw_combined_2015_2025.csv"
)

# Playere info
write_csv(
  player_info_clean,
  "outputs/model_data/player_info_clean.csv"
)

write_csv(
  player_info_by_season,
  "outputs/model_data/player_info_by_season.csv"
)

# Leaderboard pulls
write_csv(
  oaa_by_season,
  "outputs/model_data/oaa_by_season.csv"
)

write_csv(
  sprint_by_season,
  "outputs/model_data/sprint_by_season.csv"
)

# Aggregated Tables
write_csv(
  player_season_statcast,
  "outputs/model_data/player_season_statcast.csv"
)

# Joined table
write_csv(
  player_season_full,
  "outputs/model_data/player_season_full.csv"
)

# Model Dataset archetypes
write_csv(
  player_season_model,
  "outputs/model_data/player_season_model_archetypes.csv"
)

# Season archetype thresholds
write_csv(
  season_thresholds,
  "outputs/model_data/season_thresholds.csv"
)

# Archetypes
write_csv(
  archetype_profile_check,
  "outputs/summaries/archetype_profile_check.csv"
)
