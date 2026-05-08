# MA677-Final-Project
# MLB Hitter Survivorship Analysis

This project studies survivorship bias in Major League Baseball hitter aging patterns using Statcast player-season data from 2015 to 2025. The main question is whether observed aging curves are shaped not only by how hitters perform as they age, but also by which hitters remain in the MLB sample long enough to be observed.

The project begins with raw Statcast data, aggregates it into player-season records, creates hitter archetypes based on offensive skill profiles, and then models future survival, exit, reduced role, and archetype movement.

## Important Note Before Running

The Statcast scraping file should not be rerun unless absolutely necessary. It pulls pitch-level Statcast data across multiple seasons and can take multiple hours to complete. The repository is designed so that the scraping step only needs to be run once. After the output CSVs are created, the EDA and modeling scripts can load those saved files directly.

## Code Files

### 1. `Statcast Data Scrape MA 677.R`

This script pulls and builds the main dataset used in the analysis.

It does the following:

- Pulls Statcast data from Baseball Savant for MLB seasons 2015 through 2025.
- Splits each season into smaller date chunks to make the scrape more manageable.
- Pulls player biographical information from the MLB Stats API.
- Pulls supporting leaderboard data such as Outs Above Average and sprint speed.
- Aggregates pitch-level Statcast records into player-season observations.
- Creates plate-discipline, batted-ball, contact-quality, and expected-offense variables.
- Filters the data to non-pitchers with at least 50 plate appearances.
- Creates hitter archetypes using season-specific thresholds.
- Saves the main modeling datasets and summary CSVs.

Do not rerun this script unless the raw data need to be rebuilt. It is the slowest file in the project.

### 2. `Final EDA MA 677.R`

This script creates descriptive summaries and exploratory charts.

It loads the saved CSVs from the scrape file and builds the working EDA dataset. It does not need to scrape Statcast again.

It includes:

- Basic dataset summaries.
- Player-season counts by season and age.
- Unique hitter counts by season and age.
- Plate appearance distributions.
- Observed xwOBA and core hitter metrics by age.
- Distributions of key variables such as xwOBA, 90th percentile exit velocity, chase rate, whiff rate, and zone contact rate.
- Archetype composition charts.
- Archetype summaries by age band.
- Average xwOBA and plate appearances by archetype.
- Median and interquartile range summaries for the main variables.

The purpose of this script is to explain the structure of the dataset before moving into survival modeling.

### 3. `Survival Analysis MA 677.R`

This script runs the main survivorship and transition analysis.

It loads the saved player-season dataset, rebuilds the survival variables, performs the imputation exercise, creates summary tables, fits models, and produces the main survival plots.

It includes:

- A player-age panel for missing player-season analysis.
- An observed-only aging curve and an imputed xwOBA aging curve.
- Missing player-season rates by age.
- Future survival indicators.
- Next-year survival indicators.
- Gap-return indicators.
- Reduced-role indicators.
- Archetype transition and movement labels.
- Survival summaries by age, archetype, and plate appearance bucket.
- Movement summaries showing upgrades, downgrades, reduced roles, and exits.
- Logistic regression models for future survival.
- Skill-adjusted survival models.
- Age-by-archetype interaction models.
- Continuous nonlinear age models.
- Strict next-year survival models.
- Upgrade and downgrade movement models.
- A multinomial transition model.
- Plots for survival, exit, reduced role, upgrade, downgrade, stability, and movement.

The main modeling outcome is future survival, defined as whether a hitter appears again in any later MLB season in the dataset. This avoids treating a player who misses one season but later returns as a permanent exit.

## Output CSVs

The scripts save outputs into the `outputs/` folder. The main folders are:

### `Outputs/Model Data/`

These are the core datasets created by the scraping and data-building file.

Key files include:

- `statcast_raw_combined_2015_2025.csv`  
  The combined raw Statcast pull across all seasons.

- `player_info_clean.csv`  
  Cleaned player biographical information from the MLB Stats API.

- `player_info_by_season.csv`  
  Player biographical information expanded by season, including age.

- `oaa_by_season.csv`  
  Outs Above Average leaderboard data by player-season.

- `sprint_by_season.csv`  
  Sprint speed leaderboard data by player-season.

- `player_season_statcast.csv`  
  Aggregated player-season Statcast metrics before joining bio, speed, and defensive data.

- `player_season_full.csv`  
  Full joined player-season dataset before final model filtering.

- `player_season_model_archetypes.csv`  
  Final player-season modeling dataset with hitter archetype classifications.

- `season_thresholds.csv`  
  Season-specific percentile thresholds used to classify hitter archetypes.

### `Outputs/Summaries/`

These are summary files created during the data-building process.

Key files include:

- `archetype_profile_check.csv`  
  Summary of each hitter archetype, including average age, plate appearances, xwOBA, exit velocity, plate-discipline metrics, sprint speed, Outs Above Average, and next-year survival rate.

- `yearly_hitter_archetype_distribution.csv`  
  Counts of detailed hitter archetypes by season.

- `overall_hitter_archetype_distribution.csv`  
  Overall counts of detailed hitter archetypes.

- `overall_broad_archetype_distribution.csv`  
  Overall counts of the broader archetype groups used in the analysis.

### `Outputs/EDA/`

These are tables created by the EDA script.

Key files include:

- `data_summary.csv`  
  Overall dataset summary, including number of player-seasons, unique players, season range, age range, and average core metrics.

- `data_by_season.csv`  
  Player-season counts, unique player counts, average age, average plate appearances, and average xwOBA by season.

- `data_by_age.csv`  
  Player-season counts, unique player counts, average plate appearances, and average hitter metrics by age.

- `pa_distribution.csv`  
  Distribution of plate appearance buckets.

- `archetype_composition.csv`  
  Overall distribution of broad hitter archetypes.

- `archetype_by_age_band.csv`  
  Archetype composition by age band.

- `archetype_by_age.csv`  
  Archetype composition by one-year age group.

- `archetype_metric_summary.csv`  
  Average hitter metrics by archetype.

- `median_summary.csv`  
  Median and interquartile range summaries for plate appearances, xwOBA, exit velocity, chase rate, whiff rate, and zone contact rate.

### `Outputs/Analysis/`

These are tables created by the survival modeling script.

Key files include:

- `aging_curve_compare.csv`  
  Observed and imputed xwOBA aging curves.

- `missingness_by_age.csv`  
  Missing player-season rates by age.

- `survival_by_age_archetype.csv`  
  Survival, exit, gap-return, and reduced-role rates by age and hitter archetype.

- `survival_by_age_pa_archetype.csv`  
  Survival summaries by age, plate appearance bucket, and archetype.

- `transition_by_age_archetype.csv`  
  Long-format transition probabilities by age and current archetype.

- `transition_by_age_archetype_wide.csv`  
  Wide-format transition probabilities by age and current archetype.

- `movement_summary.csv`  
  Overall upgrade, same-archetype, downgrade, reduced-role, and exit rates by archetype.

- `movement_summary_by_age.csv`  
  Archetype movement rates by age.

- `movement_counts.csv`  
  Long-format movement counts and probabilities.

- `movement_counts_wide.csv`  
  Wide-format movement probabilities by archetype.

- `age_state_summary.csv`  
  Overall survival, exit, reduced-role, and movement rates by age.

- `archetype_stability_by_age.csv`  
  Stability and transition rates among players who survive into a later season.

- `common_transitions_by_age.csv`  
  Common future transitions by age group and current archetype.

- `survival_by_age_band_archetype.csv`  
  Survival and movement summaries by broader age band and archetype.

- `survival_logit_age_band_or.csv`  
  Odds ratios from the baseline future survival model.

- `survival_logit_age_band_skills_or.csv`  
  Odds ratios from the skill-adjusted future survival model.

- `survival_logit_age_band_interaction_or.csv`  
  Odds ratios from the archetype-by-age-band interaction model.

- `survival_logit_age_curve_or.csv`  
  Odds ratios from the continuous nonlinear age model.

- `survival_logit_next_year_or.csv`  
  Odds ratios from the strict next-year survival model.

- `upgrade_logit_or.csv`  
  Odds ratios from the upgrade movement model.

- `downgrade_logit_or.csv`  
  Odds ratios from the downgrade movement model.

- `avg_transition_probs_age.csv`  
  Average predicted transition probabilities from the multinomial transition model.

## Main Variables

Some of the main variables used in the project are:

- `batter`: MLB player ID.
- `season`: MLB season.
- `age`: Player age in that season.
- `plate_appearances`: Number of plate appearances in the player-season.
- `xwoba`: Expected weighted on-base average.
- `ev_p90`: 90th percentile exit velocity.
- `chase_rate`: Share of out-of-zone pitches swung at.
- `whiff_rate`: Share of swings that result in whiffs.
- `zone_contact_rate`: Contact rate on swings inside the strike zone.
- `broad_archetype_tuned`: Broad offensive hitter profile.
- `survived_future`: Whether the player appears again in any later MLB season.
- `survived_next_year`: Whether the player appears in the immediately following MLB season.
- `reappeared_after_gap`: Whether the player misses at least one season but later returns.
- `reduced_role_next_year`: Whether the player remains present but moves into a smaller role.
- `archetype_movement`: Whether the player upgrades, stays in the same archetype, downgrades, enters a reduced role, or exits MLB.

## Hitter Archetypes

The project groups hitters into five broad offensive profiles:

- `Complete`
- `Power-based`
- `Contact/discipline-based`
- `Middle-profile`
- `Low offensive profile`

These categories are defined using season-specific thresholds, so players are classified relative to their season's offensive environment.

## Project Goal

The goal of the project is not just to estimate a traditional aging curve. Instead, the project studies how MLB aging curves are shaped by sample selection. Older hitters who remain in the data are not simply older versions of younger hitters. They are players who have already survived roster competition, performance decline, role changes, injury risk, and team decision-making.

The main conclusion is that observed aging patterns should be interpreted as conditional on survival. The players who remain observable at older ages are a selected group, and this selection process differs by age, playing time, skill, and offensive archetype.
