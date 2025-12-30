source("datacleaning.R")

# ==============================================================================
# 1. GLOBAL SETTINGS & DEFINITIONS
# ==============================================================================

likert_levels <- c(
  "Strongly Disagree",
  "Somewhat Disagree",
  "Neither Disagree nor Agree",
  "Somewhat Agree",
  "Strongly Agree"
)

# --- Variable Sets & Labels ---

# Community 1
community_vars_1 <- c("sense_of_belonging", "safe_on_campus", "sense_of_community", 
                      "close_friends", "work_life", "phys_activity", 
                      "approach_strangers", "place_for_me")
community_labels_1 <- c(
  sense_of_belonging = "Feel a Sense of Belonging",
  safe_on_campus     = "Feel Safe on Campus",
  sense_of_community = "Feel Part of a Community",
  close_friends      = "Have Close Friends",
  work_life          = "Manage Work-Life Balance",
  phys_activity      = "Get Enough Physical Activity",
  approach_strangers = "Comfortable Approaching Strangers",
  place_for_me       = "University is a Place for Me"
)

# Community 2
community_vars_2 <- c("lonely", "hard_make_friends", "time_alone", 
                      "plans_with_friends", "regret_ubc", "feel_judged", 
                      "low_mood", "anxious")
community_labels_2 <- c(
  lonely             = "Feel Lonely",
  hard_make_friends  = "Hard to Make Friends",
  time_alone         = "Spend a Lot of Time Alone",
  plans_with_friends = "Make Plans with Friends",
  regret_ubc         = "Regret Choosing UBC",
  feel_judged        = "Feel Judged by Others",
  low_mood           = "Low Mood",
  anxious            = "Anxious"
)

# Society
society_vars <- c("constituency_events", "constituency_resource", 
                  "constituency_program", "constituency_identity", 
                  "constituency_belonging")
society_labels <- c(
  constituency_events    = "Attend Society Events",
  constituency_resource  = "Use Society Resources",
  constituency_program   = "Know What Programming Is Available",
  constituency_identity  = "Society Is Part of My Identity",
  constituency_belonging = "Feel a Sense of Belonging in My Society"
)

# Events
events_vars <- c("events_safe", "events_belong", "events_worth", 
                 "events_friends", "more_sporting", "more_parties", 
                 "alc_important", "food_important")
events_labels <- c(
  events_safe    = "Felt Safe at Campus Events",
  events_belong  = "Felt a Sense of Belonging at Events",
  events_worth   = "Events Are Worth My Time",
  events_friends = "Make Friends at Events",
  more_sporting  = "Would Attend More Sporting Events",
  more_parties   = "Would Attend More Parties",
  alc_important  = "Alcohol Is Important at Events",
  food_important = "Food Is Important at Events"
)

# Merch
merch_vars <- c("merch_belonging", "merch_campus", "merch_off_campus", 
                "merch_access", "blue_gold", "tbird_pride", "ubc_pride")
merch_labels <- c(
  merch_belonging  = "Merchandise Increases Belonging",
  merch_campus     = "Wear UBC Merch on Campus",
  merch_off_campus = "Wear UBC Merch Off Campus",
  merch_access     = "Lower Cost Would Increase Use",
  blue_gold        = "Feel Connected to Blue & Gold Branding",
  tbird_pride      = "Proud to Be a Thunderbird",
  ubc_pride        = "Proud to Associate with UBC"
)

# AMS
ams_vars <- c("ams_awareness", "ams_impact", "ams_best_interest", 
              "ams_accountable", "ams_budget", "ams_good_job")
ams_labels <- c(
  ams_awareness      = "Know What the AMS Does",
  ams_impact         = "AMS Work Impacts Me",
  ams_best_interest  = "AMS Has Students’ Best Interests in Mind",
  ams_accountable    = "AMS Keeps UBC Accountable",
  ams_budget         = "Trust AMS Budget Management",
  ams_good_job       = "AMS Does a Good Job Overall"
)

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================

#' Create a stacked Likert bar chart
#' @param data The dataframe to plot
#' @param vars Vector of column names
#' @param labels Named vector of question labels
#' @param palette RColorBrewer palette name
make_likert_plot <- function(data, vars, labels, palette = "RdBu") {
  
  df_long <- data |>
    pivot_longer(
      cols = all_of(vars),
      names_to = "question",
      values_to = "response"
    ) |>
    filter(!is.na(response)) |>
    mutate(
      response = factor(response, levels = likert_levels),
      question = factor(question, levels = vars, labels = labels[vars])
    ) |>
    count(question, response) |>
    group_by(question) |>
    mutate(prop = n / sum(n))
  
  ggplot(df_long, aes(x = prop, y = question, fill = response)) +
    geom_col() +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette = palette, direction = 1) +
    labs(x = "Proportion", y = NULL, fill = "Response") +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box.just = "center"
    )
}

#' Perform Mann-Whitney U Tests between two groups
#' @param df_a Dataframe for Group A
#' @param df_b Dataframe for Group B
#' @param vars Vector of column names to test
#' @param labels Named vector of question labels
#' @param name_a Name of Group A (for output table)
#' @param name_b Name of Group B (for output table)
run_mw_comparison <- function(df_a, df_b, vars, labels, name_a, name_b) {
  
  results <- tibble(
    question = character(),
    median_a = numeric(),
    median_b = numeric(),
    p_value = numeric(),
    significance = character(),
    higher_group = character()
  )
  
  # Update column names based on group names provided
  col_median_a <- paste0(str_to_lower(str_replace_all(name_a, " ", "_")), "_median")
  col_median_b <- paste0(str_to_lower(str_replace_all(name_b, " ", "_")), "_median")
  
  for (q in vars) {
    # Extract and convert to numeric based on likert_levels index
    x <- df_a |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
    y <- df_b |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
    
    # Calculate stats
    med_x <- median(x, na.rm = TRUE)
    med_y <- median(y, na.rm = TRUE)
    
    test <- wilcox.test(x, y)
    
    # Determine directionality via mean ranks
    combined <- c(x, y)
    ranks <- rank(combined)
    mean_rank_x <- mean(ranks[1:length(x)])
    mean_rank_y <- mean(ranks[(length(x) + 1):length(combined)])
    
    higher <- case_when(
      mean_rank_x > mean_rank_y ~ paste(name_a, "higher"),
      mean_rank_y > mean_rank_x ~ paste(name_b, "higher"),
      TRUE ~ "Equal"
    )
    
    # Add row
    results <- results |> add_row(
      question = labels[[q]],
      median_a = med_x,
      median_b = med_y,
      p_value = test$p.value,
      significance = ifelse(test$p.value < 0.05, "*", ""),
      higher_group = higher
    )
  }
  
  # Rename columns dynamically to match user preference
  results <- results |> 
    rename(!!col_median_a := median_a, !!col_median_b := median_b)
  
  return(results)
}

# ==============================================================================
# 3. DATA SUBSET CREATION
# ==============================================================================

# -- Base Filters --
# Variables earlier in the survey use attention_SD
df_sd_checked <- df |> filter(attention_SD == "Strongly Disagree")
# Variables later in the survey use attention_SWA
df_swa_checked <- df |> filter(attention_SWA == "Somewhat Agree")

# -- Group Subsets (Based on attention_SD) --
df_sd_fy    <- df_sd_checked |> filter(year_level == 1)
df_sd_notfy <- df_sd_checked |> filter(year_level != 1)

df_sd_disabled    <- df_sd_checked |> filter(!disabilities %in% c("J. None", "K. Prefer Not to Say"))
df_sd_nondisabled <- df_sd_checked |> filter(disabilities == "J. None")

df_sd_commuter    <- df_sd_checked |> filter(!str_detect(living_arrangement, "On campus"))
df_sd_resident    <- df_sd_checked |> filter(str_detect(living_arrangement, "On campus"))

# -- Group Subsets (Based on attention_SWA) --
df_swa_fy    <- df_swa_checked |> filter(year_level == 1)
df_swa_notfy <- df_swa_checked |> filter(year_level != 1)

df_swa_disabled    <- df_swa_checked |> filter(!disabilities %in% c("J. None", "K. Prefer Not to Say"))
df_swa_nondisabled <- df_swa_checked |> filter(disabilities == "J. None")

df_swa_commuter    <- df_swa_checked |> filter(!str_detect(living_arrangement, "On campus"))
df_swa_resident    <- df_swa_checked |> filter(str_detect(living_arrangement, "On campus"))

# ==============================================================================
# 4. PLOTS AND ANALYSIS
# ==============================================================================

### SECTION A: COMMUNITY VARS 1 & 2 (Using attention_SD)

# 1. Plots: All Students
p_comm1_all <- make_likert_plot(df_sd_checked, community_vars_1, community_labels_1, "RdBu")
p_comm2_all <- make_likert_plot(df_sd_checked, community_vars_2, community_labels_2, "RdBu")

# 2. Plots: First Year (PiYG)
p_comm1_fy <- make_likert_plot(df_sd_fy, community_vars_1, community_labels_1, "PiYG")
print(p_comm1_fy + p_comm1_all) # Patchwork display

# 3. MW Test: First Year vs Non-First Year
mw_comm1_fy <- run_mw_comparison(df_sd_fy, df_sd_notfy, community_vars_1, community_labels_1, "First-year", "Non-first-year")
mw_comm2_fy <- run_mw_comparison(df_sd_fy, df_sd_notfy, community_vars_2, community_labels_2, "First-year", "Non-first-year")

# 4. Plots: Disabled (PuOr)
p_comm1_dis <- make_likert_plot(df_sd_disabled, community_vars_1, community_labels_1, "PuOr")
p_comm2_dis <- make_likert_plot(df_sd_disabled, community_vars_2, community_labels_2, "PuOr")
print(p_comm1_dis + p_comm1_all)
print(p_comm2_dis + p_comm2_all)

# 5. MW Test: Disabled vs Non-Disabled
mw_comm1_dis <- run_mw_comparison(df_sd_disabled, df_sd_nondisabled, community_vars_1, community_labels_1, "Disabled", "Non-disabled")
mw_comm2_dis <- run_mw_comparison(df_sd_disabled, df_sd_nondisabled, community_vars_2, community_labels_2, "Disabled", "Non-disabled")

# 6. Plots: Commuter (YlGnBu)
p_comm1_com <- make_likert_plot(df_sd_commuter, community_vars_1, community_labels_1, "YlGnBu")
p_comm2_com <- make_likert_plot(df_sd_commuter, community_vars_2, community_labels_2, "YlGnBu")
print(p_comm1_com + p_comm1_all)
print(p_comm2_com + p_comm2_all)

# 7. MW Test: Commuter vs Resident
mw_comm1_com <- run_mw_comparison(df_sd_commuter, df_sd_resident, community_vars_1, community_labels_1, "Commuters", "Non-commuters")
mw_comm2_com <- run_mw_comparison(df_sd_commuter, df_sd_resident, community_vars_2, community_labels_2, "Commuters", "Non-commuters")


### SECTION B: SOCIETY (Using attention_SD)

p_soc_all <- make_likert_plot(df_sd_checked, society_vars, society_labels, "RdBu")
p_soc_fy  <- make_likert_plot(df_sd_fy, society_vars, society_labels, "PiYG")
p_soc_dis <- make_likert_plot(df_sd_disabled, society_vars, society_labels, "PuOr")

print(p_soc_fy + p_soc_all)
print(p_soc_dis + p_soc_all)

mw_soc_fy  <- run_mw_comparison(df_sd_fy, df_sd_notfy, society_vars, society_labels, "First-year", "Non-first-year")
mw_soc_dis <- run_mw_comparison(df_sd_disabled, df_sd_nondisabled, society_vars, society_labels, "Disabled", "Non-disabled")


### SECTION C: EVENTS (Using attention_SWA)

p_event_all <- make_likert_plot(df_swa_checked, events_vars, events_labels, "RdBu")
p_event_fy  <- make_likert_plot(df_swa_fy, events_vars, events_labels, "PiYG")
p_event_dis <- make_likert_plot(df_swa_disabled, events_vars, events_labels, "PuOr")
p_event_com <- make_likert_plot(df_swa_commuter, events_vars, events_labels, "YlGnBu")

print(p_event_fy + p_event_all)
print(p_event_dis + p_event_all)
print(p_event_com + p_event_all)

mw_event_fy  <- run_mw_comparison(df_swa_fy, df_swa_notfy, events_vars, events_labels, "First-year", "Non-first-year")
mw_event_dis <- run_mw_comparison(df_swa_disabled, df_swa_nondisabled, events_vars, events_labels, "Disabled", "Non-disabled")
mw_event_com <- run_mw_comparison(df_swa_commuter, df_swa_resident, events_vars, events_labels, "Commuters", "Non-commuters")


### SECTION D: MERCH (Using attention_SWA)

p_merch_all <- make_likert_plot(df_swa_checked, merch_vars, merch_labels, "RdBu")
p_merch_fy  <- make_likert_plot(df_swa_fy, merch_vars, merch_labels, "PiYG")

print(p_merch_fy + p_merch_all)

mw_merch_fy <- run_mw_comparison(df_swa_fy, df_swa_notfy, merch_vars, merch_labels, "First-year", "Non-first-year")


### SECTION E: AMS (Using attention_SWA)

p_ams_all <- make_likert_plot(df_swa_checked, ams_vars, ams_labels, "RdBu")
p_ams_fy  <- make_likert_plot(df_swa_fy, ams_vars, ams_labels, "PiYG")

print(p_ams_fy + p_ams_all)

mw_ams_fy <- run_mw_comparison(df_swa_fy, df_swa_notfy, ams_vars, ams_labels, "First-year", "Non-first-year")

# ==============================================================================
# 5. OUTPUT DISPLAY (Example of printing result tables)
# ==============================================================================

# Print Community 1 Results
print(mw_comm1_fy)
print(mw_comm1_dis)
print(mw_comm1_com)

# Print Society Results
print(mw_soc_fy)
print(mw_soc_dis)

# Print Events Results
print(mw_event_fy)
print(mw_event_dis)
print(mw_event_com)