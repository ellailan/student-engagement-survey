source("likert-mwtest.R")

#' Summarize Likert results with grouped percentages and means
#' @param data The dataframe to analyze
#' @param vars Vector of column names
#' @param labels Named vector of question labels
summarize_likert_results <- function(data, vars, labels) {
  
  data |>
    select(all_of(vars)) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "response"
    ) |>
    mutate(
      # Convert text to 1-5 scale
      val = as.numeric(factor(response, levels = likert_levels)),
      question = labels[variable]
    ) |>
    filter(!is.na(val)) |>
    group_by(question) |>
    summarise(
      n = n(),
      mean_score = round(mean(val), 2),
      # Grouping: 1 & 2 = Disagree, 3 = Neutral, 4 & 5 = Agree
      pct_disagree = round(sum(val %in% c(1, 2)) / n() * 100, 1),
      pct_neutral  = round(sum(val == 3) / n() * 100, 1),
      pct_agree    = round(sum(val %in% c(4, 5)) / n() * 100, 1),
      .groups = "drop"
    ) |>
    arrange(desc(mean_score))
}

#' Summarize Likert results for a specific community subset
#' @param group_df The specific subset dataframe (e.g., df_sd_fy)
#' @param vars Vector of column names (e.g., community_vars_1)
#' @param labels Named vector of question labels
summarize_group_results <- function(group_df, vars, labels) {
  
  # Reuse the logic to calculate grouped percentages and means
  group_df |>
    select(all_of(vars)) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "response"
    ) |>
    mutate(
      val = as.numeric(factor(response, levels = likert_levels)),
      question = labels[variable]
    ) |>
    filter(!is.na(val)) |>
    group_by(question) |>
    summarise(
      n = n(),
      mean_score = round(mean(val, na.rm = TRUE), 2),
      pct_disagree = round(sum(val %in% c(1, 2)) / n() * 100, 1),
      pct_neutral  = round(sum(val == 3) / n() * 100, 1),
      pct_agree    = round(sum(val %in% c(4, 5)) / n() * 100, 1),
      .groups = "drop"
    ) |>
    arrange(desc(mean_score))
}



# Run the summary for Community 1 variables
community_1_summary <- summarize_likert_results(
  data = df_sd_checked, 
  vars = community_vars_1, 
  labels = community_labels_1)

# View the resulting table
print(community_1_summary)

# Calculate summary for First Years on Community 1 questions
fy_community_1_summary <- summarize_group_results(
  group_df = df_sd_fy, 
  vars     = community_vars_1, 
  labels   = community_labels_1)

# View results
print(fy_community_1_summary)

dis_community_1_summary <- summarize_group_results(
  group_df = df_sd_disabled, 
  vars     = community_vars_1, 
  labels   = community_labels_1)
print(dis_community_1_summary)

part_community_1_summary <- summarize_group_results(
  group_df = df_sd_participant, 
  vars     = community_vars_1, 
  labels   = community_labels_1)
print(part_community_1_summary)


# Run the summary for Community 2 variables
community_2_summary <- summarize_likert_results(
  data = df_sd_checked, 
  vars = community_vars_2, 
  labels = community_labels_2)

print(community_2_summary)

dis_community_2_summary <- summarize_group_results(
  group_df = df_sd_disabled, 
  vars     = community_vars_2, 
  labels   = community_labels_2)
print(dis_community_2_summary)

part_community_2_summary <- summarize_group_results(
  group_df = df_sd_participant, 
  vars     = community_vars_2, 
  labels   = community_labels_2)
print(part_community_2_summary)

# General Population
society_summary <- summarize_likert_results(
  data = df_sd_checked, 
  vars = society_vars, 
  labels = society_labels)
print(society_summary)

# First Years
fy_society_summary <- summarize_group_results(
  group_df = df_sd_fy, 
  vars     = society_vars, 
  labels   = society_labels)
print(fy_society_summary)

dis_society_summary <- summarize_group_results(
  group_df = df_sd_disabled, 
  vars     = society_vars, 
  labels   = society_labels)
print(dis_society_summary)

part_society_summary <- summarize_group_results(
  group_df = df_sd_participant, 
  vars     = society_vars, 
  labels   = society_labels)
print(part_society_summary)

# General Population
events_summary <- summarize_likert_results(
  data = df_swa_checked, 
  vars = events_vars, 
  labels = events_labels)
print(events_summary)

dis_events_summary <- summarize_group_results(
  group_df = df_swa_disabled, 
  vars     = events_vars, 
  labels   = events_labels)
print(dis_events_summary)

part_events_summary <- summarize_group_results(
  group_df = df_swa_participant, 
  vars     = events_vars, 
  labels   = events_labels)
print(part_events_summary)

# First Years
fy_events_summary <- summarize_group_results(
  group_df = df_swa_fy, 
  vars     = events_vars, 
  labels   = events_labels)
print(fy_events_summary)

# General Population
merch_summary <- summarize_likert_results(
  data = df_swa_checked, 
  vars = merch_vars, 
  labels = merch_labels)
print(merch_summary)

dis_merch_summary <- summarize_group_results(
  group_df = df_swa_disabled, 
  vars     = merch_vars, 
  labels   = merch_labels)
print(dis_merch_summary)

part_merch_summary <- summarize_group_results(
  group_df = df_swa_participant, 
  vars     = merch_vars, 
  labels   = merch_labels)
print(part_merch_summary)

# First Years
fy_merch_summary <- summarize_group_results(
  group_df = df_swa_fy, 
  vars     = merch_vars, 
  labels   = merch_labels)
print(fy_merch_summary)

# General Population
ams_summary <- summarize_likert_results(
  data = df_swa_checked, 
  vars = ams_vars, 
  labels = ams_labels)
print(ams_summary)

# First Years
fy_ams_summary <- summarize_group_results(
  group_df = df_swa_fy, 
  vars     = ams_vars, 
  labels   = ams_labels)
print(fy_ams_summary)

dis_ams_summary <- summarize_group_results(
  group_df = df_swa_disabled, 
  vars     = ams_vars, 
  labels   = ams_labels)
print(dis_ams_summary)

part_ams_summary <- summarize_group_results(
  group_df = df_swa_participant, 
  vars     = ams_vars, 
  labels   = ams_labels)
print(part_ams_summary)
