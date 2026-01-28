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


#' Create a diverging stacked bar chart comparing groups
#' @param data The combined dataframe containing all groups
#' @param vars Vector of column names
#' @param labels Named vector of question labels
#' @param group_var The string name of the column used for grouping
#' @param palette RColorBrewer palette name
diverging_bar_graph <- function(data, vars, labels, group_var, palette = "RdBu") {
  
  # 1. Ensure all variables are factors with the global Likert levels
  for (v in vars) {
    data[[v]] <- factor(
      as.character(data[[v]]),
      levels = likert_levels
    )
  }
  
  # 2. Prepare items dataframe and apply labels
  items <- as.data.frame(data[, vars, drop = FALSE])
  colnames(items) <- labels[vars]
  
  # 3. Extract the grouping column as a factor
  grouping_factor <- factor(data[[group_var]])
  
  # 4. Create the grouped likert object
  likert_data <- likert::likert(items = items, grouping = grouping_factor)
  
  # 5. Plot with centering and grouped display
  plot(
    likert_data,
    centered = TRUE,
    wrap = 45
  ) +
    scale_fill_brewer(palette = palette, direction = 1) +
    theme_minimal(base_size = 13) +
    labs(fill = "Response Level") +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank()
    )
}

# 1. Prepare the comparison dataset
diverging_bar_graph <- function(data, vars, labels, group_var, palette = "RdBu") {
  plot_data <- data |> select(all_of(vars), all_of(group_var))
  
  # Ensure items are factors with correct levels
  for (v in vars) {
    plot_data[[v]] <- factor(as.character(plot_data[[v]]), levels = likert_levels)
  }
  
  items <- as.data.frame(plot_data[, vars, drop = FALSE])
  colnames(items) <- labels[vars]
  grouping_factor <- factor(plot_data[[group_var]])
  
  likert_data <- likert::likert(items = items, grouping = grouping_factor)
  
  plot(likert_data, centered = TRUE, wrap = 45) +
    scale_fill_brewer(palette = palette, direction = 1) +
    theme_minimal(base_size = 12) +
    labs(fill = "Response") +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 9))
}

# ==============================================================================
# 2. ANALYSIS EXECUTION
# ==============================================================================

# Define question set metadata
question_sets <- list(
  list(name = "Community 1", vars = community_vars_1, labels = community_labels_1, type = "sd"),
  list(name = "Community 2", vars = community_vars_2, labels = community_labels_2, type = "sd"),
  list(name = "Society",     vars = society_vars,     labels = society_labels,     type = "sd"),
  list(name = "Events",      vars = events_vars,      labels = events_labels,      type = "swa"),
  list(name = "Merch",       vars = merch_vars,       labels = merch_labels,       type = "swa"),
  list(name = "AMS",         vars = ams_vars,         labels = ams_labels,         type = "swa")
)

# Define target groups to analyze
target_groups <- list(
  list(label = "First Years",   sd_df = df_sd_fy,         swa_df = df_swa_fy,         pal = "PiYG"),
  list(label = "Disabled",      sd_df = df_sd_disabled,   swa_df = df_swa_disabled,   pal = "PuOr"),
  list(label = "Participants",  sd_df = df_sd_participant, swa_df = df_swa_participant, pal = "Greens")
)

# Run the full loop
for (grp in target_groups) {
  cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("STARTING ANALYSIS FOR GROUP:", grp$label, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  for (set in question_sets) {
    # 1. Select appropriate data frames based on the question set type
    base_df  <- if(set$type == "sd") df_sd_checked else df_swa_checked
    group_df <- if(set$type == "sd") grp$sd_df    else grp$swa_df
    
    # 2. Print Summary Table
    cat("\n--- Summary Table:", set$name, "(Group:", grp$label, ") ---\n")
    print(summarize_group_results(group_df, set$vars, set$labels))
    
    # 3. Generate and Print Comparison Plot
    df_comp <- bind_rows(
      base_df  |> mutate(comparison_group = "General Population"),
      group_df |> mutate(comparison_group = grp$label)
    )
    
    p <- diverging_bar_graph(df_comp, set$vars, set$labels, "comparison_group", palette = grp$pal) +
      labs(title = paste(set$name, "-", grp$label, "vs General Population"))
    
    print(p)
  }
}