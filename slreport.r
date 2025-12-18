
#TO-DO
#CHOOSE HOW TO FACTORIZE LIKERT SCALES
#non-attention filter
#LIKERT GRAPHS
#SUMMARY STATS
#HYPOTHESIS TESTS + GRAPHS
#FIGURE OUT REPO SETUP... commits... 


source("datacleaning.R")
source("summarygraphs.R")
glimpse(df)
commute_distribution 
print(positive_cloud + negative_cloud)



#likert scale graphs

# Likert scale and numeric mapping


likert_levels <- c(
  "Strongly Disagree",
  "Somewhat Disagree",
  "Neither Disagree nor Agree",
  "Somewhat Agree",
  "Strongly Agree")

# variables (exclude attention_SD from plotting)
community_vars_1 <- c(
  "sense_of_belonging",
  "safe_on_campus",
  "sense_of_community",
  "close_friends",
  "work_life",
  "phys_activity",
  "approach_strangers",
  "place_for_me"
)

# Human-readable labels
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

# Filter respondents who passed attention check
df_filtered <- df |> 
  filter(attention_SD == "Strongly Disagree")

# Transform to long format
df_long_1 <- df_filtered |>
  pivot_longer(
    cols = all_of(community_vars_1),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(response, levels = likert_levels),
    question = factor(question, levels = community_vars_1, labels = community_labels_1[community_vars_1])
  ) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

# Plot using Brewer palette
likert_community_1 <- ggplot(df_long_1, aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = 1
  ) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13) 

likert_community_1


df_first_year <- df |> 
  filter(year_level == 1)
df_first_year 

# Transform to long format
df_long_1F <- df_first_year |>
  pivot_longer(
    cols = all_of(community_vars_1),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(response, levels = likert_levels),
    question = factor(question, levels = community_vars_1, labels = community_labels_1[community_vars_1])
  ) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

# Simple stacked Likert plot using scale_fill_brewer
likert_community_1F <- ggplot(df_long_1F, aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(
    palette = "PiYG", direction = 1  # lighter, softer colors for first-year plot?
  ) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_community_1F

print(likert_community_1F + likert_community_1)


community_vars_2 <- c(
  "lonely",
  "hard_make_friends",
  "time_alone",
  "plans_with_friends",
  "regret_ubc",
  "feel_judged",
  "low_mood",
  "anxious")

# Human-readable labels
community_labels_2 <- c(
  lonely = "Feel Lonely",
  hard_make_friends = "Hard to Make Friends",
  time_alone = "Spend a Lot of Time Alone",
  plans_with_friends = "Make Plans with Friends",
  regret_ubc = "Regret Choosing UBC",
  feel_judged = "Feel Judged by Others",
  low_mood = "Low Mood",
  anxious = "Anxious")

df_long_2 <- df |>
  pivot_longer(
    cols = all_of(community_vars_2),
    names_to = "question",
    values_to = "response") |> filter(!is.na(response)) |>  # remove NAs
  mutate(response = factor(response, levels = likert_levels),
    question = factor(question, levels = community_vars_2, labels = community_labels_2[community_vars_2])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

# Plot using a red-to-blue Brewer palette
likert_community_2 <- ggplot(df_long_2, aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",   # center horizontally
    legend.box.just = "center")         # ensures box itself is centered

likert_community_2

#Significance testing

# Filter attention-checked students
df_checked <- df |> filter(attention_SD == "Strongly Disagree")

# Initialize results tibble for Community 1
community_1_mw_results <- tibble(
  question = character(),
  first_year_median = numeric(),
  non_first_median = numeric(),
  p_value = numeric(),
  significance = character(),
  higher_group = character()
)

# Loop through community_vars_1
for(q in community_vars_1) {
  
  # Convert responses to numeric
  x <- df_checked |> filter(year_level == 1) |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  y <- df_checked |> filter(year_level != 1) |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  
  # Compute medians (keep for reference)
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  # Mann-Whitney U test
  test <- wilcox.test(x, y)
  
  # Compute mean ranks for directionality
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x)+1):length(combined)])
  
  higher <- case_when(
    mean_rank_x > mean_rank_y ~ "First-year higher",
    mean_rank_y > mean_rank_x ~ "Non-first-year higher",
    TRUE ~ "Equal"
  )
  
  # Add to results
  community_1_mw_results <- community_1_mw_results |> add_row(
    question = community_labels_1[[q]],
    first_year_median = med_x,
    non_first_median = med_y,
    p_value = test$p.value,
    significance = ifelse(test$p.value < 0.05, "*", ""),
    higher_group = higher
  )
}

community_1_mw_results


# Initialize results tibble for Community 1
community_2_mw_results <- tibble(
  question = character(),
  first_year_median = numeric(),
  non_first_median = numeric(),
  p_value = numeric(),
  significance = character(),
  higher_group = character()
)

# Loop through community_vars_1
for(q in community_vars_2) {
  
  # Convert responses to numeric
  x <- df_checked |> filter(year_level == 1) |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  y <- df_checked |> filter(year_level != 1) |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  
  # Compute medians (keep for reference)
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  # Mann-Whitney U test
  test <- wilcox.test(x, y)
  
  # Compute mean ranks for directionality
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x)+1):length(combined)])
  
  higher <- case_when(
    mean_rank_x > mean_rank_y ~ "First-year higher",
    mean_rank_y > mean_rank_x ~ "Non-first-year higher",
    TRUE ~ "Equal"
  )
  
  # Add to results
  community_2_mw_results <- community_2_mw_results |> add_row(
    question = community_labels_2[[q]],
    first_year_median = med_x,
    non_first_median = med_y,
    p_value = test$p.value,
    significance = ifelse(test$p.value < 0.05, "*", ""),
    higher_group = higher)}


#RESULTS
community_1_mw_results
community_2_mw_results









