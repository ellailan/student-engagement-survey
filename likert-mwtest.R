source("datacleaning.R")


#likert scale graphs
# Likert scale and numeric mapping

likert_levels <- c(
  "Strongly Disagree",
  "Somewhat Disagree",
  "Neither Disagree nor Agree",
  "Somewhat Agree",
  "Strongly Agree")

# variables (exclude attention_SD from plottingx)
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




society_vars <- c("constituency_events",
                  "constituency_resource",
                  "constituency_program",
                  "constituency_identity",
                  "constituency_belonging")

society_labels <- c(constituency_events    = "Attend Society Events",
                    constituency_resource  = "Use Society Resources",
                    constituency_program   = "Know What Programming Is Available",
                    constituency_identity  = "Society Is Part of My Identity",
                    constituency_belonging = "Feel a Sense of Belonging in My Society")

df_checked <- df |> filter(attention_SD == "Strongly Disagree")

df_society_long <- df_checked |>
  pivot_longer(cols = all_of(society_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = society_vars,
                           labels = society_labels[society_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_society_all <- ggplot(df_society_long,
                             aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_society_all

df_society_long_FY <- df_checked |>
  filter(year_level == 1) |>
  pivot_longer(cols = all_of(society_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = society_vars,
                           labels = society_labels[society_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_society_FY <- ggplot(df_society_long_FY,
                            aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PiYG", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_society_FY

likert_society_FY + likert_society_all


society_mw_results <- tibble(question = character(),
                             first_year_median = numeric(),
                             non_first_median = numeric(),
                             p_value = numeric(),
                             significance = character(),
                             higher_group = character())

for (q in society_vars) {
  
  x <- df_checked |>
    filter(year_level == 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  y <- df_checked |>
    filter(year_level != 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x) + 1):length(combined)])
  
  higher <- case_when(mean_rank_x > mean_rank_y ~ "First-year higher",
                      mean_rank_y > mean_rank_x ~ "Non-first-year higher",
                      TRUE ~ "Equal")
  
  society_mw_results <- society_mw_results |>
    add_row(question = society_labels[[q]],
            first_year_median = med_x,
            non_first_median = med_y,
            p_value = test$p.value,
            significance = ifelse(test$p.value < 0.05, "*", ""),
            higher_group = higher)
}

society_mw_results


events_vars <- c("events_safe",
                 "events_belong",
                 "events_worth",
                 "events_friends",
                 "more_sporting",
                 "more_parties",
                 "alc_important",
                 "food_important")

events_labels <- c(events_safe    = "Felt Safe at Campus Events",
                   events_belong  = "Felt a Sense of Belonging at Events",
                   events_worth   = "Events Are Worth My Time",
                   events_friends = "Make Friends at Events",
                   more_sporting  = "Would Attend More Sporting Events",
                   more_parties   = "Would Attend More Parties",
                   alc_important  = "Alcohol Is Important at Events",
                   food_important = "Food Is Important at Events")

df_checked_2 <- df |>
  filter(attention_SWA == "Somewhat Agree")

df_events_long <- df_checked_2 |>
  pivot_longer(cols = all_of(events_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = events_vars,
                           labels = events_labels[events_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_events_all <- ggplot(df_events_long,
                            aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

df_events_long_FY <- df_checked_2 |>
  filter(year_level == 1) |>
  pivot_longer(cols = all_of(events_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = events_vars,
                           labels = events_labels[events_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_events_FY <- ggplot(df_events_long_FY,
                           aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PiYG", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_events_FY + likert_events_all

events_mw_results <- tibble(question = character(),
                            first_year_median = numeric(),
                            non_first_median = numeric(),
                            p_value = numeric(),
                            significance = character(),
                            higher_group = character())

for (q in events_vars) {
  
  x <- df_checked_2 |>
    filter(year_level == 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  y <- df_checked_2 |>
    filter(year_level != 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x) + 1):length(combined)])
  
  higher <- case_when(mean_rank_x > mean_rank_y ~ "First-year higher",
                      mean_rank_y > mean_rank_x ~ "Non-first-year higher",
                      TRUE ~ "Equal")
  
  events_mw_results <- events_mw_results |>
    add_row(question = events_labels[[q]],
            first_year_median = med_x,
            non_first_median = med_y,
            p_value = test$p.value,
            significance = ifelse(test$p.value < 0.05, "*", ""),
            higher_group = higher)
}

events_mw_results

merch_vars <- c("merch_belonging",
                "merch_campus",
                "merch_off_campus",
                "merch_access",
                "blue_gold",
                "tbird_pride",
                "ubc_pride")

merch_labels <- c(merch_belonging  = "Merchandise Increases Belonging",
                  merch_campus     = "Wear UBC Merch on Campus",
                  merch_off_campus = "Wear UBC Merch Off Campus",
                  merch_access     = "Lower Cost Would Increase Use",
                  blue_gold        = "Feel Connected to Blue & Gold Branding",
                  tbird_pride      = "Proud to Be a Thunderbird",
                  ubc_pride        = "Proud to Associate with UBC")

df_merch_long <- df_checked_2 |>
  pivot_longer(cols = all_of(merch_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = merch_vars,
                           labels = merch_labels[merch_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_merch_all <- ggplot(df_merch_long,
                           aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

df_merch_long_FY <- df_checked_2 |>
  filter(year_level == 1) |>
  pivot_longer(cols = all_of(merch_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = merch_vars,
                           labels = merch_labels[merch_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_merch_FY <- ggplot(df_merch_long_FY,
                          aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PiYG", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_merch_FY + likert_merch_all

merch_mw_results <- tibble(question = character(),
                           first_year_median = numeric(),
                           non_first_median = numeric(),
                           p_value = numeric(),
                           significance = character(),
                           higher_group = character())

for (q in merch_vars) {
  
  x <- df_checked_2 |>
    filter(year_level == 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  y <- df_checked_2 |>
    filter(year_level != 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x) + 1):length(combined)])
  
  higher <- case_when(mean_rank_x > mean_rank_y ~ "First-year higher",
                      mean_rank_y > mean_rank_x ~ "Non-first-year higher",
                      TRUE ~ "Equal")
  
  merch_mw_results <- merch_mw_results |>
    add_row(question = merch_labels[[q]],
            first_year_median = med_x,
            non_first_median = med_y,
            p_value = test$p.value,
            significance = ifelse(test$p.value < 0.05, "*", ""),
            higher_group = higher)
}

merch_mw_results

ams_vars <- c("ams_awareness",
              "ams_impact",
              "ams_best_interest",
              "ams_accountable",
              "ams_budget",
              "ams_good_job")

ams_labels <- c(ams_awareness      = "Know What the AMS Does",
                ams_impact         = "AMS Work Impacts Me",
                ams_best_interest  = "AMS Has Students’ Best Interests in Mind",
                ams_accountable    = "AMS Keeps UBC Accountable",
                ams_budget         = "Trust AMS Budget Management",
                ams_good_job       = "AMS Does a Good Job Overall")

df_ams_long <- df_checked_2 |>
  pivot_longer(cols = all_of(ams_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = ams_vars,
                           labels = ams_labels[ams_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_ams_all <- ggplot(df_ams_long,
                         aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "RdBu", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

df_ams_long_FY <- df_checked_2 |>
  filter(year_level == 1) |>
  pivot_longer(cols = all_of(ams_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(response = factor(response, levels = likert_levels),
         question = factor(question,
                           levels = ams_vars,
                           labels = ams_labels[ams_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_ams_FY <- ggplot(df_ams_long_FY,
                        aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PiYG", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_ams_FY + likert_ams_all

ams_mw_results <- tibble(question = character(),
                         first_year_median = numeric(),
                         non_first_median = numeric(),
                         p_value = numeric(),
                         significance = character(),
                         higher_group = character())

for (q in ams_vars) {
  
  x <- df_checked_2 |>
    filter(year_level == 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  y <- df_checked_2 |>
    filter(year_level != 1) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x) + 1):length(combined)])
  
  higher <- case_when(mean_rank_x > mean_rank_y ~ "First-year higher",
                      mean_rank_y > mean_rank_x ~ "Non-first-year higher",
                      TRUE ~ "Equal")
  
  ams_mw_results <- ams_mw_results |>
    add_row(question = ams_labels[[q]],
            first_year_median = med_x,
            non_first_median = med_y,
            p_value = test$p.value,
            significance = ifelse(test$p.value < 0.05, "*", ""),
            higher_group = higher)
}

ams_mw_results



df

df_disabled <- df_checked |> 
  filter(!disabilities %in% c("J. None", "K. Prefer Not to Say"))

df_disabled

df_non_disabled <- df_checked |> 
  filter(disabilities == "J. None")

df_long_1D <- df_disabled |>
  pivot_longer(
    cols = all_of(community_vars_1),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(response, levels = likert_levels),
    question = factor(question,
                      levels = community_vars_1,
                      labels = community_labels_1[community_vars_1])
  ) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_community_1D <- ggplot(df_long_1D,
                              aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_community_1D + likert_community_1

df_long_2D <- df_disabled |>
  pivot_longer(
    cols = all_of(community_vars_2),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(response, levels = likert_levels),
    question = factor(
      question,
      levels = community_vars_2,
      labels = community_labels_2[community_vars_2]
    )
  ) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_community_2D <- ggplot(
  df_long_2D,
  aes(x = prop, y = question, fill = response)
) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_community_2D + likert_community_2


community_1_mw_disabled <- tibble(
  question = character(),
  disabled_median = numeric(),
  non_disabled_median = numeric(),
  p_value = numeric(),
  significance = character(),
  higher_group = character()
)

for (q in community_vars_1) {
  
  x <- df_disabled |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  y <- df_non_disabled |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  
  med_x <- median(x, na.rm = TRUE)
  med_y <- median(y, na.rm = TRUE)
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  mean_rank_x <- mean(ranks[1:length(x)])
  mean_rank_y <- mean(ranks[(length(x) + 1):length(combined)])
  
  higher <- case_when(
    mean_rank_x > mean_rank_y ~ "Disabled higher",
    mean_rank_y > mean_rank_x ~ "Non-disabled higher",
    TRUE ~ "Equal"
  )
  
  community_1_mw_disabled <- community_1_mw_disabled |>
    add_row(
      question = community_labels_1[[q]],
      disabled_median = med_x,
      non_disabled_median = med_y,
      p_value = test$p.value,
      significance = ifelse(test$p.value < 0.05, "*", ""),
      higher_group = higher
    )
}

community_1_mw_disabled

community_2_mw_disabled <- tibble(
  question = character(),
  disabled_median = numeric(),
  non_disabled_median = numeric(),
  p_value = numeric(),
  significance = character(),
  higher_group = character()
)

for (q in community_vars_2) {
  
  x <- df_disabled |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  y <- df_non_disabled |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  
  community_2_mw_disabled <- community_2_mw_disabled |>
    add_row(
      question = community_labels_2[[q]],
      disabled_median = median(x, na.rm = TRUE),
      non_disabled_median = median(y, na.rm = TRUE),
      p_value = test$p.value,
      significance = ifelse(test$p.value < 0.05, "*", ""),
      higher_group = case_when(
        mean(ranks[1:length(x)]) > mean(ranks[(length(x)+1):length(combined)]) ~ "Disabled higher",
        TRUE ~ "Non-disabled higher"
      )
    )
}


community_1_mw_disabled
community_2_mw_disabled




df_society_long_D <- df_disabled |>
  pivot_longer(cols = all_of(society_vars),
               names_to = "question",
               values_to = "response") |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(response, levels = likert_levels),
    question = factor(question,
                      levels = society_vars,
                      labels = society_labels[society_vars])
  ) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_society_D <- ggplot(df_society_long_D,
                           aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_society_D + likert_society_all

society_mw_disabled <- tibble(
  question = character(),
  disabled_median = numeric(),
  non_disabled_median = numeric(),
  p_value = numeric(),
  significance = character(),
  higher_group = character()
)

for (q in society_vars) {
  
  x <- df_disabled |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  y <- df_non_disabled |> pull(q) |> factor(levels = likert_levels) |> as.numeric()
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  
  society_mw_disabled <- society_mw_disabled |>
    add_row(
      question = society_labels[[q]],
      disabled_median = median(x, na.rm = TRUE),
      non_disabled_median = median(y, na.rm = TRUE),
      p_value = test$p.value,
      significance = ifelse(test$p.value < 0.05, "*", ""),
      higher_group = case_when(
        mean(ranks[1:length(x)]) > mean(ranks[(length(x)+1):length(combined)]) ~ "Disabled higher",
        TRUE ~ "Non-disabled higher"
      )
    )
}

society_mw_disabled

df_events_long_D <- df_checked_2 |>
  filter(!disabilities %in% c("J. None", "K. Prefer Not to Say")) |>
  pivot_longer(
    cols = all_of(events_vars),
    names_to = "question",
    values_to = "response"
  ) |>
  filter(!is.na(response)) |>
  mutate(
    response = factor(response, levels = likert_levels),
    question = factor(
      question,
      levels = events_vars,
      labels = events_labels[events_vars]
    )
  ) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

likert_events_D <- ggplot(
  df_events_long_D,
  aes(x = prop, y = question, fill = response)
) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "PuOr", direction = 1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13)

likert_events_D + likert_events_all


events_mw_disabled <- tibble(
  question = character(),
  disabled_median = numeric(),
  non_disabled_median = numeric(),
  p_value = numeric(),
  significance = character(),
  higher_group = character()
)

for (q in events_vars) {
  
  x <- df_checked_2 |>
    filter(!disabilities %in% c("J. None", "K. Prefer Not to Say")) |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  y <- df_checked_2 |>
    filter(disabilities == "J. None") |>
    pull(q) |>
    factor(levels = likert_levels) |>
    as.numeric()
  
  test <- wilcox.test(x, y)
  
  combined <- c(x, y)
  ranks <- rank(combined)
  
  events_mw_disabled <- events_mw_disabled |>
    add_row(
      question = events_labels[[q]],
      disabled_median = median(x, na.rm = TRUE),
      non_disabled_median = median(y, na.rm = TRUE),
      p_value = test$p.value,
      significance = ifelse(test$p.value < 0.05, "*", ""),
      higher_group = case_when(
        mean(ranks[1:length(x)]) > mean(ranks[(length(x) + 1):length(combined)]) ~ "Disabled higher",
        TRUE ~ "Non-disabled higher"
      )
    )
}

events_mw_disabled





