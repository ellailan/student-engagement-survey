
library(wesanderson)
library(likert)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggwordcloud)
library(tidytext)
library(patchwork)
library(here)

#TO-DO
#CHOOSE HOW TO FACTORIZE LIKERT SCALES
#non-attention filter
#LIKERT GRAPHS
#SUMMARY STATS
#HYPOTHESIS TESTS + GRAPHS
#FIGURE OUT REPO SETUP... commits... 


df <- read_csv("sesdata.csv")

#removing extra data added by Qualtrics when downloading csv, including
#IP address and leftover vars from testing of survey
df <- df[-c(1,2), -c(1:17)]

#renaming variables to correspond with their associated question, see list of survey questions in repo??
#currently does not include names for written textbox questions!

df <- df |>
  rename(
    student_status      = Q2,
    student_status_other = Q2_6_TEXT,
    isaffiliate         = Q3,
    affiliate_school    = Q4,
    student_society     = Q5,
    extracurriculars    = Q6,
    extracurricular_other = Q6_32_TEXT, 
    employment          = Q7,
    living_arrangement  = Q8,
    living_other = Q8_8_TEXT, 
    commute_time        = Q9,
    social_attitude     = Q10,
    weekend_activities  = Q11,
    age                 = Q12,
    year_level          = Q13,
    gender              = Q14,
    gender_other = Q14_6_TEXT, 
    lgbtq               = Q15,
    ethnicity           = Q16,
    ethnicity_other = Q16_11_TEXT,
    transfer            = Q17,
    international       = Q18,
    disabilities        = Q19,
    disab_other = Q19_11_TEXT,
    disab_diagnosis     = Q20,
    diagnosis_barrier   = Q21,

    sense_of_belonging  = Q22_1,
    safe_on_campus      = Q22_2,
    sense_of_community  = Q22_3,
    close_friends       = Q22_4,
    work_life           = Q22_5,
    phys_activity       = Q22_6,
    attention_SD        = Q22_7, #attention check, should be strongly disagree
    approach_strangers  = Q22_8, 
    place_for_me        = Q22_9, 
    
    lonely              = Q23_1,
    hard_make_friends   = Q23_2, 
    time_alone          = Q23_3, 
    plans_with_friends  = Q23_4, 
    regret_ubc          = Q23_5, 
    feel_judged         = Q23_6,
    low_mood          = Q23_7,
    anxious = Q23_8,
    
    one_word_student_life = Q24,
    constituency_events = Q25_1,
    constituency_resource = Q25_2,
    constituency_program = Q25_3,
    constituency_identity = Q25_4,
    constituency_belonging = Q25_5,
    events_safe = Q26_1,
    events_belong = Q26_2,
    events_worth = Q26_3,
    events_friends = Q26_4, 
    more_sporting = Q26_5, 
    more_parties = Q26_6,
    attention_SWA = Q26_7, #attention check, should be somewhat agree
    alc_important = Q26_8,
    food_important = Q26_9,
    
    recent_event_feedback = Q27,
    non_attend_reasons = Q28,
    non_attend_other = Q28_11_TEXT,
    accessibility_events = Q29,
    
    ubc_merch = Q30,
    merch_belonging = Q31_1,
    merch_campus = Q31_2,
    merch_off_campus = Q31_3,
    merch_access = Q31_4,
    blue_gold = Q31_5,
    tbird_pride = Q31_6,
    ubc_pride = Q31_7,
    
    ams_awareness = Q32_1,
    ams_impact = Q32_2,
    ams_best_interest = Q32_3,
    ams_accountable = Q32_4, 
    ams_budget = Q32_5, 
    ams_good_job = Q32_6, 
    
    lead_to_survey = Q33,
    lead_to_other = Q33_10_TEXT
    
  )


glimpse(df)
#converting age, year, into integer values
df <- df |> mutate(commute_time = as.numeric(commute_time))
df <- df |> mutate(age = as.numeric(age))
df <- df |> mutate(year_level = as.numeric(year_level)) 


#only drop NAs when needed... 
commute <- df |> select(commute_time) |> drop_na()
#histogram of distribution time
commute_distribution <- ggplot(commute, aes(x = commute_time)) +
  geom_histogram(
    bins = 3,
    color = "white",
    fill = "skyblue"
  ) +
  theme_minimal(base_size = 10) +
  labs(
    title = "Commute Time Distribution",
    x = "Commute Time (minutes)",
    y = "Count"
  )


commute_distribution 

#generate a word-cloud with descriptions of student life
bing <- get_sentiments("bing")

#one cloud with colored tags; positive, neutral, negative
word_cloud <- df |>
  select(one_word_student_life) |>
  unnest_tokens(word, one_word_student_life) |>
  left_join(bing, by = "word") |>
  mutate(sentiment = replace_na(sentiment, "neutral")) |>
  count(word, sentiment, sort = TRUE) |>
  ggplot(aes(label = word, size = n, color = sentiment)) +
  geom_text_wordcloud(rm_outside = TRUE) +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = c(
    positive = "steelblue",
    negative = "tomato",
    neutral  = "grey60")) +
  theme_minimal()

word_cloud

#dataframe with sentiments of words
sent_words <- df |>
  select(one_word_student_life) |>
  unnest_tokens(word, one_word_student_life) |>
  inner_join(bing, by = "word") |>
  count(word, sentiment, sort = TRUE)

#count per sentiment
sent_counts <- sent_words |> count(sentiment)
sent_counts

#cloud with only positive words
positive_cloud <- sent_words |>
  filter(sentiment == "positive") |>
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, family = "Futura") +
  scale_size_area(max_size = 12) +
  scale_color_gradient(low = "#4586ff", high = "#00379e") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#d9edff", color = NA))

#cloud with only negative words
negative_cloud <- sent_words |>
  filter(sentiment == "negative") |>
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, family = "Futura") +
  scale_size_area(max_size = 12) +
  scale_color_gradient(low = "#ff4d4d", high = "#a83232") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#fad4d2", color = NA))

#print positive, negative clouds side by side
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
    direction = -1
  ) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center")

likert_community_1

community_2_vars <- c(
  "lonely",
  "hard_make_friends",
  "time_alone",
  "plans_with_friends",
  "regret_ubc",
  "feel_judged",
  "low_mood",
  "anxious")

# Human-readable labels
community_2_labels <- c(
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
    cols = all_of(community_2_vars),
    names_to = "question",
    values_to = "response") |> filter(!is.na(response)) |>  # remove NAs
  mutate(response = factor(response, levels = likert_levels),
    question = factor(question, levels = community_2_vars, labels = community_2_labels[community_2_vars])) |>
  count(question, response) |>
  group_by(question) |>
  mutate(prop = n / sum(n))

# Plot using a red-to-blue Brewer palette
likert_community_2 <- ggplot(df_long_2, aes(x = prop, y = question, fill = response)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = -1) +
  labs(x = "Proportion", y = NULL, fill = "Response") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",   # center horizontally
    legend.box.just = "center")         # ensures box itself is centered

likert_community_2



