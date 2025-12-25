source("datacleaning.R")

student_status <- df |> select(student_status) |> drop_na()


student_status_dist <- df |>
  select(student_status) |>
  drop_na() |>
  ggplot(aes(x = student_status, fill = student_status)) +
  geom_bar(alpha = 0.9) +
  scale_fill_manual(
    values = wesanderson::wes_palette(
      "Royal1",
      n = length(unique(df$student_status)),
      type = "continuous"
    )
  ) +
  labs(
    x = "Student Status",
    y = "Number of Respondents",
    title = "Student Status"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

student_status_dist


affiliate <- df |> select(isaffiliate) |> drop_na()

affiliate_dist <- ggplot(affiliate, aes(x = fct_infreq(isaffiliate), fill = isaffiliate)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  labs(
    title = "Affiliate Status Distribution",
    x = "Affiliate Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

affiliate_dist


affiliate_school<-  df |> select(affiliate_school) |> drop_na()
affiliate_school

affiliate_school_dist <- ggplot(affiliate_school, aes(x = fct_infreq(affiliate_school), fill = affiliate_school)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Affiliate School Distribution",
    x = "Affiliate School",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

affiliate_school_dist



#some numbers leftover from early survey design, testing... cutting them out here
faculty <- df |> select(student_society) |> drop_na() |> filter(!grepl("[0-9]", student_society))
faculty

faculty_dist <- ggplot(faculty, aes(x = fct_infreq(student_society), fill = student_society)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() + 
  labs(
    title = "Student Society Memberships",
    x = "Student Society",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

faculty_dist

extracurric <- df |> select(extracurriculars) |> drop_na() |>
  # Split multiple activities into separate rows
  separate_rows(extracurriculars, sep = ",\\s*") |> 
  mutate(extracurriculars = str_trim(extracurriculars)) |>  # Remove extra spaces
  filter(!grepl("[0-9]", extracurriculars)) #same numbers issue as faculty


extracurric

extracurric_dist <- ggplot(extracurric, 
                           aes(x = fct_relevel(fct_infreq(extracurriculars), "K. None"),
                               fill = extracurriculars)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Extracurricular Involvement",
    x = "Activity",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

extracurric_dist


employment_stat <- df |> select(employment) |> drop_na()

employment_stat

employment_stat_dist <- employment_stat |> ggplot(aes(x = fct_infreq(employment), fill = employment)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Employment Status Distribution", x = "Employment Status", y = "Count") +
  theme_minimal(base_size = 14)

employment_stat_distx




living_stat <- df |> select(living_arrangement) |> drop_na()

living_stat

living_stat_dist <- living_stat |> ggplot(aes(x = fct_infreq(living_arrangement), fill = living_arrangement)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Living Arrangement Distribution", x = "Living Arrangement", y = "Count") +
  theme_minimal(base_size = 14)

living_stat_dist


social <- df |> select(social_attitude) |> drop_na()
social

social_dist <- social |> ggplot(aes(x = fct_infreq(social_attitude), fill = social_attitude)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Social Attitude Distribution", x = "Social Attitude", y = "Count") +
  theme_minimal(base_size = 14)

social_dist


weekend <- df |> 
  select(weekend_activities) |> 
  drop_na() |> 
  # Split at A-J labels
  mutate(weekend_activities = str_split(weekend_activities, "(?=[A-J]\\.)")) |> 
  unnest(weekend_activities) |> 
  # Remove trailing commas
  mutate(weekend_activities = str_replace(weekend_activities, ",$", "")) |>
  filter(!grepl("[0-9]", weekend_activities)) |> filter(weekend_activities != "")
#because options had commas inside, new delim had to be used....

weekend

weekend_dist <- ggplot(weekend, 
                           aes(x = fct_relevel(fct_infreq(weekend_activities),
                                               "J. None of the above"),
                               fill = weekend_activities)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Weekend Activity",
    x = "Activity",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

weekend_dist

#why is there an extra bar?





commute <- df |> select(commute_time) |> drop_na() |> 
  mutate(commute_time = as.numeric(commute_time)) |>
  filter(commute_time <= 480) #some troll values screwed up the graph


commute

#histogram of distribution time
commute_dist <- ggplot(commute, aes(x = commute_time)) +
  geom_histogram(
    bins = 60,
    fill = wesanderson::wes_palette("Zissou1", 1),
    color = "white",
    alpha = 0.9
  ) +
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, 480)
  ) +
  labs(
    x = "Commute Time (minutes)",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold")
  )

commute_dist




age <- df |> select(age) |> drop_na() |> 
  mutate(age = as.numeric(age)) |>
  filter(age < 100, age > 13) #some troll values screwed up the graph
age


#histogram of distribution time
age_dist <- ggplot(age, aes(x = age)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    fill = wesanderson::wes_palette("GrandBudapest2", 1),
    alpha = 0.9
  ) +
  scale_x_continuous(
    labels = scales::comma,
  ) +
  labs(
    x = "Age of Students",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold")
  )

age_dist 




year_of_study <- df |> select(year_level) |> drop_na()
year_of_study

year_of_study_dist <- year_of_study |> 
  mutate(year_level = factor(year_level)) |> 
  ggplot(aes(x = year_level, fill = year_level)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Year Level Distribution", x = "Year Level", y = "Count") +
  theme_minimal(base_size = 14)

year_of_study_dist

gender <- df |> select(gender) |> drop_na()
gender

gender_dist <- gender |>
  ggplot(aes(x = gender, fill = gender)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal(base_size = 14)
gender_dist

lgbtq <- df |> select(lgbtq) |> drop_na()
lgbtq

lgbtq_dist <- lgbtq |>
  ggplot(aes(x = lgbtq, fill = lgbtq)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "LGBTQ Self-Identification", x = "Identity", y = "Count") +
  theme_minimal(base_size = 14)
lgbtq_dist

ethnicity <- df |> select(ethnicity) |> drop_na() |>
  # Split at labels
  mutate(ethnicity = str_split(ethnicity, "(?=[A-J]\\.)")) |> 
  unnest(ethnicity) |> 
  # Remove trailing commas
  mutate(ethnicity = str_replace(ethnicity, ",$", "")) |>
  filter(!grepl("[0-9]", ethnicity)) |> filter(ethnicity != "")
ethnicity

ethnicity_dist <- ethnicity |> ggplot(aes(x = fct_infreq(ethnicity), fill = ethnicity)) +
  geom_bar(show.legend = FALSE) +
    coord_flip() +
  labs(title = "Ethnic Self-Identification", x = "Ethnicity", y = "Count") +
  theme_minimal(base_size = 14)
ethnicity_dist


transfer <- df |> select(transfer) |> drop_na()
transfer

transfer_dist <- transfer |>
  ggplot(aes(x = transfer, fill = transfer)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Transfer Student Distribution", x = "Transfer Status", y = "Count") +
  theme_minimal(base_size = 14)
transfer_dist

international <- df |> select(international) |> drop_na()
international

international_dist <- international |>
  ggplot(aes(x = international, fill = international)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "International Student Distribution", x = "International Status", y = "Count") +
  theme_minimal(base_size = 14)
international_dist

disabilities <- df |> select(disabilities) |> drop_na() |>
  # Split at labels
  mutate(disabilities = str_split(disabilities, ",")) |> 
  unnest(disabilities) |> 
  # Remove trailing commas
  mutate(disabilities = str_replace(disabilities, ",$", "")) |>
  filter(!grepl("[0-9]", disabilities)) |> filter(disabilities != "")
disabilities


disabilities_dist <- disabilities |> ggplot(aes(x = fct_infreq(disabilities),
                                                fill = disabilities)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Disabilities Self-Identification", x = "Disabilities", 
       y = "Count") +
  theme_minimal(base_size = 14)
disabilities_dist


diagnosis <- df |> select(disab_diagnosis) |> drop_na()
diagnosis


diagnosis_dist <- diagnosis |> 
  ggplot(aes(x = disab_diagnosis, fill = disab_diagnosis)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Diagnosis Distribution", x = "Diagnosis Status", y = "Count") +
  theme_minimal(base_size = 14)
diagnosis_dist


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




event_non_attend <- df |> select(non_attend_reasons) |>   # Split at labels
  mutate(non_attend_reasons = str_split(non_attend_reasons, ",")) |> 
  unnest(non_attend_reasons) |> 
  # Remove trailing commas
  mutate(non_attend_reasons = str_replace(non_attend_reasons, ",$", "")) |>
  filter(!grepl("[0-9]", non_attend_reasons)) |> filter(non_attend_reasons != "")
event_non_attend


event_non_attend_dist <- event_non_attend |> 
  ggplot(aes(x = fct_infreq(non_attend_reasons), fill = non_attend_reasons)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +  # flip to horizontal
  labs(title = "Reasons Students Don't Attend Events", x = "Reasons", y = "Count") +
  theme_minimal(base_size = 14)

event_non_attend_dist


merch <- df |> select(ubc_merch) |> 
  drop_na() |>
  # Split at labels
  mutate(ubc_merch = str_split(ubc_merch, "(?=[A-J]\\.)")) |> 
  unnest(ubc_merch) |> 
  # Remove trailing commas
  mutate(ubc_merch = str_replace(ubc_merch, ",$", "")) |>
  filter(!grepl("[0-9]", ubc_merch)) |> filter(ubc_merch != "")
merch



merch_dist <- merch |>
  ggplot(aes(x = fct_infreq(ubc_merch), fill = ubc_merch)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +  # flip to horizontal
  labs(title = "Merch Students Own", x = "Type", y = "Count") +
  theme_minimal(base_size = 14)
merch_dist


found_survey <- df |> select(lead_to_survey) |> drop_na() |>
  mutate(lead_to_survey = str_split(lead_to_survey, "(?=[A-J]\\.)")) |> 
  unnest(lead_to_survey) |> 
  # Remove trailing commas
  mutate(lead_to_survey = str_replace(lead_to_survey, ",$", "")) |>
  filter(!grepl("[0-9]", lead_to_survey)) |> filter(lead_to_survey != "")
found_survey


found_survey_dist <- found_survey |> ggplot(aes(x = fct_infreq(lead_to_survey), 
                                                fill = lead_to_survey)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +  # flip to horizontal
  labs(title = "What Brought Students to this Survey", x = "Type", y = "Count") +
  theme_minimal(base_size = 14)
found_survey_dist

