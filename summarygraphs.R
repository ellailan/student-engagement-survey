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
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

student_status_dist

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