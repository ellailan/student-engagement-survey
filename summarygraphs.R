source("datacleaning.R")
#only drop NAs when needed... 
commute <- df |> select(commute_time) |> drop_na()
#histogram of distribution time
commute_distribution <- ggplot(commute, aes(x = commute_time)) +
  geom_histogram(
    bins = 10,
    color = "white",
    fill = "skyblue"
  ) +
  theme_minimal() +
  labs(
    title = "Commute Time Distribution",
    x = "Commute Time (minutes)",
    y = "Count"
  )

commute
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