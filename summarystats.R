library(tidyverse)
library(tidytext)

# ==========================
# Helper functions
# ==========================

get_cat_summary <- function(data, col) {
  data |>
    filter(!is.na(.data[[col]])) |>
    count(.data[[col]], sort = TRUE) |>
    mutate(percentage = round(n / sum(n) * 100, 2))
}

get_multi_summary <- function(data, col, split_pattern) {
  total <- data |> filter(!is.na(.data[[col]])) |> nrow()
  
  data |>
    select(all_of(col)) |>
    drop_na() |>
    rename(option = all_of(col)) |>
    separate_rows(option, sep = split_pattern) |>
    mutate(option = str_trim(option)) |>
    filter(option != "", !str_detect(option, "[0-9]")) |>
    count(option, sort = TRUE) |>
    mutate(pct_of_respondents = round(n / total * 100, 2))
}

get_num_summary <- function(data, col, min = -Inf, max = Inf) {
  data |>
    mutate(val = as.numeric(.data[[col]])) |>
    filter(val >= min, val <= max) |>
    summarise(
      mean = round(mean(val, na.rm = TRUE), 2),
      median = median(val, na.rm = TRUE),
      sd = round(sd(val, na.rm = TRUE), 2),
      min = min(val, na.rm = TRUE),
      max = max(val, na.rm = TRUE),
      total_n = n()
    )
}

# ==========================
# Generate all stats
# ==========================

stats <- list(
  
  # Single-choice
  student_status = get_cat_summary(df, "student_status"),
  affiliate      = get_cat_summary(df, "isaffiliate"),
  year_level     = get_cat_summary(df, "year_level"),
  gender         = get_cat_summary(df, "gender"),
  lgbtq          = get_cat_summary(df, "lgbtq"),
  international  = get_cat_summary(df, "international"),
  transfer       = get_cat_summary(df, "transfer"),
  employment     = get_cat_summary(df, "employment"),
  living         = get_cat_summary(df, "living_arrangement"),
  social_att     = get_cat_summary(df, "social_attitude"),
  diagnosis      = get_cat_summary(df, "disab_diagnosis"),
  
  # Multi-choice
  extracurriculars = get_multi_summary(df, "extracurriculars", ",\\s*"),
  societies        = get_multi_summary(df, "student_society", ",\\s*"),
  disabilities     = get_multi_summary(df, "disabilities", ","),
  non_attend       = get_multi_summary(df, "non_attend_reasons", ","),
  
  weekend   = get_multi_summary(df, "weekend_activities", "(?=[A-J]\\.)"),
  ethnicity = get_multi_summary(df, "ethnicity", "(?=[A-J]\\.)"),
  merch     = get_multi_summary(df, "ubc_merch", "(?=[A-J]\\.)"),
  source    = get_multi_summary(df, "lead_to_survey", "(?=[A-J]\\.)"),
  
  # Numeric
  age     = get_num_summary(df, "age", 14, 99),
  commute = get_num_summary(df, "commute_time", 0, 480),
  
  # Sentiment
  sentiment = df |>
    select(one_word_student_life) |>
    unnest_tokens(word, one_word_student_life) |>
    inner_join(get_sentiments("bing"), by = "word") |>
    count(sentiment) |>
    mutate(percentage = round(n / sum(n) * 100, 2))
)

# ==========================
# Print everything
# ==========================

walk(names(stats), function(name) {
  cat("\n==============================\n")
  cat(str_to_upper(name), "\n")
  cat("==============================\n")
  print(stats[[name]])
})
