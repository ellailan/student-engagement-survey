source("datacleaning.R")

# ==========================================
# SECTION 1: RENDERING FUNCTIONS
# ==========================================

# High-level Bar Plot Renderer
render_bar_dist <- function(data, x_var, title, flip = TRUE, sort = TRUE) {
  x_sym <- sym(x_var)
  n_levels <- n_distinct(data[[x_var]])
  
  # Use wesanderson palette; repeat colors if more levels than palette length
  base_palette <- wesanderson::wes_palette("Darjeeling1", n = max(n_levels, 3), type = "continuous")
  
  ggplot(data, aes(x = if(sort) fct_infreq(!!x_sym) else !!x_sym, fill = !!x_sym)) +
    geom_bar(show.legend = FALSE, alpha = 0.85, color = "white") +
    scale_fill_manual(values = rep(base_palette, length.out = n_levels)) +
    labs(title = title, x = NULL, y = "Count") +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10),
      panel.grid.major.x = element_blank()
    ) +
    {if(flip) coord_flip()}
}


# High-level Histogram Renderer (fixed)
render_hist_dist <- function(data, x_var, title, bins = 30) {
  x_sym <- sym(x_var)
  
  # Use a single pleasing color from Wes Anderson
  fill_color <- wesanderson::wes_palette("Darjeeling1", type = "continuous")[2]
  
  ggplot(data, aes(x = !!x_sym)) +
    geom_histogram(
      bins = bins,
      fill = fill_color,
      color = "white",
      alpha = 0.85
    ) +
    labs(title = title, x = title, y = "Number of Respondents") +
    theme_minimal(base_size = 13) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
}
# ==========================================
# SECTION 2: CLEANING & EXECUTION
# ==========================================

# 1. Simple Categorical Bar Plots
student_status_dist <- df |> select(student_status) |> drop_na() |> 
  render_bar_dist("student_status", "Student Status", flip = FALSE)

affiliate_dist <- df |> select(isaffiliate) |> drop_na() |> 
  render_bar_dist("isaffiliate", "Affiliate Status Distribution", flip = FALSE)

affiliate_school_dist <- df |> select(affiliate_school) |> drop_na() |> 
  render_bar_dist("affiliate_school", "Affiliate School Distribution")

year_of_study_dist <- df |> select(year_level) |> drop_na() |> mutate(year_level = factor(year_level)) |> 
  render_bar_dist("year_level", "Year Level Distribution", flip = FALSE, sort = FALSE)

gender_dist <- df |> select(gender) |> drop_na() |> 
  render_bar_dist("gender", "Gender Distribution", flip = FALSE)

lgbtq_dist <- df |> select(lgbtq) |> drop_na() |> 
  render_bar_dist("lgbtq", "LGBTQ Self-Identification", flip = FALSE)

transfer_dist <- df |> select(transfer) |> drop_na() |> 
  render_bar_dist("transfer", "Transfer Student Distribution", flip = FALSE)

international_dist <- df |> select(international) |> drop_na() |> 
  render_bar_dist("international", "International Student Distribution", flip = FALSE)

diagnosis_dist <- df |> select(disab_diagnosis) |> drop_na() |> 
  render_bar_dist("disab_diagnosis", "Diagnosis Distribution", flip = FALSE)

employment_stat_dist <- df |> select(employment) |> drop_na() |> 
  render_bar_dist("employment", "Employment Status Distribution")

living_stat_dist <- df |> select(living_arrangement) |> drop_na() |> 
  render_bar_dist("living_arrangement", "Living Arrangement Distribution")

social_dist <- df |> select(social_attitude) |> drop_na() |> 
  render_bar_dist("social_attitude", "Social Attitude Distribution")

# 2. Multi-Response Bar Plots (String Splitting)
faculty_dist <- df |> select(student_society) |> drop_na() |> 
  filter(!grepl("[0-9]", student_society)) |> 
  render_bar_dist("student_society", "Student Society Memberships")

extracurric_dist <- df |> select(extracurriculars) |> drop_na() |> 
  separate_rows(extracurriculars, sep = ",\\s*") |> 
  filter(!grepl("[0-9]", extracurriculars)) |> 
  render_bar_dist("extracurriculars", "Extracurricular Involvement")

disabilities_dist <- df |> select(disabilities) |> drop_na() |> 
  mutate(disabilities = str_split(disabilities, ",")) |> unnest(disabilities) |> 
  filter(!grepl("[0-9]", disabilities), disabilities != "") |> 
  render_bar_dist("disabilities", "Disabilities Self-Identification")

event_non_attend_dist <- df |> select(non_attend_reasons) |> drop_na() |> 
  mutate(non_attend_reasons = str_split(non_attend_reasons, ",")) |> unnest(non_attend_reasons) |> 
  filter(!grepl("[0-9]", non_attend_reasons), non_attend_reasons != "") |> 
  render_bar_dist("non_attend_reasons", "Reasons Students Don't Attend Events")

weekend_dist <- df |> select(weekend_activities) |> drop_na() |> 
  mutate(weekend_activities = str_split(weekend_activities, "(?=[A-J]\\.)")) |> unnest(weekend_activities) |> 
  filter(!grepl("[0-9]", weekend_activities), weekend_activities != "") |> 
  render_bar_dist("weekend_activities", "Weekend Activity")

ethnicity_dist <- df |> select(ethnicity) |> drop_na() |> 
  mutate(ethnicity = str_split(ethnicity, "(?=[A-J]\\.)")) |> unnest(ethnicity) |> 
  filter(!grepl("[0-9]", ethnicity), ethnicity != "") |> 
  render_bar_dist("ethnicity", "Ethnic Self-Identification")

merch_dist <- df |> select(ubc_merch) |> drop_na() |> 
  mutate(ubc_merch = str_split(ubc_merch, "(?=[A-J]\\.)")) |> unnest(ubc_merch) |> 
  filter(!grepl("[0-9]", ubc_merch), ubc_merch != "") |> 
  render_bar_dist("ubc_merch", "Merch Students Own")

found_survey_dist <- df |> select(lead_to_survey) |> drop_na() |> 
  mutate(lead_to_survey = str_split(lead_to_survey, "(?=[A-J]\\.)")) |> unnest(lead_to_survey) |> 
  filter(!grepl("[0-9]", lead_to_survey), lead_to_survey != "") |> 
  render_bar_dist("lead_to_survey", "What Brought Students to this Survey")

# 3. Continuous Data (Histograms)
commute_dist <- df |> select(commute_time) |> drop_na() |> 
  mutate(commute_time = as.numeric(commute_time)) |> filter(commute_time <= 480) |> 
  render_hist_dist("commute_time", "Commute Time (minutes)", bins = 60)


age_dist <- df |> select(age) |> drop_na() |> 
  mutate(age = as.numeric(age)) |> filter(age < 100, age > 13) |> 
  render_hist_dist("age", "Age of Students", bins = 20)



# ==========================================
# SECTION 3: WORD CLOUDS
# ==========================================

bing <- get_sentiments("bing")

sent_words <- df |>
  select(one_word_student_life) |>
  drop_na() |>
  unnest_tokens(word, one_word_student_life) |>
  inner_join(bing, by = "word") |>
  count(word, sentiment, sort = TRUE)

# Render Positive Cloud
positive_cloud <- sent_words |>
  filter(sentiment == "positive") |>
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, family = "Futura") +
  scale_size_area(max_size = 12) +
  scale_color_gradient(low = "#4586ff", high = "#00379e") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#d9edff", color = NA))

# Render Negative Cloud
negative_cloud <- sent_words |>
  filter(sentiment == "negative") |>
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, family = "Futura") +
  scale_size_area(max_size = 12) +
  scale_color_gradient(low = "#ff4d4d", high = "#a83232") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#fad4d2", color = NA))

