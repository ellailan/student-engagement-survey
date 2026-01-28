source("datacleaning.R")

## ==========================================
# SECTION 1: BRANDED RENDERING FUNCTIONS
# ==========================================

# Define Brand Colors from Brand Guide
brand_palette <- c(
  "royal_blue" = "#0057B7",
  "cyan"       = "#41B6E6",
  "dark_navy"  = "#052B48",
  "warm_red"   = "#F9423A",
  "white"      = "#FFFFFF"
)

# Shared Brand Theme for Consistency
theme_brand_modern <- function() {
  theme_minimal(base_size = 18) + # Significantly larger base font size
    theme(
      text = element_text(family = "sans", color = "#052B48"), # Modern sans-serif
      plot.title = element_text(face = "bold", size = 22, margin = margin(b = 15)),
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14, color = "#052B48"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(), # Cleaner modern look
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Branded Bar Plot Renderer
render_bar_dist <- function(data, x_var, title, flip = TRUE, sort = TRUE) {
  x_sym <- sym(x_var)
  n_levels <- n_distinct(data[[x_var]])
  
  # Cycle through brand colors excluding white and red (reserved for alerts)
  fill_colors <- rep(c("#0057B7", "#41B6E6", "#052B48"), length.out = n_levels)
  
  ggplot(data, aes(x = if(sort) fct_infreq(!!x_sym) else !!x_sym, fill = !!x_sym)) +
    geom_bar(show.legend = FALSE, alpha = 0.9, color = "white", linewidth = 0.8) +
    scale_fill_manual(values = fill_colors) +
    coord_flip() +
    labs(title = title, x = NULL, y = "Count") +
    theme_brand_modern() +
    {if(flip) coord_flip()}
}

# Branded Histogram Renderer
render_hist_dist <- function(data, x_var, title, bins = 30) {
  x_sym <- sym(x_var)
  
  ggplot(data, aes(x = !!x_sym)) +
    geom_histogram(
      bins = bins,
      fill = "#0057B7", # Primary Royal Blue
      color = "white",
      alpha = 0.85
    ) +
    labs(title = title, x = title, y = "Number of Respondents") +
    theme_brand_modern() +
    theme(panel.grid.major.x = element_line(color = "#E0E0E0")) # Re-enable vertical grids for histograms
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
# 1. Prepare Sentiment Data
bing <- get_sentiments("bing")

sent_words <- df |>
  select(one_word_student_life) |>
  drop_na() |>
  unnest_tokens(word, one_word_student_life) |>
  inner_join(bing, by = "word") |>
  count(word, sentiment, sort = TRUE)

# 2. Render Positive Cloud (Cyan to Royal Blue)
positive_cloud <- sent_words |>
  filter(sentiment == "positive") |>
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, family = "sans", fontface = "bold") +
  scale_size_area(max_size = 25) + # Much larger size
  scale_color_gradient(low = "#41B6E6", high = "#0057B7") + # Cyan to Royal Blue
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.title = element_text(family = "sans", face = "bold", size = 28, color = "#052B48", hjust = 0.5)
  ) +
  labs(title = "Positive Sentiments")

# 3. Render Negative Cloud (Dark Navy to Warm Red)
negative_cloud <- sent_words |>
  filter(sentiment == "negative") |>
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(rm_outside = TRUE, family = "sans", fontface = "bold") +
  scale_size_area(max_size = 25) + # Much larger size
  scale_color_gradient(low = "#052B48", high = "#F9423A") + # Dark Navy to Warm Red
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.title = element_text(family = "sans", face = "bold", size = 28, color = "#052B48", hjust = 0.5)
  ) +
  labs(title = "Negative Sentiments")

# Display the branded clouds
print(positive_cloud)
print(negative_cloud)
positive_cloud + negative_cloud


