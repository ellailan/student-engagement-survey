
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

#converting age, year, into integer values
df <- df |> mutate(commute_time = as.numeric(commute_time))
df <- df |>
  mutate(
    year_level = case_when(
      year_level == "A. 1st" ~ 1,
      year_level == "B. 2nd" ~ 2,
      year_level == "C. 3rd" ~ 3,
      year_level == "D. 4th" ~ 4,
      year_level == "E. 5th or more" ~ 5,
      TRUE ~ NA_real_  # in case of missing/other values
    ))
df <- df |> mutate(year_level = as.numeric(year_level)) 

