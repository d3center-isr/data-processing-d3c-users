library(dplyr)
library(lubridate)  # This package is for reading and using the date/time formatted variables correctly
library(readxl)

################################################
# DATASET: dat_primary_aim_another_subset_app_tracked_cols
#  Primary aim data, mostly consisting of tracked app usage data.
# RUN: pre-processing code
source(file.path("working-with-ema-and-tracked-app-usage", "preprocess-app-tracked.R"))

# dat_primary_aim:  Data for 99 usable participants, i.e., individuals
#   with at least three completed EMA observations between days 2-9.  
#   It has 5940 rows.  
#   This is longitudinal data in long (tall) format,
#   i.e., one row for each time within each person.
#   Rows are uniquely identified by the person ID
#   (mars_id) and decision time index (decision_point).
# dat_primary_aim_another_subset: A subset of data_primary_aim,
#   excluding data from the first and last  days of the study
#   (i.e., days 1 and 8).  This is done by excluding decision
#   points before number 7 or after number 54.  It has
#   4752 rows.
#   If you further filter out unavailable decision times via
#    (dat_primary_aim_another_subset %>% filter(eligibility == 1))
#   then you get 3858  rows.
# dat_primary_aim_another_subset_app_tracked_cols: Has the same rows
#   as dat_primary_aim_another_subset but contains few columns. Specifically
#   columns in dat_primary_aim_another_subset were dropped so that we are mostly
#   left with the tracked app usage variables.
# A few important variables:
#    mars_id:   Person ID as a character string
#    decision_point:  Time index from 1 to 60, representing
#         6 times on each of 10 days.  We're especially interested
#         in days 2-9 (times 7-54)
#    Y: This is the primary proximal outcome of the MARS MRT. 
#       The primary outcome was engagement with self-regulatory strategies 
#       self-reported in EMAs ?1 hour following each micro-randomization. 
#       The primary outcome was a binary variable, operationalized using a 
#       combination of the three EMA items below. 
#       ‘Think about the most recent tip or activity recommended by MARS. Did you use it in the last hour?’ (Possible responses: ‘Yes’ or ‘No’)
#       ‘In the last hour, did you use any other tip or activity recommended by MARS?’ (Possible responses: ‘Yes’ or ‘No’)
#       ‘In the last hour, did you use any other tip or activity not included in MARS?’ (Possible responses: ‘Yes’ or ‘No’)
#        Participants were considered to have engaged in self-regulatory strategies
#        1 hour after micro-randomization if they answered ‘Yes’ to 
#       at least one of the items (in this case, the binary variable was coded as 1). 
#       On the other hand, participants were considered to have not engaged in self-regulatory strategies 
#       1 hour after micro-randomization if they answered ‘No’ to all three of 
#       the items (in this case, the binary variable was coded as 0).  
#    ts_emi_resp: If the participant was randomized to any prompt, they were 
#                 presented with a screen that said "Time to practice strategy?". 
#                 The screen presented two possible responses: Ok, or Cancel. 
#                 This variable shows the timestamp when the participant selected either Ok or Cancel. 
#                 If the participant ignored the screen this timestamp represents 
#                 when the screen "timed out" thereby the participant MISSED their 
#                 opportunity to select a response on the screen.
#    emi_resp: Whether the participant selected Ok, Cancel, or missed their opportunity to select a response.
#    in_mars: Whether they were in the high-effort activities part of the 
#             MARS app  within the one hour period after micro-ranodmization
#    in_tips: Whether they were in the low-effort activities part of the MARS app  
#             within the one hour period after micro-ranodmization

remove(dat_primary_aim, dat_primary_aim_another_subset, only_these_cols)
# Note: dat_primary_aim above has a different number of rows than 
# dat_primary_aim. Both originated from the same raw data, but were obtained 
# from different points/junctures of preprocessing.


################################################
# DATASET: dat_primary_aim
#  Primary aim data, consisting of Ecological Momentary Assessment
#  (EMA) questionnaires and randomization statuses.
# RUN: pre-processing code
source(file.path("working-with-ema-and-tracked-app-usage", "preprocess-ema.R"))

# dat_primary_aim:  Data for 99 usable participants.
#   It has 6600 rows
#   This is longitudinal data in long (tall) format,
#   i.e., one row for each time within each person.
#   Rows are uniquely identified by the person ID
#   (mars_id) and decision time index (decision_point).
# dat_primary_aim_subset:  A subset of data_primary_aim,
#   excluding data from the first and last  days of the study
#   (i.e., days 1 and 8).  This is done by excluding decision
#   points before number 7 or after number 54.  It has
#   5280 rows.
# dat_primary_aim_another_subset:  A further subset of
#   dat_primary_aim_subset, but including only individuals
#   with at least three completed EMA observations.  It has
#   4752 rows.  If you further filter out unavailable
#   decision times via
#    (dat_primary_aim_another_subset %>% filter(eligibility == 1))
#   then you get 3858  rows.
# A few important variables:
#    mars_id:   Person ID as a character string
#    decision_point:  Time index from 1 to 60, representing
#         6 times on each of 10 days.  We're especially interested
#         in days 2-9 (times 7-54)
#    eligibility:  Eligibility (availability) status of
#                  the person at this decision time.
#    ep_emi_rand:  Randomized decision to "none" (no
#                  prompt), "low_effort" (low effort prompt),
#                  or "mars" (high effort prompt).
#    coinflip:  Equals 1 if randomization was to a prompt
#               (either "low_effort" or "mars") or 0
#               if it was to no prompt.
#    ei_2qs_cig_avail:  Self-reported cigarette availability
#               at this occasion (1=yes, 0=no); reported
#               on the two-question Ecological Momentary
#               Assessment survey.
#    ei_2qs_negaff:  Self-reported negative affect at
#               this occasion (scaled from 0 to 1); reported
#               on the two-question Ecological Momentary
#               Assessment survey.
#    ep_2qs_status:  Status of the two-question EMA survey;
#               e.g., "undelivered", "completed", "missed", etc.

################################################
# EXAMPLE CODE: Merging app tracked data with EMA data

nrow(dat_primary_aim_another_subset) # Should be 4752

nrow(dat_primary_aim_another_subset_app_tracked_cols) # Should be 4752

intersect(x = colnames(dat_primary_aim_another_subset), y = colnames(dat_primary_aim_another_subset_app_tracked_cols)) # You should get "mars_id" and "decision_point"

dat_primary_aim_another_subset %>%
  group_by(eligibility, coinflip) %>%
  summarise(n_decision_points = n())
# You should get the following output:
#             eligibility     coinflip    n_decision_points
#             <dbl>           <dbl>       <int>
# 1           0               NA          894
# 2           1               0           1934
# 3           1               1           1924

dat_primary_aim_another_subset %>%
  group_by(eligibility, is_high_effort, is_low_effort) %>%
  summarise(n_decision_points = n())

#             eligibility     is_high_effort    is_low_effort     n_decision_points
#             <dbl>           <dbl>             <dbl>             <int>
# 1           0               NA                NA                894
# 2           1               0                 0                 1934
# 3           1               0                 1                 997
# 4           1               1                 0                 927

merged_ema_and_app_tracked <- left_join(x = dat_primary_aim_another_subset,
                                        y = dat_primary_aim_another_subset_app_tracked_cols,
                                        by = join_by("mars_id" == "mars_id",
                                                     "decision_point" == "decision_point"))

# After merging, see how many participants and rows you have
merged_ema_and_app_tracked %>%
  .[["mars_id"]] %>%
  unique(.) %>%
  length(.) # What is this number now? You should get 99

merged_ema_and_app_tracked %>% 
  nrow(.) # What is this number now? You should get 4752

merged_ema_and_app_tracked %>% 
  filter(eligibility == 1) %>%
  nrow(.) # What is this number now? You should get 3858

# Change to TRUE if saving the dataset
if(FALSE){
  my_path <- "O:/d3c-BIDS/MARS/05 - Manipulated Data/Derived Engagement Measures"  # You can change this to your relevant file path
  saveRDS(merged_ema_and_app_tracked, file = file.path(my_path, "merged_ema_and_app_tracked.rds"))
}

################################################
# DATASET:  Screener (Baseline)
path_screening_questionnaire <- "O:/d3c-BIDS/MARS/04 - Original Data/2025-6-25-MARS Curated Data/Questionnaire/Release v1.1.0/Data for analysis"
screening_questionnaire <- readRDS(file.path(path_screening_questionnaire, "screening_quest.rds"))
# A few important variables:
# scr_age:  age at screening
# scr_cpd: On average, how many cigarettes do you smoke each day? (number)

################################################
# DATASET:  Visit 1 Questionnaire (Baseline)
path_v1_questionnaire <- "O:/d3c-BIDS/MARS/04 - Original Data/2025-6-25-MARS Curated Data/Questionnaire/Release v1.1.0/Data for analysis"
v1_questionnaire <- readRDS(file.path(path_v1_questionnaire, "v1_baseline_quest.rds"))
# A few important variables:
# dses1a_1:  Sex at birth (1=male, 2=female)
# th1_1:  Smoking frequency (1=sometimes, 2=every day; almost all say every day)
# th2_1:  Cigarettes per day (number)
# wisdm_total:  WISDM smoking dependence score

################################################
# DATASET: Visit 4 Questionnaire (6 Month Follow-up)
path_v4_questionnaire <- "O:/d3c-BIDS/MARS/04 - Original Data/2025-6-25-MARS Curated Data/Questionnaire/Release v1.1.0/Data for analysis"
v4_questionnaire <- readRDS(file.path(path_v4_questionnaire, "v4_quest_6_month.rds"))
# A few important variables:
#   SE_total:  Self-efficacy to abstain from smoking, totaled over
#              the self-efficacy scale
#   self_effic_scale_complete Whether the self-efficacy scale was
#              completed
# tac1_1_v5:  Smoking since previous  visit
# tac1a_1_v5:  Smoking during previous week (at -99 for those who said no
#                                            smoking since previous visit)

################################################
# EXAMPLE CODE: Merging visit data with app
# tracked data with EMA data

colnames_v1_questionnaire <- colnames(v1_questionnaire)
colnames_v4_questionnaire <- colnames(v4_questionnaire)
common_names <- intersect(colnames_v1_questionnaire, colnames_v4_questionnaire)
common_names <- common_names[common_names != "record_id" & common_names != "mars_id"]

# mars_id and record_id uniquely identify each observation in the study visit questionnaire.
# Thus, merging the Visit 1 and Visit 4 questionnaires will be based on mars_id and record_id
# Note that some (but not all) items at visit 1 were assessed again Visit 4; such items
# will have identical column names across the Visit 1 and Visit 4 datasets. Thus we will
# perform a data preprocessing step to add a suffix indicating when the item was assessed

colnames_v1_questionnaire <- if_else(colnames_v1_questionnaire %in% common_names, 
                                     paste(colnames_v1_questionnaire, "_V1", sep = ""), 
                                     colnames_v1_questionnaire)

colnames(v1_questionnaire) <- colnames_v1_questionnaire

colnames_v4_questionnaire <- if_else(colnames_v4_questionnaire %in% common_names, 
                                     paste(colnames_v4_questionnaire, "_V4", sep = ""), 
                                     colnames_v4_questionnaire)

colnames(v4_questionnaire) <- colnames_v4_questionnaire

all_study_visit_questionnaire <- full_join(x = v1_questionnaire, 
                                           y = v4_questionnaire,
                                           by = join_by("mars_id" == "mars_id", "record_id" == "record_id"))

# It is important to perform a LEFT JOIN
# rather than a RIGHT JOIN, INNER JOIN, or FULL JOIN
# since the Visit 1 dataset and the Visit 4 datasets
# each contains 111 participants and we only want
# visit data for the 99 participants who
# completed at least 3 EMA between days 2-9.
merged_ema_and_app_tracked_and_visit_data <- left_join(x = merged_ema_and_app_tracked,
                                                       y = all_study_visit_questionnaire,
                                                       by = join_by("mars_id" == "mars_id"))

# There are some items that were assessed at screening at Visit 1.
# In those cases, we'd prioritize the Visit 1 assessment over screening
# and only use screening to recoup missing values in Visit 1 data.
screening_questionnaire <- screening_questionnaire %>%
  select(record_id, 
         scr_age, # Age is only assessed at screening
         scr_cpd, # This variable can be used to recoup missing values in th2_1 at Visit 1
         gender, # This variable will be used to recoup missing values at Visit 1
         ethnicity, # This variable will be used to recoup missing values at Visit 1
         # These variables will be used to recoup missing values at Visit 1
         race___1, # White
         race___2, # Black or African American
         race___3, # Asian
         race___4, # Native Hawaiian or other pacific islander
         race___5) # American Indian or Alaska Native 

merged_ema_and_app_tracked_and_visit_data <- left_join(x = merged_ema_and_app_tracked_and_visit_data, 
                                                       y = screening_questionnaire,
                                                       by = join_by("record_id" == "record_id"))

# Create demographic variables for analysis
merged_ema_and_app_tracked_and_visit_data <- merged_ema_and_app_tracked_and_visit_data %>%
  mutate(age = scr_age,
         baseline_tobacco_history = if_else(!is.na(th2_1), th2_1, scr_cpd)) %>%
  mutate(income_category = case_when(
    (dses14_1 == 1) | (dses14_1 == 2) ~ "less than or equal to USD 9,999",
    (dses14_1 == 3) | (dses14_1 == 4) ~ "greater than USD 9,999 and less than or equal to USD 19,999",
    (dses14_1 == 5) | (dses14_1 == 6) ~ "greater than USD 19,999 and less than or equal to USD 29,999",
    dses14_1 == 7 ~ "greater than USD 29,999 and less than or equal to USD 39,999",
    dses14_1 == 8 ~ "greater than USD 39,999 and less than or equal to USD 49,999",
    dses14_1 == 9 ~ "greater than USD 49,999 and less than or equal to USD 59,999",
    dses14_1 == 10 ~ "greater than USD 59,999 and less than or equal to USD 69,999",
    dses14_1 == 11 ~ "greater than USD 69,999 and less than or equal to USD 79,999",
    dses14_1 == 12 ~ "greater than USD 79,999 and less than or equal to USD 89,999",
    dses14_1 == 13 ~ "greater than USD 89,999 and less than or equal to USD 99,999",
    dses14_1 == 14 ~ "greater than or equal to USD 100,000",
    dses14_1 == 15 ~ NA_character_,
    .default = NULL
  )) %>%
  mutate(income_val = case_when(
    income_category == "less than or equal to USD 9,999" ~ 1,
    income_category == "greater than USD 9,999 and less than or equal to USD 19,999" ~ 2,
    income_category == "greater than USD 19,999 and less than or equal to USD 29,999" ~ 3,
    income_category == "greater than USD 29,999 and less than or equal to USD 39,999" ~ 4,
    income_category == "greater than USD 39,999 and less than or equal to USD 49,999" ~ 5,
    income_category == "greater than USD 49,999 and less than or equal to USD 59,999" ~ 6,
    income_category == "greater than USD 59,999 and less than or equal to USD 69,999" ~ 7,
    income_category == "greater than USD 69,999 and less than or equal to USD 79,999" ~ 8,
    income_category == "greater than USD 79,999 and less than or equal to USD 89,999" ~ 9,
    income_category == "greater than USD 89,999 and less than or equal to USD 99,999" ~ 10,
    income_category == "greater than or equal to USD 100,000" ~ 11,
    .default = NULL
  )) %>%
  mutate(partner_status_category = case_when(
    dses3_1 == 1 ~ "single",
    dses3_1 == 2 ~ "married",
    dses3_1 == 3 ~ "divorced",
    dses3_1 == 4 ~ "widowed",
    dses3_1 == 5 ~ "living with significant other",
    dses3_1 == 6 ~ "separated",
    .default = NULL
  )) %>%
  mutate(has_partner = if_else((partner_status_category == "married") | (partner_status_category == "living with significant other"), 1, 0)) %>%
  mutate(gender_category = case_when(
    dses1_1 == 1 ~ "male",
    dses1_1 == 2 ~ "female",
    dses1_1 == 8 ~ NA_character_,
    (dses1_1 != 1) & (dses1_1 != 2) & (dses1_1 != 8) ~ "other",
    .default = NULL
  )) %>%
  mutate(screener_gender = case_when(
    gender == "M" ~ "male",
    gender == "F" ~ "female",
    .default = NULL
  )) %>%
  mutate(is_missing_v1_gender = if_else(is.na(gender_category), 1, 0)) %>%
  # If no reported gender at V1, then use reported gender at screener
  mutate(gender_category = if_else(is_missing_v1_gender == 1, screener_gender, gender_category)) %>%
  mutate(is_male = if_else(gender_category == "male", 1, 0))

merged_ema_and_app_tracked_and_visit_data <- merged_ema_and_app_tracked_and_visit_data %>%
  mutate(is_latino_category = if_else(dses4_1 == 1, "latino", "not latino")) %>%
  mutate(race_is_white = dses5a_1___1,
         race_is_black_or_african_american = dses5a_1___2,
         race_is_asian = dses5a_1___3,
         race_is_native_hawaiian_or_other_pacific_islander = dses5a_1___4,
         race_is_american_indian_or_alaska_native = dses5a_1___5) %>%
  mutate(num_checked_race = dses5a_1___1 + dses5a_1___2 + dses5a_1___3 + dses5a_1___4 + dses5a_1___5) %>%
  mutate(race_category = case_when(
    num_checked_race == 1 & race_is_white == 1 ~ "white",
    num_checked_race == 1 & race_is_black_or_african_american == 1 ~ "black or african american",
    num_checked_race == 1 & race_is_american_indian_or_alaska_native == 1 ~ "american indian or alaska native",
    num_checked_race == 1 & race_is_native_hawaiian_or_other_pacific_islander == 1 ~ "native hawaiian or other pacific islander",
    num_checked_race == 1 & race_is_asian == 1 ~ "asian",
    num_checked_race >= 2 ~ "multiracial",
    .default = NULL
  ))

# If race/ethnicity was not reported in V1 look back to screener
merged_ema_and_app_tracked_and_visit_data <- merged_ema_and_app_tracked_and_visit_data %>%
  mutate(is_missing_v1_ethnicity = if_else(is.na(is_latino_category), 1, 0),
         is_missing_v1_race = if_else(is.na(race_category), 1, 0)) %>%
  mutate(screener_is_latino_category = if_else(ethnicity == 1, "latino", "not latino")) %>%
  mutate(screener_race_is_white = race___1,
         screener_race_is_black_or_african_american = race___2,
         screener_race_is_asian = race___3,
         screener_race_is_native_hawaiian_or_other_pacific_islander = race___4,
         screener_race_is_american_indian_or_alaska_native = race___5) %>%
  mutate(screener_num_checked_race = race___1 + race___2 + race___3 + race___4 + race___5) %>%
  mutate(screener_race_category = case_when(
    screener_num_checked_race == 1 & screener_race_is_white == 1 ~ "white",
    screener_num_checked_race == 1 & screener_race_is_black_or_african_american == 1 ~ "black or african american",
    screener_num_checked_race == 1 & screener_race_is_american_indian_or_alaska_native == 1 ~ "american indian or alaska native",
    screener_num_checked_race == 1 & screener_race_is_native_hawaiian_or_other_pacific_islander == 1 ~ "native hawaiian or other pacific islander",
    screener_num_checked_race == 1 & screener_race_is_asian == 1 ~ "asian",
    screener_num_checked_race >= 2 ~ "multiracial",
    .default = NULL
  )) %>%
  mutate(is_latino_category = if_else(is_missing_v1_ethnicity==1, screener_is_latino_category, is_latino_category),
         race_category = if_else(is_missing_v1_race == 1, screener_race_category, race_category),
         num_checked_race = if_else(is_missing_v1_race == 1, screener_num_checked_race, num_checked_race)) %>%
  mutate(race_is_white = if_else(is_missing_v1_race == 1, screener_race_is_white, race_is_white),
         race_is_black_or_african_american = if_else(is_missing_v1_race == 1, screener_race_is_black_or_african_american, race_is_black_or_african_american),
         race_is_asian = if_else(is_missing_v1_race == 1, screener_race_is_asian, race_is_asian),
         race_is_native_hawaiian_or_other_pacific_islander = if_else(is_missing_v1_race == 1, screener_race_is_native_hawaiian_or_other_pacific_islander, race_is_native_hawaiian_or_other_pacific_islander),
         race_is_american_indian_or_alaska_native = if_else(is_missing_v1_race == 1, screener_race_is_american_indian_or_alaska_native, race_is_american_indian_or_alaska_native)) %>%
# Collapse categories into buckets recommended by collaborators
  mutate(race_and_ethnicity = case_when(
    is_latino_category == "latino" ~ "latino",
    is_latino_category == "not latino" & (race_is_black_or_african_american == 1 & num_checked_race == 1) ~ "not latino and black",
    is_latino_category == "not latino" & (race_is_white == 1 & num_checked_race == 1) ~ "not latino and white",
    is_latino_category == "not latino" & (num_checked_race > 1 | race_is_asian == 1 | race_is_native_hawaiian_or_other_pacific_islander == 1 | race_is_american_indian_or_alaska_native == 1) ~ "other",
    .default = NULL))  %>%  
  # Race/ethnicity
  # * Very Important: remember that the reference category specified by collaborators for race/ethnicity is the "not latino and white" category
  # * Turns out, not latino and white and latino categories tie as the largest categories among the four categories
  mutate(is_latino = if_else(race_and_ethnicity == "latino", 1, 0),
         is_not_latino_and_black = if_else(race_and_ethnicity == "not latino and black", 1, 0),
         is_not_latino_and_other = if_else(race_and_ethnicity == "other", 1, 0),
         is_not_latino_and_white = if_else(race_and_ethnicity == "not latino and white", 1, 0))

# Change to TRUE if saving the dataset
if(FALSE){
  my_path <- "O:/d3c-BIDS/MARS/05 - Manipulated Data/Derived Engagement Measures"  # You can change this to your relevant file path
  saveRDS(merged_ema_and_app_tracked_and_visit_data, file = file.path(my_path, "merged_ema_and_app_tracked_and_visit_data.rds"))
}

################################################################################
# Calculate summary statistics for demographics

# Create missing data indicators for counting number of participants 
# having missing values in the demographic variables
merged_ema_and_app_tracked_and_visit_data <- merged_ema_and_app_tracked_and_visit_data %>%
  mutate(is_missing_age = if_else(is.na(age), 1, 0),
         is_missing_gender = if_else(is.na(gender_category), 1, 0),
         is_missing_race_and_ethnicity = if_else(is.na(race_and_ethnicity), 1, 0),
         is_missing_baseline_tobacco_history = if_else(is.na(baseline_tobacco_history), 1, 0),
         is_missing_partner_status = if_else(is.na(partner_status_category), 1, 0),
         is_missing_income = if_else(is.na(income_val), 1, 0)) %>%
  mutate(is_missing_any_demog_data = if_else(is_missing_age + is_missing_gender + is_missing_race_and_ethnicity + is_missing_baseline_tobacco_history + is_missing_partner_status + is_missing_income >= 1, 1, 0))

dat_summary_missing_demogs <- merged_ema_and_app_tracked_and_visit_data %>%
  filter(decision_point == 10) %>%
  summarise(n_participants = n(),
            n_participants_with_missing_demogs = sum(is_missing_any_demog_data),
            n_missing_age = sum(is_missing_age),
            n_missing_gender = sum(is_missing_gender),
            n_missing_race_and_ethnicity = sum(is_missing_race_and_ethnicity),
            n_missing_baseline_tobacco_history = sum(is_missing_baseline_tobacco_history),
            n_missing_partner_status = sum(is_missing_partner_status),
            n_missing_income = sum(is_missing_income))

print(dat_summary_missing_demogs)

# > dat_summary_missing_demogs
# # A tibble: 1 × 8
#             n_participants          n_participants_with_missing_demogs    n_missing_age n_missing_gender n_missing_race_and_ethnicity n_missing_baseline_tobacco_history n_missing_partner_status n_missing_income
#               <int>                                             <dbl>             <dbl>            <dbl>                        <dbl>                              <dbl>                    <dbl>            <dbl>
#   1             99                                                8                 0                0                            0                                  0                        2                8

dat_summary_demogs_continuous <- merged_ema_and_app_tracked_and_visit_data %>%
  filter(decision_point == 10) %>%
  summarise(m_age = mean(age),
            sd_age = sd(age),
            m_baseline_tobacco_history = mean(baseline_tobacco_history, na.rm = TRUE),
            sd_baseline_tobacco_history = sd(baseline_tobacco_history, na.rm = TRUE),
            m_income = mean(income_val, na.rm = TRUE),
            sd_income = sd(income_val, na.rm = TRUE))

print(dat_summary_demogs_continuous)

# > dat_summary_demogs_continuous
# # A tibble: 1 × 6
#         m_age sd_age m_baseline_tobacco_history sd_baseline_tobacco_history m_income sd_income
#         <dbl>  <dbl>                      <dbl>                       <dbl>    <dbl>     <dbl>
#   1     43.5   11.8                       13.2                        7.29     4.92      3.06

dat_summary_demogs_binary <- merged_ema_and_app_tracked_and_visit_data %>%
  filter(decision_point == 10) %>%
  summarise(n_male = sum(is_male),
            pct_male = mean(is_male) * 100,
            n_latino = sum(is_latino),
            pct_latino = mean(is_latino) * 100,
            n_not_latino = sum(is_latino == 0),
            pct_not_latino = mean(is_latino == 0),
            n_not_latino_and_black = sum(is_not_latino_and_black),
            pct_not_latino_and_black = mean(is_not_latino_and_black) * 100,
            n_not_latino_and_other = sum(is_not_latino_and_other),
            pct_not_latino_and_other = mean(is_not_latino_and_other) * 100,
            n_not_latino_and_white = sum(is_not_latino_and_white),
            pct_not_latino_and_white = mean(is_not_latino_and_white) * 100,
            n_has_partner = sum(has_partner, na.rm = TRUE),
            pct_has_partner = mean(has_partner, na.rm = TRUE) * 100)

print(dat_summary_demogs_binary)

# > dat_summary_demogs_binary
# # A tibble: 1 × 14
#       n_male pct_male n_latino pct_latino n_not_latino pct_not_latino n_not_latino_and_black pct_not_latino_and_black n_not_latino_and_other pct_not_latino_and_other n_not_latino_and_white pct_not_latino_and_white n_has_partner pct_has_partner
#       <dbl>    <dbl>    <dbl>      <dbl>        <int>          <dbl>                  <dbl>                    <dbl>                  <dbl>                    <dbl>                  <dbl>                    <dbl>         <dbl>           <dbl>
#   1     33     33.3       30       30.3           69          0.697                     30                     30.3                      8                     8.08                     31                     31.3            44            45.4

dat_summary_income_tabulation <- merged_ema_and_app_tracked_and_visit_data %>% 
  filter(decision_point == 10) %>%
  group_by(income_val) %>% 
  summarise(num_participants = n())

print(dat_summary_income_tabulation)

# > dat_summary_income_tabulation
# # A tibble: 12 × 2
#         income_val num_participants
#         <dbl>            <int>
# 1          1               11
# 2          2               11
# 3          3               17
# 4          4                8
# 5          5               10
# 6          6                8
# 7          7                7
# 8          8                5
# 9          9                3
# 10         10                3
# 11         11                8
# 12         NA                8


################################################
# Relevant columns for trying to replicate the primary aim analysis
dat_for_analysis <- merged_ema_and_app_tracked_and_visit_data %>%
  select(mars_id, participant_id,
         decision_point, 
         # Baseline control variables:
         age, is_male, is_latino, is_not_latino_and_black, is_not_latino_and_other, baseline_tobacco_history, has_partner, income_val, 
         # Eligibility for micro-randomization and treatment assignments
         #        eligibility: Whether a participant-decision point was eligible for micro-randomization
         #        coinflip: Whether a participant-decision point was assigned to any prompt (=1) or no prompt (=0)
         #                  Note that we use the convention of coding this variable as NA if eligibility=0
         #        is_high_effort: Whether a participant-decision point was assigned to high effort prompt (=1) 
         #                        or any of the two remaining options (=0) specifically low effort prompt or no prompt
         #        is_low_effort: Whether a participant-decision point was assigned to low effort prompt (=1) 
         #                       or any of the two remaining options (=0) specifically high effort prompt or no prompt
         eligibility, coinflip, is_high_effort, is_low_effort,
         # Time-varying control variables:
         #        hour_coinflip_local: Hour of day micro-ranodmization occurred in participant's local time
         #        days_between_v1_and_coinflip_local: Number of days between Visit 1 and micro-randomization
         #        any_response_2qs: whether (=1) or not (=0) any of the two-question survey items had a response
         #        Note: In a complete case analysis, you might end up dropping all rows
         #        which do not have any recent eligible decision point. 
         #        If that's the case, this variable is not needed in the 
         #        complete case MRT analysis model.
         hour_coinflip_local, days_between_v1_and_coinflip_local, any_response_2qs, any_recent_eligible_dp, engagement_most_recent_eligible,
         # Proximal outcome of interest:
         #        Y: engagement in self-regulatory strategies (same operationalization
         #           as in primary aim paper)
         Y)

# Change to TRUE if saving the dataset
if(FALSE){
  my_path <- "O:/d3c-BIDS/MARS/05 - Manipulated Data/Derived Engagement Measures"  # You can change this to your relevant file path
  saveRDS(dat_for_analysis, file = file.path(my_path, "dat_for_analysis.rds"))
}

################################################
# Analysis workflow used in the primary aim paper
# This is for primary aim complete case analysis

library(MRTAnalysis)
library(dplyr)
library(tidyr) # Only used for replace_na()

my_list <- list(Y = -1,
                coinflip = -1,
                hour_coinflip_local = -1, 
                days_between_v1_and_coinflip_local = -1,
                any_response_2qs = -1, 
                any_recent_eligible_dp = -1, 
                engagement_most_recent_eligible = -1,
                age = -1, 
                is_male = -1, 
                is_latino = -1, 
                is_not_latino_and_black = -1, 
                is_not_latino_and_other = -1, 
                baseline_tobacco_history = -1, 
                has_partner = -1, 
                income_val = -1)

dat_for_analysis_elig <- dat_for_analysis %>% filter(eligibility == 1)
dat_for_analysis_not_elig <- dat_for_analysis %>% filter(eligibility == 0)

logical_vec_cc <- dat_for_analysis_elig %>% 
  select(Y,
         hour_coinflip_local, days_between_v1_and_coinflip_local,
         any_response_2qs, any_recent_eligible_dp, engagement_most_recent_eligible,
         age, is_male, is_latino, is_not_latino_and_black, is_not_latino_and_other, 
         baseline_tobacco_history, has_partner, income_val) %>%
  complete.cases(.)

# Data manipulation trick when performing a complete case analysis:
# Note that this line is needed because we are doing a complete case analysis
# We would not need this line if we were doing a multiply imputed analysis
dat_for_analysis_elig <- dat_for_analysis_elig %>% mutate(eligibility = if_else(logical_vec_cc, eligibility, 0))


dat_for_analysis <- rbind(dat_for_analysis_elig, dat_for_analysis_not_elig)
dat_for_analysis <- dat_for_analysis %>% arrange(participant_id, decision_point)
dat_for_analysis <- dat_for_analysis %>% replace_na(my_list)

participants_fully_dropped <- dat_for_analysis %>% 
  group_by(participant_id) %>% 
  summarise(count_elig = sum(eligibility)) %>%
  arrange(count_elig) %>%
  filter(count_elig == 0) %>%
  .[["participant_id"]]

if(length(participants_fully_dropped) > 0){
  dat_for_analysis <- dat_for_analysis %>% filter(!(participant_id %in% participants_fully_dropped))
}

fit1 <- emee(
  data = dat_for_analysis,
  id = "participant_id",  
  outcome = "Y",
  treatment = "coinflip",
  rand_prob = 0.5,
  moderator_formula = ~ 1,  
  control_formula = ~ age + is_male + is_latino + is_not_latino_and_black + is_not_latino_and_other + baseline_tobacco_history + has_partner + income_val + hour_coinflip_local + days_between_v1_and_coinflip_local + any_response_2qs + any_recent_eligible_dp + engagement_most_recent_eligible, 
  availability = "eligibility"
)

summary(fit1, show_control_fit = TRUE)

# This is the output you should get
#$causal_excursion_effect
#            Estimate    95% LCL   95% UCL     StdErr  t_value df    p-value
#(Intercept) 0.105664 0.01061132 0.2007166 0.04771474 2.214493 75 0.02983248

# Estimated effect on the log risk ratio scale
dat_result_causal <- summary(fit1, show_control_fit = TRUE)[["causal_excursion_effect"]]
dat_result_control <- summary(fit1, show_control_fit = TRUE)[["control_variables"]]

dat_result <- rbind(dat_result_causal, dat_result_control)


dat_result_causal <- as.data.frame(dat_result_causal)

# Estimated effect on the risk ratio scale
exp_estimates <- exp(dat_result_causal[["Estimate"]])
rrLB95 <- exp(dat_result_causal[["95% LCL"]])
rrUB95 <- exp(dat_result_causal[["95% UCL"]])

dat_exp_scale <- data.frame(exp_estimates = exp_estimates, rrLB95 = c(rrLB95), rrUB95 = c(rrUB95))

print(dat_exp_scale)

# This is the output you should get
#> dat_exp_scale
#  exp_estimates   rrLB95   rrUB95
# 1      1.111448 1.010668 1.222278

