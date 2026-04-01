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
# EXAMPLE CODE: Merging app tracked data with EMA data

nrow(dat_primary_aim_another_subset) # Should be 4752

nrow(dat_primary_aim_another_subset_app_tracked_cols) # Should be 4752

intersect(x = colnames(dat_primary_aim_another_subset), y = colnames(dat_primary_aim_another_subset_app_tracked_cols)) # You should get "mars_id" and "decision_point"

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
# EXAMPLE CODE: Merging visit data with app
# tracked data with EMA data
# It is important to perform a LEFT JOIN
# rather than a RIGHT JOIN, INNER JOIN, or FULL JOIN
# since the Visit 1 dataset and the Visit 4 datasets
# each contains 111 participants and we only want
# visit data for the 99 participants who
# completed at least 3 EMA between days 2-9.
merged_ema_and_app_tracked <- left_join(x = merged_ema_and_app_tracked,
                                        y = v1_questionnaire,
                                        by = join_by("mars_id" == "mars_id"))

merged_ema_and_app_tracked <- left_join(x = merged_ema_and_app_tracked,
                                        y = v4_questionnaire,
                                        by = join_by("mars_id" == "mars_id"))

# Change to TRUE if saving the dataset
if(FALSE){
  my_path <- "O:/d3c-BIDS/MARS/05 - Manipulated Data/Derived Engagement Measures"  # You can change this to your relevant file path
  saveRDS(merged_ema_and_app_tracked, file = file.path(my_path, "merged_ema_and_app_tracked_and_visit_data.rds"))
}

################################################
# You may want to just deal with a few columns 
# to get your feet wet to start with.
# Here's one example of minimal columns you may want to first consider.
merged_ema_and_app_tracked_minimal_cols <- merged_ema_and_app_tracked %>%
  select(mars_id, 
         decision_point, 
         eligibility, 
         coinflip, 
         Y, # One particular operationalization of self-reported engagement; same as in primary aim paper
         tracked_engagement_combined) # One particular operationalization of app-tracked engagement, based on a combination of engagement with either high effort or low effort self-regulatory strategies.

# Change to TRUE if saving the dataset
if(FALSE){
  my_path <- "O:/d3c-BIDS/MARS/05 - Manipulated Data/Derived Engagement Measures"  # You can change this to your relevant file path
  saveRDS(merged_ema_and_app_tracked_minimal_cols, file = file.path(my_path, "merged_ema_and_app_tracked_minimal_cols.rds"))
}

