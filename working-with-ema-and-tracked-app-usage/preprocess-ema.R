library(dplyr)
library(lubridate)
library(readxl)

path_ema <- "O:/d3c-BIDS/MARS/04 - Original Data/2025-6-25-MARS Curated Data/EMA/Release v2.0.0/Data for analysis"

dat_primary_aim <- readRDS(file.path(path_ema, "data_final.RDS"))

# Here is where you can view details of each variable in the dataset
ema_codebook <- read_xlsx(file.path(path_ema, "codebook.xlsx"))

library(dplyr)

dat_primary_aim <- dat_primary_aim %>% mutate(eligibility = if_else(is.na(ep_emi_rand), 0, 1))

dat_primary_aim %>%
  filter(eligibility== 1) %>%
  .[["mars_id"]] %>%
  unique(.) %>%
  length(.)  # You should arrive at 110 ppl with >=1 microrand

dat_primary_aim <- dat_primary_aim %>% 
  mutate(coinflip = case_when(
    ep_emi_rand == "mars" ~ 1,
    ep_emi_rand == "low_effort" ~ 1,
    ep_emi_rand == "none" ~ 0,
    .default = NULL),
    is_high_effort = case_when(
      ep_emi_rand == "mars" ~ 1,
      ep_emi_rand == "low_effort" ~ 0,
      ep_emi_rand == "none" ~ 0,
      .default = NULL),
    is_low_effort = case_when(
      ep_emi_rand == "mars" ~ 0,
      ep_emi_rand == "low_effort" ~ 1,
      ep_emi_rand == "none" ~ 0,
      .default = NULL)) %>%
  mutate(hour_coinflip_local = hour(ep_rand_hrts_local) + minute(ep_rand_hrts_local)/60 + second(ep_rand_hrts_local)/(60*60)) %>%
  mutate(days_between_v1_and_coinflip_local = int_length(v_first_day_hrts_local %--% ep_rand_hrts_local)/(60*60*24))

dat_primary_aim <- dat_primary_aim %>% mutate(decision_point = 6*(ec_study_day_int - 1) + (ec_block_calc + 1))

dat_primary_aim %>%
  group_by(eligibility, coinflip) %>%
  summarise(n_decision_points = n())

# You should get the following output:
#           eligibility   coinflip        n_decision_points
#           <dbl>         <dbl>           <int>
# 1           0           NA              1879
# 2           1           0               2382
# 3           1           1               2339

dat_primary_aim %>%
  group_by(eligibility, is_high_effort, is_low_effort) %>%
  summarise(n_decision_points = n())

# You should get the following output:
#             eligibility   is_high_effort    is_low_effort   n_decision_points
#             <dbl>         <dbl>             <dbl>           <int>
# 1           0             NA                NA              1879
# 2           1              0                0               2382
# 3           1              0                1               1200
# 4           1              1                0               1139


################################################################################
# Construct the EMA-based proximal outcome for engagement with 
# self-regulatory strategies
# Note that this was the same function used to construct this variable for the
# primary aim paper.
construct_primary_proximal_outcome <- function(cleaned_data_frame, q1_var_name, q2_var_name, q3_var_name){
  
  q1_response_value <- cleaned_data_frame[[q1_var_name]]
  q2_response_value <- cleaned_data_frame[[q2_var_name]]
  q3_response_value <- cleaned_data_frame[[q3_var_name]]
  
  primary_proximal_outcome_value <- case_when(
    !is.na(q1_response_value) & !is.na(q2_response_value) & !is.na(q3_response_value) & (q1_response_value=="Yes" | q2_response_value=="Yes" | q3_response_value=="Yes") ~ 1,
    !is.na(q1_response_value) & !is.na(q2_response_value) & !is.na(q3_response_value) & (q1_response_value=="No" & q2_response_value=="No" & q3_response_value=="No") ~ 0,
    !is.na(q1_response_value) & (is.na(q2_response_value) | is.na(q3_response_value)) & (q1_response_value=="Yes") ~ 1,
    !is.na(q2_response_value) & (is.na(q1_response_value) | is.na(q3_response_value)) & (q2_response_value=="Yes") ~ 1,
    !is.na(q3_response_value) & (is.na(q1_response_value) | is.na(q2_response_value)) & (q3_response_value=="Yes") ~ 1,
    .default = NULL
  )
  
  return(primary_proximal_outcome_value)
}

dat_primary_aim[["Y"]] <- construct_primary_proximal_outcome(cleaned_data_frame = dat_primary_aim, 
                                                             q1_var_name = "ei_mars_use", 
                                                             q2_var_name = "ei_mars_other_use", 
                                                             q3_var_name = "ei_non_mars_use")

################################################################################
# We create a binary indicator for whether the 2qs had any response

# ei_2qs_cig_avail is a binary indicator for whether cigarettes were available
# ei_2qs_negaff is a binary indicator for whether negative affect was present
dat_primary_aim %>% 
  group_by(ei_2qs_cig_avail, ei_2qs_negaff) %>% 
  summarise(count = n())

# You should get the following output:
#                  ei_2qs_cig_avail     ei_2qs_negaff     count
#                  <dbl>                <dbl>             <int>
# 1                0                    0                 1023
# 2                0                    1                 148
# 3                1                    0                 1337
# 4                1                    1                 265
# 5               NA                    NA                3827

# From the output above, either participants completed both items in the 
# two-question survey, or they completed none of the items in the 
# two-question survey.
dat_primary_aim <- dat_primary_aim %>%
  mutate(any_response_2qs = if_else((!is.na(ei_2qs_cig_avail)) & (!is.na(ei_2qs_negaff)), 1, 0))

dat_primary_aim %>%
  group_by(any_response_2qs, eligibility, coinflip) %>%
  summarise(n_decision_points = n())

# You should get the following output:
#                  any_response_2qs     eligibility     coinflip          n_decision_points
#                  <dbl>                <dbl>           <dbl>             <int>
# 1                0                    0               NA                1870
# 2                0                    1               0                 1009
# 3                0                    1               1                 948
# 4                1                    0               NA                9
# 5                1                    1               0                 1373
# 6                1                    1               1                 1391

################################################################################
# In this for loop, we identify the most recent eligible decision point BEFORE
# the current decision point
dat_primary_aim[["decision_points_most_recent_eligible"]] <- NA
list_all_dat <- list()

all_ids <- unique(dat_primary_aim[["mars_id"]])
n_ids <- length(all_ids)

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_primary_aim %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  arr_all_elig_indices <- which(dat_current_participant[["eligibility"]] == 1)
  
  for(current_row_number in 1:n_blocks){
    is_rand <- if_else(!is.na(dat_current_participant[current_row_number,"coinflip"]), TRUE, FALSE)
    
    if(is_rand == TRUE){
      arr_found <- arr_all_elig_indices < current_row_number
      any_found <- if_else(sum(arr_found) == 0, 0, 1)
      if(any_found == 1){
        these_indices_found <- arr_all_elig_indices[arr_found]
        the_matched_index <- max(these_indices_found)
      }else{
        the_matched_index <- NA_real_
      }
      dat_current_participant[["decision_points_most_recent_eligible"]][current_row_number] <- the_matched_index
    }
  }
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_primary_aim <- bind_rows(list_all_dat)

################################################################################
# We will create a variable for reported engagement in self-regulatory 
# strategies at the most recent eligible decision point,
# randomized assignment at the most recent eligible decision point,
# hours elapsed since most recent eligible decision point

dat_primary_aim[["any_recent_eligible_dp"]] <- if_else(!is.na(dat_primary_aim[["decision_points_most_recent_eligible"]]), 1, 0)
dat_primary_aim[["engagement_most_recent_eligible"]] <- NA_real_
dat_primary_aim[["coinflip_most_recent_eligible"]] <- NA_real_
dat_primary_aim[["hours_elapsed_since_most_recent_eligible"]] <- NA_real_

list_all_dat <- list()

for(i in 1:n_ids){
  current_participant <- all_ids[i]
  dat_current_participant <- dat_primary_aim %>% filter(mars_id == current_participant)
  n_blocks <- nrow(dat_current_participant)
  
  for(j in 1:n_blocks){
    if(dat_current_participant[j,"any_recent_eligible_dp"] == 1){
      this_idx <- as.numeric(dat_current_participant[j,"decision_points_most_recent_eligible"])
      dat_current_participant[j,"engagement_most_recent_eligible"] <- dat_current_participant %>% filter(decision_point == this_idx) %>% .[["Y"]]
      dat_current_participant[j,"coinflip_most_recent_eligible"] <- dat_current_participant %>% filter(decision_point == this_idx) %>% .[["coinflip"]]
      past_timestamp <- dat_current_participant %>% filter(decision_point == this_idx) %>% .[["ep_rand_hrts_local"]]
      present_timestamp <- dat_current_participant[j,"ep_rand_hrts_local"]
      hours_elapsed_between_timestamps <- int_length(past_timestamp %--% present_timestamp)/(60*60)
      dat_current_participant[j,"hours_elapsed_since_most_recent_eligible"] <- hours_elapsed_between_timestamps
    }else{
      ##########################################################################
      # We will be using Method 1 recommended by Dziak and Henry in the paper
      # cited below where we have an indicator for whether a variable 
      # is relevant (this is any_recent_eligible_dp in our case)
      # and the numeric value of the variable when it 
      # is relevant (this is engagement_most_recent_eligible in our case)
      # In line with Dziak and Henry's Method 1, we will include both of these 
      # variables in our regression model and code 
      # engagement_most_recent_eligible as 0 whenever any_recent_eligible_dp 
      # is zero
      # 
      # Paper:
      # * Dziak, J. J., & Henry, K. L. (2017). Two-part predictors in regression models. Multivariate behavioral research, 52(5), 551-561.
      ##########################################################################
      dat_current_participant[j,"engagement_most_recent_eligible"] <- 0
      dat_current_participant[j,"coinflip_most_recent_eligible"] <- 0
      dat_current_participant[j,"hours_elapsed_since_most_recent_eligible"] <- 0
    }
  }
  
  list_all_dat <- append(list_all_dat, list(dat_current_participant))
}

dat_primary_aim <- bind_rows(list_all_dat)

###############################################################################
# Here, we remove rows corresponding to the first day (decision points 1-6) 
# and last day (decision points 55-60) of the MRT
dat_primary_aim_subset <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

dat_primary_aim_subset <- dat_primary_aim_subset %>%
  group_by(mars_id) %>%
  mutate(ep_status_sum = sum(ep_status == "completed")) %>%
  ungroup(.)

dat_primary_aim_another_subset <- dat_primary_aim_subset %>% filter(ep_status_sum >=3)

dat_primary_aim_another_subset %>%
  .[["mars_id"]] %>%
  unique(.) %>%
  length(.) # What is this number now? You should get 99

dat_primary_aim_another_subset %>% 
  nrow(.) # What is this number now? You should get 4752

dat_primary_aim_another_subset %>% 
  filter(eligibility == 1) %>%
  nrow(.) # What is this number now? You should get 3858


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

dat_primary_aim %>%
  group_by(any_response_2qs, eligibility, coinflip) %>%
  summarise(n_decision_points = n())

# You should get the following output:
#                  any_response_2qs   eligibility       coinflip        n_decision_points
#                  <dbl>              <dbl>             <dbl>           <int>
# 1                0                  0                 NA              1870
# 2                0                  1                 0               1009
# 3                0                  1                 1               948
# 4                1                  0                 NA              9
# 5                1                  1                 0               1373
# 6                1                  1                 1               1391


