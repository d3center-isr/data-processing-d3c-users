library(dplyr)
library(readxl)


path_ema <- "O:/d3c-BIDS/MARS/04 - Original Data/2025-6-25-MARS Curated Data/EMA/Release v2.0.0/Data for analysis"

dat_primary_aim <- readRDS(file.path(path_ema, "data_final.RDS"))

# Here is where you can view details of each variable in the dataset
ema_codebook <- read_xlsx(file.path(path_ema, "codebook.xlsx"))

library(dplyr)

dat_primary_aim <- dat_primary_aim %>% mutate(eligibility = 1* (ep_emi_rand != ""))

dat_primary_aim %>%
  filter(eligibility== 1) %>%
  .[["mars_id"]] %>%
  unique(.) %>%
  length(.)  # You should arrive at 110 ppl with >=1 microrand

dat_primary_aim <- dat_primary_aim %>% mutate(coinflip = 1*(ep_emi_rand %in% c("mars", "low_effort")))
dat_primary_aim <- dat_primary_aim %>% mutate(decision_point = 6*(ec_study_day_int - 1) + (ec_block_calc + 1))

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


