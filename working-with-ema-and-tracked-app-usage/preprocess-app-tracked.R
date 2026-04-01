library(dplyr)

path_app_tracked <- "O:/d3c-BIDS/MARS/05 - Manipulated Data/Derived Engagement Measures"

dat_primary_aim <- readRDS(file.path(path_app_tracked, "dat_primary_aim_engagement_measures.rds"))

# Here is where you can view details of each variable in the dataset
app_tracked_codebook <- read.csv(file.path(path_app_tracked, "description_dat_primary_aim_engagement_measures.csv"))

dat_primary_aim <- dat_primary_aim %>%
  mutate(engagement_combined = case_when(
    eligibility == 0 ~ 0,
    (eligibility == 1) & (!is.na(Y)) ~ Y,
    (eligibility == 1) & (is.na(Y)) ~ 0,
    .default = NULL
  )) %>%
  # Here are examples of various ways one can combine the different app tracked indicators
  mutate(tracked_engagement_combined = case_when(
    eligibility == 0 ~ 0,
    (eligibility == 1) & (in_mars==1 | in_tips==1) ~ 1,
    (eligibility == 1) & (in_mars==0 & in_tips==0) ~ 0,
    .default = NULL
  )) %>%
  mutate(tracked_engagement_done_combined = case_when(
    eligibility == 0 ~ 0,
    (eligibility == 1) & (activ_done==1 | read_tips==1) ~ 1,
    (eligibility == 1) & (activ_done==0 & read_tips==0) ~ 0,
    .default = NULL
  )) %>%
  # In this example, if the participant very quickly went through the low effort message
  # operationalized via reading speed being faster than a specified cutoff.
  # Based on Yu et al.'s forthcoming paper, the cutoff for "proper engagement" is AT LEAST 340 words per minute, or about 5.67 words per second.
  mutate(tracked_engagement_done_cutoff_combined = case_when(
    eligibility == 0 ~ 0,
    (eligibility == 1) & activ_done==1 ~ 1,
    (eligibility == 1) & (activ_done==0 & read_tips==1 & tips_comp_wps <= 340/60) ~ 1,
    (eligibility == 1) & (activ_done==0 & read_tips==1 & tips_comp_wps > 340/60) ~ 0,
    (eligibility == 1) & (activ_done==0 & read_tips==0) ~ 0,
    .default = NULL
  )) %>%
  mutate(ema_with_tracked_engagement_combined = if_else((engagement_combined==1)|(tracked_engagement_combined==1), 1, 0),
         ema_with_tracked_engagement_done_combined = if_else((engagement_combined==1) | (tracked_engagement_done_combined==1), 1, 0),
         ema_with_tracked_engagement_done_cutoff_combined = if_else((engagement_combined==1) | (tracked_engagement_done_cutoff_combined==1), 1, 0))


dat_primary_aim_another_subset <- dat_primary_aim %>% filter((decision_point >= 7) & (decision_point <= 54))

dat_primary_aim_another_subset %>%
  .[["mars_id"]] %>%
  unique(.) %>%
  length(.) # What is this number now? You should get 99

dat_primary_aim_another_subset %>% 
  nrow(.) # What is this number now? You should get 4752

dat_primary_aim_another_subset %>% 
  filter(eligibility == 1) %>% 
  nrow(.) # What is this number now? You should get 3858

# Some column names are commented out as they are redundant with some columns in the file "O:/d3c-BIDS/MARS/04 - Original Data/2025-6-25-MARS Curated Data/EMA/Release v2.0.0/Data for analysis/data_final.RDS"

only_these_cols <- c( "mars_id",                                         
                      "participant_id", # R packages like geeglm use numeric values for ID; you may want to use "participant_id" rather than "mars_id" when working with geeglm                                
                      #"cluster_id",                                      
                      "decision_point",                                  
                      #"eligibility",                                     
                      #"coinflip",                                        
                      #"is_high_effort",                                  
                      #"is_low_effort",                                   
                      "Y",                                               
                      #"olson",                                           
                      #"ts_coinflip_mountain",                            
                      #"ts_coinflip_local",                               
                      "ts_emi_resp",                                     
                      "emi_resp",                                        
                      "num_page",                                        
                      "num_click",                                       
                      "in_mars",                                         
                      "activ_started",                                   
                      "activ_done",                                      
                      "activ_done_m",                                    
                      "time_spent",                                      
                      "num_page_p",                                      
                      "num_click_p",                                     
                      "in_mars_p",                                       
                      "activ_started_p",                                 
                      "activ_done_p",                                    
                      "activ_done_m_p",                                  
                      "time_spent_p",                                    
                      "num_message",                                     
                      "in_tips",                                         
                      "read_tips",                                       
                      "read_tips_m",                                     
                      "time_spent_tips",                                 
                      "tips_comp_time",                                  
                      "tips_comp_wps",                                   
                      "num_message_p",                                   
                      "in_tips_p",                                       
                      "read_tips_p",                                     
                      "read_tips_m_p",                                   
                      "time_spent_tips_p",                               
                      "tips_comp_time_p",                                
                      "tips_comp_wps_p",                                 
                      "num_page_preblock",                               
                      "num_click_preblock",                              
                      "in_mars_preblock",                                
                      "activ_started_preblock",                          
                      "activ_done_preblock",                             
                      "activ_done_m_preblock",                           
                      "time_spent_preblock",                             
                      "num_message_preblock",                            
                      "in_tips_preblock",                                
                      "read_tips_preblock",                              
                      "read_tips_m_preblock",                            
                      "time_spent_tips_preblock",                        
                      "tips_comp_time_preblock",                         
                      "tips_comp_wps_preblock",                          
                      "engagement_combined",                             
                      "tracked_engagement_combined",                     
                      "tracked_engagement_done_combined",                
                      "tracked_engagement_done_cutoff_combined",         
                      "ema_with_tracked_engagement_combined",            
                      "ema_with_tracked_engagement_done_combined",       
                      "ema_with_tracked_engagement_done_cutoff_combined")


dat_primary_aim_another_subset_app_tracked_cols <- dat_primary_aim_another_subset %>% select(all_of(only_these_cols))

