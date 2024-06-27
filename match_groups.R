match_groups <- function(data, grouping_variable, result_name) {
  
  library(tidyr)
  library(dplyr)
  library(readxl)
  library(MatchIt)
  
  # Create the first group variable
  data <- mutate(data, Group1 = ifelse(!!sym(grouping_variable) == 2, "0",
                                       ifelse(!!sym(grouping_variable) == 1, "1", NA)))
  
  # Create the second group variable
  data <- mutate(data, Group2 = ifelse(!!sym(grouping_variable) == 2, "0",
                                       ifelse(!!sym(grouping_variable) == 0, "1", NA)))
  
  # Ensure variables are in the correct format
  data <- mutate(data, sex = as.factor(sex))
  data <- mutate(data, demo_race_white = as.factor(demo_race_white))
  data <- mutate(data, Group1 = as.factor(Group1))
  data <- mutate(data, Group2 = as.factor(Group2))
  
  # Perform matching for the first group
  db_Group1 <- subset(data, !is.na(Group1))
  
  match_result1 <- matchit(
    Group1 ~ age_years_mri + demo_race_white,
    data = db_Group1,
    method = "optimal",
    exact = ~ sex,
    ratio = 1
  )
  
  matched_data1 <- match.data(match_result1)
  
  # Extract matched groups
  matched_group1_control <- matched_data1 %>% filter(Group1 == "0")
  matched_group1_treatment <- matched_data1 %>% filter(Group1 == "1")
  
  # Perform matching for the second group
  db_Group2 <- subset(data, !is.na(Group2))
  
  match_result2 <- matchit(
    Group2 ~ age_years_mri + demo_race_white,
    data = db_Group2,
    method = "optimal",
    exact = ~ sex,
    ratio = 1
  )
  
  matched_data2 <- match.data(match_result2)
  
  # Extract matched groups
  matched_group2_control <- matched_data2 %>% filter(Group2 == "0")
  matched_group2_treatment <- matched_data2 %>% filter(Group2 == "1")
  
  # Combine the matched data into a single dataframe
  matched_group1_control <- mutate(matched_group1_control, group_label = "ASD")
  matched_group1_treatment <- mutate(matched_group1_treatment, group_label = "HT")
  matched_group2_treatment <- mutate(matched_group2_treatment, group_label = "LT")
  
  combined_matched_data <- bind_rows(matched_group1_control, matched_group1_treatment, matched_group2_treatment)
  
  # Assign the result to the specified name
  assign(result_name, combined_matched_data, envir = .GlobalEnv)
  
  # Return the combined matched data
  return(combined_matched_data)
}