match_groups_gender <- function(data, id, grouping_variable, age, sex, gender, result_name) {
  
  # Create the first group variable
  data <- mutate(data, Group1 = ifelse(!!sym(grouping_variable) == 2, "0",
                                       ifelse(!!sym(grouping_variable) == 1, "1", NA)))
  
  # Create the second group variable
  data <- mutate(data, Group2 = ifelse(!!sym(grouping_variable) == 2, "0",
                                       ifelse(!!sym(grouping_variable) == 0, "1", NA)))
  
  # Ensure variables are in the correct format
  data <- mutate(data, {{ sex }} := as.factor(!!ensym(sex)))
  data <- mutate(data, {{ gender }} := as.factor(!!ensym(gender)))
  data <- mutate(data, Group1 = as.factor(Group1))
  data <- mutate(data, Group2 = as.factor(Group2))
  data <- mutate(data, {{ age }} := as.numeric(!!ensym(age)))
  
  match_with_fallback <- function(group_var, age_var, sex_var, gender_var, id_var, data) {
    group_var <- sym(group_var)
    age_var <- sym(age_var)
    sex_var <- sym(sex_var)
    gender_var <- sym(gender_var)
    id_var <- sym(id_var)
    
    # Filter out rows where treatment variable is missing
    data <- data %>% filter(!is.na(!!group_var))
    
    # Initial matching attempt with gender (exclude missing gender)
    match_result <- matchit(
      as.formula(paste(rlang::as_name(group_var), "~", rlang::as_name(age_var))),
      data = data %>% filter(!is.na(!!gender_var)),
      method = "optimal",
      exact = as.formula(paste("~", rlang::as_name(gender_var))),
      ratio = 1
    )
    matched_data <- match.data(match_result)
    
    # Identify unmatched ASD participants (Group1 == "0")
    unmatched_asd <- data %>% filter(!!group_var == "0" & !({{ id_var }} %in% matched_data[[rlang::as_name(id_var)]]))
    
    if (nrow(unmatched_asd) > 0) {
      # Include all treatment group participants for fallback matching (unmatched ASD + all treatments)
      remaining_data <- data %>% filter(!!group_var == "1" | (!!id_var %in% unmatched_asd[[rlang::as_name(id_var)]]))
      
      # Fallback matching for unmatched ASD participants with sex
      fallback_result <- matchit(
        as.formula(paste(rlang::as_name(group_var), "~", rlang::as_name(age_var))),
        data = remaining_data,
        method = "optimal",
        exact = as.formula(paste("~", rlang::as_name(sex_var))),
        ratio = 1
      )
      fallback_data <- match.data(fallback_result)
      
      # Adjust subclass labels to be continuous to add on to existing numbering system 
      max_subclass <- max(as.numeric(matched_data$subclass), na.rm = TRUE)
      fallback_data <- fallback_data %>% mutate(subclass = as.numeric(subclass) + max_subclass)
      # convert back to factor after addition
      fallback_data <- fallback_data %>% mutate(subclass = as.factor(subclass))
      
      # Combine matched data ensuring all controls (ASD participants) are included
      matched_data <- bind_rows(matched_data, fallback_data)
    }
    
    return(matched_data)
  }
  
  # Perform matching for the first group
  db_Group1 <- subset(data, !is.na(Group1))
  matched_data1 <- match_with_fallback("Group1", age, sex, gender, id, db_Group1)
  
  # Extract matched groups
  matched_group1_ASD <- matched_data1 %>% filter(Group1 == "0")
  matched_group1_HT <- matched_data1 %>% filter(Group1 == "1")
  
  # Perform matching for the second group
  db_Group2 <- subset(data, !is.na(Group2))
  matched_data2 <- match_with_fallback("Group2", age, sex, gender, id, db_Group2)
  
  # Extract matched groups
  matched_group2_ASD <- matched_data2 %>% filter(Group2 == "0")
  matched_group2_LT <- matched_data2 %>% filter(Group2 == "1")
  
  # Combine the matched data into a single dataframe
  matched_group1_ASD <- mutate(matched_group1_ASD, group_label = "ASD")
  matched_group1_HT <- mutate(matched_group1_HT, group_label = "HT")
  matched_group2_LT <- mutate(matched_group2_LT, group_label = "LT")
  
  combined_matched_data <- bind_rows(matched_group1_ASD, matched_group1_HT, matched_group2_LT)
  
  # Assign the result to the specified name
  assign(result_name, combined_matched_data, envir = .GlobalEnv)
  
  # Return the combined matched data
  return(combined_matched_data)
}
