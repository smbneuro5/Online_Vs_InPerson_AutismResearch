library(dplyr)

match_groups_DX <- function(data, id, grouping_variable, age, sex, gender, result_name) {
  # Convert input variables to symbols
  id <- rlang::ensym(id)
  grouping_variable <- rlang::ensym(grouping_variable)
  age <- rlang::ensym(age)
  sex <- rlang::ensym(sex)
  gender <- rlang::ensym(gender)
  
  # Define Group1 as ASD group (prolific_autism == 1) 
  data <- dplyr::mutate(data, Group1 = ifelse(prolific_autism == 1, "ASD", NA))
  
  # Define Group2 as ASD vs LT using the specified grouping variable
  data <- dplyr::mutate(data, Group2 = ifelse(!!grouping_variable == 2, "ASD", 
                                              ifelse(!!grouping_variable == 0, "LT", NA)))
  
  # Ensure variables are in the correct format
  data <- dplyr::mutate(data, 
                        !!sex := as.factor(!!sex),      # Convert sex to factor
                        !!gender := as.factor(!!gender), # Convert gender to factor
                        Group1 = as.factor(Group1),      # Convert Group1 to factor
                        Group2 = as.factor(Group2),      # Convert Group2 to factor
                        !!age := as.numeric(!!age))      # Convert age to numeric
  
  # Function to match LT group based on age and gender or sex
  match_with_fallback <- function(data_var, group_var, age_var, sex_var, gender_var, id_var) {
    group_var <- rlang::ensym(group_var)
    
    # Track matched comparison IDs
    used_comparison_ids <- c()
    
    # Function to perform nearest neighbor matching based on age difference
    nearest_neighbor_match <- function(reference_row, comparison, age_var, exact_var) {
      comparison_filtered <- dplyr::filter(comparison, !!exact_var == reference_row[[rlang::as_name(exact_var)]]) %>%
        dplyr::filter(!(.data[[rlang::as_name(id_var)]] %in% used_comparison_ids))
      
      if (nrow(comparison_filtered) > 0) {
        comparison_filtered <- dplyr::mutate(comparison_filtered, age_diff = abs(.data[[rlang::as_name(age_var)]] - reference_row[[rlang::as_name(age_var)]]))
        best_match <- dplyr::slice(comparison_filtered, which.min(age_diff))
        return(best_match)
      } else {
        return(NULL)  # No match found
      }
    }
    
    # ASD reference group
    reference <- dplyr::filter(data_var, !!group_var == "ASD")
    comparison <- dplyr::filter(data_var, !!group_var != "ASD")
    
    # Initialize matches dataframe and pair_id counter
    matches <- data.frame()
    pair_id_counter <- 1
    
    # Iterate over each participant in the ASD reference group
    for (i in seq_len(nrow(reference))) {
      reference_row <- reference[i, ]
      
      # Try matching on gender first
      best_match <- nearest_neighbor_match(reference_row, comparison, age_var, gender_var)
      
      # If no gender match is found, fall back to sex matching
      if (is.null(best_match)) {
        best_match <- nearest_neighbor_match(reference_row, comparison, age_var, sex_var)
      }
      
      # If a match is found, assign the pair_id and add to matches
      if (!is.null(best_match)) {
        reference_row <- dplyr::mutate(reference_row, pair_id = pair_id_counter)
        best_match <- dplyr::mutate(best_match, pair_id = pair_id_counter)
        
        matches <- dplyr::bind_rows(matches, reference_row, best_match)
        used_comparison_ids <- c(used_comparison_ids, best_match[[rlang::as_name(id_var)]])  # Track matched comparison IDs
        pair_id_counter <- pair_id_counter + 1  # Increment pair_id for the next match
      }
    }
    
    return(matches)
  }
  
  matched_group1_HT <- dplyr::filter(data, prolific_autism == 1)
  
  # Subset data to remove participants reporting ASD (prolific_autism == 1) from the LT group
  data <- dplyr::filter(data, !(prolific_autism == 1 & Group2 == "LT"))
  
  # Perform matching for the second group (ASD vs LT)
  db_Group2 <- dplyr::filter(data, !is.na(Group2))
  matched_data2 <- match_with_fallback(db_Group2, "Group2", age, sex, gender, id)
  
  # Extract matched groups: ASD participants from Group2, LT participants from Group2, and all ASD (Group1 == "ASD")
  matched_group2_ASD <- dplyr::filter(matched_data2, Group2 == "ASD")
  matched_group2_LT <- dplyr::filter(matched_data2, Group2 == "LT")
  
  # Label the groups
  matched_group2_ASD <- dplyr::mutate(matched_group2_ASD, group_label = "ASD")
  matched_group2_LT <- dplyr::mutate(matched_group2_LT, group_label = "LT")
  matched_group1_HT <- dplyr::mutate(matched_group1_HT, group_label = "Online_ASD")
  
  # Combine the matched data into a single dataframe
  
  # Combine the matched data into a single dataframe
  combined_matched_data <- dplyr::bind_rows(matched_group2_ASD, matched_group1_HT, matched_group2_LT)
  
  
  # Combine the matched data into a single dataframe
  
  # Assign the result to the specified name
  assign(result_name, combined_matched_data, envir = .GlobalEnv)
  
  # Return the combined matched data
  return(combined_matched_data)
}
