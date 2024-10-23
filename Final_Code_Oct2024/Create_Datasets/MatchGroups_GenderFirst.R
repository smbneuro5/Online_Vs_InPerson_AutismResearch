library(dplyr)

match_groups <- function(data, id, grouping_variable, age, sex, gender, result_name) {
  # Convert input variables to symbols
  id <- rlang::ensym(id)
  grouping_variable <- rlang::ensym(grouping_variable)
  age <- rlang::ensym(age)
  sex <- rlang::ensym(sex)
  gender <- rlang::ensym(gender)
  
  # Create group variables in the dataset (Group1: ASD vs HT, Group2: ASD vs LT)
  data <- dplyr::mutate(data,
                        Group1 = dplyr::case_when(!!grouping_variable == 2 ~ "ASD",
                                                  !!grouping_variable == 1 ~ "HT"),
                        Group2 = dplyr::case_when(!!grouping_variable == 2 ~ "ASD",
                                                  !!grouping_variable == 0 ~ "LT"),
                        !!sex := as.factor(!!sex),
                        !!gender := as.factor(!!gender),
                        Group1 = as.factor(Group1),
                        Group2 = as.factor(Group2),
                        !!age := as.numeric(!!age))
  
  match_with_fallback <- function(data_var, group_var, age_var, sex_var, gender_var, id_var) {
    group_var <- rlang::ensym(group_var)
    
    # Initialize variables to track matched IDs
    used_comparison_ids <- c()  # Track matched comparison participants
    
    # Function to find the nearest neighbor match based on age difference
    nearest_neighbor_match <- function(reference_row, comparison, age_var, exact_var) {
      comparison_filtered <- dplyr::filter(comparison, !!exact_var == reference_row[[rlang::as_name(exact_var)]]) %>%
        dplyr::filter(!(.data[[rlang::as_name(id_var)]] %in% used_comparison_ids))
      
      if (nrow(comparison_filtered) > 0) {
        # Calculate age difference and find the closest match
        comparison_filtered <- dplyr::mutate(comparison_filtered, age_diff = abs(.data[[rlang::as_name(age_var)]] - reference_row[[rlang::as_name(age_var)]]))
        best_match <- dplyr::slice(comparison_filtered, which.min(age_diff))
        return(best_match)
      } else {
        return(NULL)  # No match found
      }
    }
    
    # ASD reference group (no pair_id assigned upfront, assigned after matching)
    reference <- dplyr::filter(data_var, !!group_var == "ASD")
    comparison <- dplyr::filter(data_var, !!group_var != "ASD")
    
    # Initialize an empty dataframe to store matched pairs
    matches <- data.frame()
    
    # Initialize pair_id at 1
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
  
  # Perform matching for the first group (ASD vs HT)
  db_Group1 <- dplyr::filter(data, !is.na(Group1))
  matched_data1 <- match_with_fallback(db_Group1, "Group1", age, sex, gender, id)
  
  # Perform matching for the second group (ASD vs LT) with a reset pair_id
  db_Group2 <- dplyr::filter(data, !is.na(Group2))
  matched_data2 <- match_with_fallback(db_Group2, "Group2", age, sex, gender, id)
  
  # Extract matched groups (ASD participants only from the first match, HT from first, LT from second)
  matched_group1_ASD <- dplyr::filter(matched_data1, Group1 == "ASD")
  matched_group1_HT <- dplyr::filter(matched_data1, Group1 == "HT")
  matched_group2_LT <- dplyr::filter(matched_data2, Group2 == "LT")
  
  # Label the groups
  matched_group1_ASD <- dplyr::mutate(matched_group1_ASD, ASD_group = "ASD")
  matched_group1_HT <- dplyr::mutate(matched_group1_HT, ASD_group = "HT")
  matched_group2_LT <- dplyr::mutate(matched_group2_LT, ASD_group = "LT")
  
  # Combine the matched data into a single dataframe, without duplicated ASD subjects
  combined_matched_data <- dplyr::bind_rows(matched_group1_ASD, matched_group1_HT, matched_group2_LT)
  
  # Assign the result to the specified name
  assign(result_name, combined_matched_data, envir = .GlobalEnv)
  
  # Return the combined matched data
  return(combined_matched_data)
}
