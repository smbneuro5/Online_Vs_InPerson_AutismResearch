library(tidyr)
library(dplyr)
library(readxl)
library(MatchIt)

#### Load the data ####
db <- read_excel("FullDataset.xlsx")

#### Load the matching function ####
source("MatchGroups_GenderFirst.R")
source("MatchGroups_GenderFirst_DX.R")

#### Get the final datasets ####

# Match the groups - for main analyses, n=56 per group
match_groups(db, "sub_id", "ASD_group_cutoff", "age", "sex", "Gender_ID", "Matched_Data_Cutoff_Gender")

# For supplemental analyses, full groups (HT = above BAPQ cutoff, LT = bottom quartile on BAPQ)
Full_Data_Cutoff <- subset(db, !is.na(ASD_group_cutoff))

# For supplemental analyses, compare online participants with self-reported diagnosis
# in-person ASD vs online ASD vs LT
# To match within LT, remove any individuals who self-report a DX 
match_groups_DX(db, "sub_id", "ASD_group_cutoff", "age", "sex", "Gender_ID", "Matched_Data_Cutoff_Gender_DX")

# Export Data
writexl::write_xlsx(Matched_Data_Cutoff_Gender, 'Matched_Data_Cutoff_Gender2.xlsx')
writexl::write_xlsx(Full_Data_Cutoff, 'Full_Data_Cutoff.xlsx')
writexl::write_xlsx(Matched_Data_Cutoff_Gender_DX, 'Matched_Data_Cutoff_Gender_DX.xlsx')

