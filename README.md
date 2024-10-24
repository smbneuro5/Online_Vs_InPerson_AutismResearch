# Phenotypic divergence between individuals with self-reported autistic traits and clinically ascertained autism

Code and data repository for "Phenotypical divergence between self-reported and clinically ascertained autism"

"Create_Datasets" contains:
Full data file: FullDataset.xlsx
Matching function for main analysis: MatchGroups_GenderFirst.R
Matching function for supplemental online self-DX analysis: MatchGroups_GenderFirst_DX.R
Script to create matched samples: Matching.R

"Run_Analyses" contains:
Matched datafile (generated using Matching.R): Matched_Data_Cutoff_Gender.xlsx
Supplemental "matched" datafile with online self-DX (generated using Matching.R): Matched_Data_Cutoff_Gender_DX.xlsx
Supplemental datafile with full unmatched "high-trait" and "low-trait" groups (generated using Matching.R): Full_Data_Cutoff.xlsx
Main script: Main_Analyses_PhenoDivergeASD.R
Helper plotting function: SummarySE.R
