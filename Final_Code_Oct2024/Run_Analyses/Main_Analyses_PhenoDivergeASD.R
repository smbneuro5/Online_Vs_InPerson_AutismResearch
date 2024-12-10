### Data Analysis for "Phenotypic divergence between individuals with self-reported 
### ...autistic traits and clinically ascertained autism" 

#Code written by Sarah Banker, 2024

#### Step 1 - Load libraries ####
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(multcomp)
library(emmeans)
library(lmerTest)
library(effectsize)
library(parameters)
library(merDeriv)
library(car)

#### Step 2 - Load functions and data ####
source("SummarySE.R") #To help with bar plotting

db_full_matched <- read_excel("Matched_Data_Cutoff_Gender.xlsx") #data

#make sure variables are in proper format
db_full_matched <- mutate(db_full_matched, rejR_L_ic = as.numeric(rejR_L_ic))
db_full_matched <- mutate(db_full_matched, rejR_M_ic = as.numeric(rejR_M_ic))
db_full_matched <- mutate(db_full_matched, rejR_H_ic = as.numeric(rejR_H_ic))
db_full_matched <- mutate(db_full_matched, pc_ic = as.numeric(pc_ic))
db_full_matched <- mutate(db_full_matched, pc_nc = as.numeric(pc_nc))
db_full_matched <- mutate(db_full_matched, race = as.factor(race))
db_full_matched <- mutate(db_full_matched, demo_ethnicity_latino_hispanic = as.factor(demo_ethnicity_latino_hispanic))
db_full_matched <- mutate(db_full_matched, Gender_ID = as.factor(Gender_ID))
db_full_matched <- mutate(db_full_matched, ASD_group = as.factor(ASD_group))


##### Split dataframe by group ####
db_full_matched_groups <- split(db_full_matched, db_full_matched$ASD_group)
db_full_ASD <- db_full_matched_groups$ASD
db_full_HT <- db_full_matched_groups$HT
db_full_LT <- db_full_matched_groups$LT

#### Step 3 - Compare demographics ####

## Age
aggregate(age ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

  # test for normality
  shapiro.test(db_full_ASD$age)
  shapiro.test(db_full_HT$age)
  shapiro.test(db_full_LT$age)
  
  # not normally distributed, use non-parametric stats
  kruskal.test(age ~ ASD_group, data = db_full_matched) # 3-Group difference?

## Sex
aggregate(sex ~ ASD_group, db_full_matched, FUN = table)

  # Chi-Square Test
  chisq.test(table(db_full_matched$sex, db_full_matched$ASD_group))

## Gender
db_full_matched %>%
  dplyr::group_by(ASD_group) %>%
  dplyr::summarise(Gender_ID = toString(apply(as.data.frame(table(Gender_ID)), 1, function(row) {
    paste(row[1], row[2], sep = ": ")
  }))) %>%
  ungroup()

  # Chi-Square Test with Monte Carlo simulation for more accurate p-value estimation due to small freq.
  chisq.test(table(db_full_matched$Gender_ID, db_full_matched$ASD_group), simulate.p.value = TRUE, B = 10000)

## In-person IQ
db_full_matched <- mutate(db_full_matched, laboratory_IQ = ifelse(!is.na(wasi_fsiq2_composite), wasi_fsiq2_composite,
                                                                               ifelse(!is.na(wais_scoring_conv_comp_full_scale), wais_scoring_conv_comp_full_scale, NA)))

  mean(db_full_matched$laboratory_IQ, na.rm=T)
  sd(db_full_matched$laboratory_IQ, na.rm=T)

## Online cognitive ability
aggregate(icar_total ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

  # test for normality
  shapiro.test(db_full_HT$icar_total)
  shapiro.test(db_full_LT$icar_total)
  
  # not normally distributed, use non-parametric stats
  kruskal.test(icar_total ~ ASD_group, data = db_full_matched) #only two groups

## Race
db_full_matched %>%
  dplyr::group_by(ASD_group) %>%
  dplyr::summarise(race = toString(apply(as.data.frame(table(race)), 1, function(row) {
    paste(row[1], row[2], sep = ": ")
  }))) %>%
  ungroup()

aggregate(race ~ ASD_group, db_full_matched, FUN =table )

  # Chi-Square Test with Monte Carlo simulation for more accurate p-value estimation due to small freq.
  chisq.test(table(db_full_matched$race, db_full_matched$ASD_group), simulate.p.value = TRUE, B = 10000)

## Ethnicity 
aggregate(demo_ethnicity_latino_hispanic ~ ASD_group, db_full_matched, FUN =table )
  
  # Chi-Square Test 
  chisq.test(table(db_full_matched$demo_ethnicity_latino_hispanic, db_full_matched$ASD_group))

## Employment
aggregate(empoyment ~ ASD_group, db_full_matched, FUN =table )

  # Chi-Square Test 
  chisq.test(table(db_full_matched$empoyment, db_full_matched$ASD_group))

## Income
db_full_matched$household_income <- factor(db_full_matched$household_income, levels = c("10-50k", "50-100k",">100k"))
aggregate(household_income ~ ASD_group, db_full_matched, FUN =table )

  # Chi-Square Test 
  chisq.test(table(db_full_matched$household_income, db_full_matched$ASD_group))

## Education
db_full_matched$education <- factor(db_full_matched$education, levels = c("Less than high school", "Partial high school","High school", "Some college",
                                                "College", "Graduate School"))
aggregate(education ~ ASD_group, db_full_matched, FUN =table )

  # Chi-Square Test with Monte Carlo simulation for more accurate p-value estimation due to small freq.
  chisq.test(table(db_full_matched$education, db_full_matched$ASD_group), simulate.p.value = TRUE, B = 10000)
  
#### Step 4 - Compare symptoms ####

#### ~ Step 4.1 - Group difference in symptoms?  ~####

#### ~~ ASD symptoms (BAPQ) ~~ ####
aggregate(bapq_score ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))
  
#Mixed-Effects Model with random intercept for pair
fit <- lmer(bapq_score ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))

# Plot ASD Symptom Scores
sum_BAPQ_Symptoms <- summarySE(db_full_matched, measurevar="bapq_score", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum_BAPQ_Symptoms, aes(y=bapq_score, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Self-Reported ASD Traits") +
  geom_errorbar(aes(ymin=bapq_score-se, ymax=bapq_score+se),
                width=.2, position=position_dodge(.85)) +
  #scale_x_discrete(labels=c("ASD", "HT","Low-trait")) +
  geom_jitter(data=db_full_matched, aes(y=bapq_score, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  coord_cartesian(ylim=c(0,6)) +
  guides(fill=FALSE, color =F) +
  theme_classic(base_size = 18)  # white background

#### ~~ Social Anxiety symptoms (LSAS) ~~ ####
aggregate(lsas_av_score ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit <- lmer(lsas_av_score ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))

# Plot Social Anxiety Symptom Scores
sum_social_anxiety <- summarySE(db_full_matched, measurevar="lsas_av_score", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum_social_anxiety, aes(y=lsas_av_score, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  #scale_fill_manual(values=c("#008F7A","#0081CF", "#845EC2"), name="Group") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Self-Reported SA Symptoms") +
  geom_errorbar(aes(ymin=lsas_av_score-se, ymax=lsas_av_score+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("ASD", "HT","Low-trait")) +
  geom_jitter(data=db_full_matched, aes(y=lsas_av_score, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color =F) +
  theme_classic(base_size = 18)  # white background

#### ~~ Avoidant Personality Disorder (AVPD; APDIS) symptoms (LSAS) ~~ ####
aggregate(apdis_score ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit <- lmer(apdis_score ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))

# Plot Avoidant Symptom Scores
sum_avpd <- summarySE(db_full_matched, measurevar="apdis_score", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum_avpd, aes(y=apdis_score, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  #scale_fill_manual(values=c("#008F7A","#0081CF", "#845EC2"), name="Group") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Self-Reported AVPD Symptoms") +
  geom_errorbar(aes(ymin=apdis_score-se, ymax=apdis_score+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("ASD", "HT","Low-trait")) +
  geom_jitter(data=db_full_matched, aes(y=apdis_score, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color =F) +
  theme_classic(base_size = 18)  # white background

#### ~ Step 4.2 - Symptom agreement within ASD ~ ####

# Calculate sub-scales to match ADOS
# For BAPQ, combine pragmatic language and aloof subscores into one "Social" score
db_full_ASD$bapq_SA <- (db_full_ASD$bapq_prag_lang_score + db_full_ASD$bapq_aloof_score)/2

# test for normality #
shapiro.test(db_full_ASD$bapq_score)
shapiro.test(db_full_ASD$ados4natot)
shapiro.test(db_full_ASD$bapq_SA)
shapiro.test(db_full_ASD$bapq_rigid_score)

# Test for agreement
# Overall agreement (total scores)

fit <- glm(bapq_score ~ ados4natot + age + sex, data=db_full_ASD) # regression model
summary(fit)

eta_squared(fit, partial = TRUE) # effect size and CI

# plot the relationship
ggplot(db_full_ASD, aes(y=ados4natot, x=bapq_score)) + 
  geom_point(size=3,  color="#03A28A") + 
  labs(x = "Self-Reported ASD Traits", y = "Clinician-Rated ASD Symptoms") + 
  #coord_cartesian(ylim=c(-2.5,2.5), xlim=c(-2.8,2.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method='lm', color="Black", se=T, size=.5) +
  theme_classic(base_size = 18) 

# Sub-scale agreement 

fit <- glm(bapq_rigid_score ~ ados4narrb + age + sex, data=db_full_ASD) #regression model
summary(fit)

eta_squared(fit, partial = TRUE) # effect size and CI

# plot the relationship
ggplot(db_full_ASD, aes(y=ados4narrb, x=bapq_rigid_score)) + 
  geom_point(size=3,  color="#03A28A") + 
  labs(x = "Self-Reported RRB Traits", y = "Clinician-Rated RRB Symptoms") + 
  #scale_color_manual(values=c("navy","slateblue1"), name="ASD Group",
  #labels=c("TD","ASD")) +
  #coord_cartesian(ylim=c(-2.5,2.5), xlim=c(-2.8,2.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method='lm', color="Black", se=T, size=.5) +
  theme_classic(base_size = 18) 


fit <- glm(bapq_SA ~ ados4nas + age + sex , data=db_full_ASD) #regression model
summary(fit)

eta_squared(fit, partial = TRUE) # effect size and CI

# plot the relationship
ggplot(db_full_ASD, aes(y=ados4nas, x=bapq_SA)) + 
  geom_point(size=3,  color="#03A28A") + 
  labs(x = "Self-Reported Social Traits", y = "Clinician-Rated Social Symptoms") + 
  #scale_color_manual(values=c("navy","slateblue1"), name="ASD Group",
  #labels=c("TD","ASD")) +
  #coord_cartesian(ylim=c(-2.5,2.5), xlim=c(-2.8,2.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method='lm', color="Black", se=T, size=.5) +
  theme_classic(base_size = 18) 


#### Step 5 - Compare social behavior ####

#### ~ Step 5.1 - Compare behavior during social controllability ~ ####

#### ~~ Overall Rejection Rate ~~ ####

aggregate(rejR_ic ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))
aggregate(rejR_nc ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))
aggregate(rejR_H_ic ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

# Put data into "long" format
db_full_matched_rejrate <- subset(db_full_matched, select=c(sub_id, ASD_group, sex, age, rejR_ic, rejR_nc, pair_id))
db_full_matched_RRLong<- pivot_longer(db_full_matched_rejrate, rejR_ic:rejR_nc, values_to = "Rejection Rate", names_to = "Condition")

#Mixed-Effects Model with random intercept for pair
fit <- lmer(`Rejection Rate` ~ ASD_group*Condition + age + sex + (1 | pair_id), db_full_matched_RRLong)
anova(fit)

# Not significant - drop the term
fit <- lmer(`Rejection Rate` ~ ASD_group + Condition + age + sex + (1 | pair_id), db_full_matched_RRLong)
anova(fit)

#Analysis of deviance table to review results
anova_fit<- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Plot overall rejection rate by group and condition
sum_RejRate <- summarySE(db_full_matched_RRLong, measurevar="Rejection Rate", groupvars=c("Condition","ASD_group"))

ggplot(data=sum_RejRate, aes(y=`Rejection Rate`, x=(factor(Condition)), group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group", labels= c("Low-trait", "High-trait", "ASD")) +
  labs(x="Condition", y="Rejection Rate") +
  geom_errorbar(aes(ymin=`Rejection Rate`-se, ymax=`Rejection Rate`+se),
                width=.2, position=position_dodge(.9)) +
  scale_x_discrete(labels=c("Controllable", "Uncontrollable")) +
  geom_point(data=db_full_matched_RRLong, aes(color=ASD_group), position=position_jitterdodge(dodge.width = .9, jitter.width = 0.2), alpha=0.6) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  guides(color=F, fill=F) +
  theme_classic(base_size = 20)  # white background

#### ~~ Rejection Rate by Offer Size ~~ ####
library(plotrix)

db_rr_ic <- subset(db_full_matched, select=c(sub_id, ASD_group, rejR_L_ic, rejR_M_ic, rejR_H_ic, age, sex, demo_ladder_rate, pair_id)) #get rejection rates for controllable condition
db_rr_ic <- db_rr_ic[!is.na(db_rr_ic$ASD_group),] #remove those not in top and bottom quartile
db_rr_ic$rejR_L_ic[db_rr_ic$rejR_L_ic == "NaN"] <- NA #recode missing values
db_rr_ic$rejR_M_ic[db_rr_ic$rejR_M_ic == "NaN"] <- NA #recode missing values
db_rr_ic$rejR_H_ic[db_rr_ic$rejR_H_ic == "NaN"] <- NA #recode missing values
db_rr_ic <- mutate(db_rr_ic, rejR_L_ic = as.numeric(rejR_L_ic)) #make numeric
db_rr_ic <- mutate(db_rr_ic, rejR_M_ic = as.numeric(rejR_M_ic)) #make numeric
db_rr_ic <- mutate(db_rr_ic, rejR_H_ic = as.numeric(rejR_H_ic)) #make numeric
db_trials_rr_ic_means <- aggregate(db_rr_ic[, 3:5], list(db_rr_ic$ASD_group), function(x) mean(x,na.rm=T)) #get means for offer size group, by miso group
db_trials_rr_ic_means <- dplyr::rename(db_trials_rr_ic_means, c("Group" = `Group.1`)) #rename group variable
db_trials_rr_ic_means_long <-pivot_longer(db_trials_rr_ic_means, cols = 2:4, values_to = "Rejection Rate", names_to = "Offer Size") #pivot long

db_trials_rr_ic_long <-pivot_longer(db_rr_ic, cols = 3:5, values_to = "Rejection Rate", names_to = "Offer Size") #pivot long

db_trials_rr_ic_se <- aggregate(db_rr_ic[, 3:5], list(db_rr_ic$ASD_group), std.error) #get means for offer size group, by miso group
db_trials_rr_ic_se <- dplyr::rename(db_trials_rr_ic_se, c("Group"=`Group.1`)) #rename group variable
db_trials_rr_ic_se_long <-pivot_longer(db_trials_rr_ic_means, cols = 2:4, values_to = "se", names_to = "Offer Size") #pivot long

rr_IC_mean <- cbind(db_trials_rr_ic_means_long, db_trials_rr_ic_se_long) #combine means and se
rr_IC_mean <- subset(rr_IC_mean, select = c(Group,`Offer Size`, `Rejection Rate`, FUN=se)) #delete repeat variables
rr_IC_mean <- mutate(rr_IC_mean, Condition="Controllable")


rr_IC_mean <- mutate(rr_IC_mean, `Rejection Rate` = `Rejection Rate`*100) #change to percentages
#rr_IC_mean$Group<-ifelse(rr_IC_mean$Group=="0", "LT", 
#                         ifelse(rr_IC_mean$Group=="1", "HT","ASD"))  #re-code group names
db_trials_rr_ic_long$`Offer Size`<-ifelse(db_trials_rr_ic_long$`Offer Size`=="rejR_L_nc", "1-3",    #re-code offer size names
                                          ifelse(db_trials_rr_ic_long$`Offer Size`=="rejR_M_nc", "4-6",
                                                 ifelse(db_trials_rr_ic_long$`Offer Size`=="rejR_H_nc", "7-9",
                                                        ifelse(db_trials_rr_ic_long$`Offer Size`=="rejR_L_ic", "1-3",
                                                               ifelse(db_trials_rr_ic_long$`Offer Size`=="rejR_M_ic", "4-6",
                                                                      ifelse(db_trials_rr_ic_long$`Offer Size`=="rejR_H_ic", "7-9", NA))))))

rr_IC_mean$`Offer Size`<-ifelse(rr_IC_mean$`Offer Size`=="rejR_L_nc", "1-3",    #re-code offer size names
                                ifelse(rr_IC_mean$`Offer Size`=="rejR_M_nc", "4-6",
                                       ifelse(rr_IC_mean$`Offer Size`=="rejR_H_nc", "7-9",
                                              ifelse(rr_IC_mean$`Offer Size`=="rejR_L_ic", "1-3",
                                                     ifelse(rr_IC_mean$`Offer Size`=="rejR_M_ic", "4-6",
                                                            ifelse(rr_IC_mean$`Offer Size`=="rejR_H_ic", "7-9", NA))))))


db_rr_nc <- subset(db_full_matched, select=c(sub_id, ASD_group, rejR_L_nc, rejR_M_nc, rejR_H_nc,  age, sex, demo_ladder_rate, pair_id)) #get rejection rates for controllable condition
db_rr_nc <- db_rr_nc[!is.na(db_rr_nc$ASD_group),] #remove those not in top and bottom quartile
db_rr_nc$rejR_L_nc[db_rr_nc$rejR_L_nc == "NaN"] <- NA #recode missing values
db_rr_nc$rejR_M_nc[db_rr_nc$rejR_M_nc == "NaN"] <- NA #recode missing values
db_rr_nc$rejR_H_nc[db_rr_nc$rejR_H_nc == "NaN"] <- NA #recode missing values
db_rr_nc <- mutate(db_rr_nc, rejR_L_nc = as.numeric(rejR_L_nc)) #make numeric
db_rr_nc <- mutate(db_rr_nc, rejR_M_nc = as.numeric(rejR_M_nc)) #make numeric
db_rr_nc <- mutate(db_rr_nc, rejR_H_nc = as.numeric(rejR_H_nc)) #make numeric
db_trials_rr_nc_means <- aggregate(db_rr_nc[, 3:5], list(db_rr_nc$ASD_group), function(x) mean(x,na.rm=T)) #get means for offer size group, by miso group
db_trials_rr_nc_means <- dplyr::rename(db_trials_rr_nc_means, c("Group" = `Group.1`)) #rename group variable
db_trials_rr_nc_means_long <-pivot_longer(db_trials_rr_nc_means, cols = 2:4, values_to = "Rejection Rate", names_to = "Offer Size") #pivot long

db_trials_rr_nc_long <-pivot_longer(db_rr_nc, cols = 3:5, values_to = "Rejection Rate", names_to = "Offer Size") #pivot long
db_trials_rr_nc_se <- aggregate(db_rr_nc[, 3:5], list(db_rr_nc$ASD_group), std.error) #get means for offer size group, by miso group
db_trials_rr_nc_se <- dplyr::rename(db_trials_rr_nc_se, c("Group" = `Group.1`)) #rename group variable
db_trials_rr_nc_se_long <-pivot_longer(db_trials_rr_nc_means, cols = 2:4, values_to = "se", names_to = "Offer Size") #pivot long

rr_NC_mean <- cbind(db_trials_rr_nc_means_long, db_trials_rr_nc_se_long) #combine means and se
rr_NC_mean <- subset(rr_NC_mean, select = c(Group,`Offer Size`, `Rejection Rate`, se)) #delete repeat variables
rr_NC_mean <- mutate(rr_NC_mean, Condition="Uncontrollable")


rr_NC_mean <- mutate(rr_NC_mean, `Rejection Rate` = `Rejection Rate`*100) #change to percentages
#rr_NC_mean$Group<-ifelse(rr_NC_mean$Group=="0", "LT", 
#                         ifelse(rr_NC_mean$Group=="1", "HT","ASD"))  #re-code group names

db_trials_rr_nc_long$`Offer Size`<-ifelse(db_trials_rr_nc_long$`Offer Size`=="rejR_L_nc", "1-3",    #re-code offer size names
                                          ifelse(db_trials_rr_nc_long$`Offer Size`=="rejR_M_nc", "4-6",
                                                 ifelse(db_trials_rr_nc_long$`Offer Size`=="rejR_H_nc", "7-9",
                                                        ifelse(db_trials_rr_nc_long$`Offer Size`=="rejR_L_ic", "1-3",
                                                               ifelse(db_trials_rr_nc_long$`Offer Size`=="rejR_M_ic", "4-6",
                                                                      ifelse(db_trials_rr_nc_long$`Offer Size`=="rejR_H_ic", "7-9", NA))))))

rr_NC_mean$`Offer Size`<-ifelse(rr_NC_mean$`Offer Size`=="rejR_L_nc", "1-3",    #re-code offer size names
                                ifelse(rr_NC_mean$`Offer Size`=="rejR_M_nc", "4-6",
                                       ifelse(rr_NC_mean$`Offer Size`=="rejR_H_nc", "7-9",
                                              ifelse(rr_NC_mean$`Offer Size`=="rejR_L_ic", "1-3",
                                                     ifelse(rr_NC_mean$`Offer Size`=="rejR_M_ic", "4-6",
                                                            ifelse(rr_NC_mean$`Offer Size`=="rejR_H_ic", "7-9", NA))))))


## Plot rejection rate for each offer size group by group and condition ##

ggplot(rr_IC_mean, aes(x=`Offer Size`, y=`Rejection Rate`, group = Group,
                       ymin=(`Rejection Rate`-se),
                       ymax=(`Rejection Rate`+se))) + 
  stat_summary(geom = "line", fun = "mean", size=0.9, aes(color=Group), na.rm = TRUE) +
  stat_summary(geom = "point", fun = "mean", size=3.2, alpha=0.7, aes(color=Group), na.rm = TRUE) +
  labs(x = "Offer Size ($)", y ="Rejection Rate (%)", title = "Controllable") + 
  scale_color_manual(values = c("#03A28A","#32A2E7", "#A975FC"),  
                     labels=c("ASD", "HT","Low-trait")) +
  geom_errorbar(width=.2, size=0.6, alpha=1, aes(color=Group)) +
  coord_cartesian(ylim=c(0,100)) +
  theme_classic(base_size = 20) 


ggplot(rr_NC_mean, aes(x=`Offer Size`, y=`Rejection Rate`, group = Group,
                       ymin=(`Rejection Rate`-se),
                       ymax=(`Rejection Rate`+se))) + 
  stat_summary(geom = "line", fun = "mean", size=0.9, aes(color=Group), na.rm = TRUE) +
  stat_summary(geom = "point", fun = "mean", size=3.2, alpha=0.7, aes(color=Group), na.rm = TRUE) +
  labs(x = "Offer Size ($)", y ="Rejection Rate (%)", title = "Uncontrollable") + 
  scale_color_manual(values = c("#03A28A","#32A2E7", "#A975FC"),
                     labels=c("ASD", "HT","Low-trait")) +
  geom_errorbar(width=.2, size=0.6, alpha=1, aes(color=Group)) +
  coord_cartesian(ylim=c(0,100)) +
  theme_classic(base_size = 20) 

## 3-Group difference in rejection rates by offer size
db_full_matched$rejR_H_ic <- as.numeric(db_full_matched$rejR_H_ic)
db_full_matched$rejR_M_ic <- as.numeric(db_full_matched$rejR_M_ic)
db_full_matched$rejR_L_ic <- as.numeric(db_full_matched$rejR_L_ic)

aggregate(rejR_H_ic ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))
aggregate(rejR_M_ic ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))
aggregate(rejR_L_ic ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit_L <- lmer(rejR_L_ic ~ ASD_group + sex + age+ (1 | pair_id), data=db_full_matched)

fit_M <- lmer(rejR_M_ic ~ ASD_group + sex + age+ (1 | pair_id), data=db_full_matched)

fit_H <- lmer(rejR_H_ic ~ ASD_group + sex + age+ (1 | pair_id), data=db_full_matched)


#Analysis of deviance table to review results
anova_fit_L <- anova(fit_L)
anova_fit_L

anova_fit_M <- anova(fit_M)
anova_fit_M

anova_fit_H <- anova(fit_H)
anova_fit_H

# Adjust p-values
rejR_H_ic = 0.00294
rejR_M_ic = 0.1907
rejR_L_ic = 0.81613

# correct p-values for multiple comparisons (FDR)
pvalues = c(rejR_H_ic, rejR_M_ic, rejR_L_ic)
p.adjust(pvalues,method="BH")

# Get effect size
eta_squared_L <- effectsize::eta_squared(anova_fit_L, partial = TRUE)
eta_squared_M <- effectsize::eta_squared(anova_fit_M, partial = TRUE)
eta_squared_H <- effectsize::eta_squared(anova_fit_H, partial = TRUE)

print(eta_squared_L)
print(eta_squared_M)
print(eta_squared_H)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit_H, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit_H), edf= df.residual(fit_H)))

#Mixed-Effects Model with random intercept for pair
fit_L <- lmer(rejR_L_nc ~ ASD_group + sex + age+ (1 | pair_id), data=db_full_matched)

fit_M <- lmer(rejR_M_nc ~ ASD_group + sex + age+ (1 | pair_id), data=db_full_matched)

fit_H <- lmer(rejR_H_nc ~ ASD_group + sex + age+ (1 | pair_id), data=db_full_matched)


#Analysis of deviance table to review results
anova_fit_L <- anova(fit_L)
anova_fit_L

anova_fit_M <- anova(fit_M)
anova_fit_M

anova_fit_H <- anova(fit_H)
anova_fit_H

# Adjust p-values
rejR_H_nc = 0.02487
rejR_M_nc = 0.7130
rejR_L_nc = 0.3088

pvalues = c(rejR_H_nc, rejR_M_nc, rejR_L_nc)
p.adjust(pvalues,method="BH")

# Get effect size
eta_squared_L <- effectsize::eta_squared(anova_fit_L, partial = TRUE)
eta_squared_M <- effectsize::eta_squared(anova_fit_M, partial = TRUE)
eta_squared_H <- effectsize::eta_squared(anova_fit_H, partial = TRUE)

print(eta_squared_L)
print(eta_squared_M)
print(eta_squared_H)

#### ~~ Perceived Control ~~ ####
aggregate(pc_ic ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))
aggregate(pc_nc ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

# Put data into "long" format
db_full_matched_pc <- subset(db_full_matched, select=c(sub_id, ASD_group, sex, age, pc_ic, pc_nc, pair_id))
db_full_matched_LongControl <- pivot_longer(db_full_matched_pc, pc_ic:pc_nc, values_to = "PC", names_to = "Condition")

db_full_matched_LongControl$PC <-as.numeric(db_full_matched_LongControl$PC)

#Mixed-Effects Model with random intercept for pair
fit <- lmer(PC ~ ASD_group*Condition + age + sex+ (1 | pair_id), db_full_matched_LongControl)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

#Look at each condition separately for posthoc pairwise differences
## Controllable
fit <- lmer(pc_ic ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))

## Uncontrollable
fit <- lmer(pc_nc ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))


# Plot perceived control by group and condition
db_full_matched_LongControl_pcfull <- subset(db_full_matched_LongControl, db_full_matched_LongControl$PC != "NaN")

sum_pc <- summarySE(db_full_matched_LongControl_pcfull, measurevar="PC", groupvars=c("Condition","ASD_group"))

ggplot(data=sum_pc, aes(y=PC, x=(factor(Condition)), group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group", labels= c("Low-trait", "High-trait", "ASD")) +
  labs(x="Condition", y="Perceived Control") +
  geom_errorbar(aes(ymin=PC-se, ymax=PC+se),
                width=.2, position=position_dodge(.9)) +
  scale_x_discrete(labels=c("Controllable", "Uncontrollable")) +
  geom_point(data=db_full_matched_LongControl, aes(color=ASD_group), position=position_jitterdodge(dodge.width = .9, jitter.width = 0.2), alpha=0.6) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(color=F, fill=F) +
  theme_classic(base_size = 20)  # white background


#### ~ Step 5.2 - Compare behavior during social navigation ~ ####

#### ~~ Character Liking (self-report) ~~ ####
aggregate(liking_mean ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit <- lmer(liking_mean ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))

# Plot character liking
sum1 <- summarySE(db_full_matched, measurevar="liking_mean", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum1, aes(y=liking_mean, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Character Liking") +
  geom_errorbar(aes(ymin=liking_mean-se, ymax=liking_mean+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("ASD", "HT","Low-trait")) +
  geom_jitter(data=db_full_matched, aes(y=liking_mean, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color=F) +
  theme_classic(base_size = 20)  # white background


#### ~~ Affiliation Tendency ~~ ####
aggregate(affil_mean_mean ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit <- lmer(affil_mean_mean ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Perform pairwise comparisons using emmeans
em <- emmeans(fit, ~ ASD_group)
pairwise_comparisons <- contrast(em, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons, infer = c(TRUE, TRUE))

#effect size
(eff1<-eff_size(em, sigma=sigma(fit), edf= df.residual(fit)))

# Plot affilliation
sum1 <- summarySE(db_full_matched, measurevar="affil_mean_mean", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum1, aes(y=affil_mean_mean, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Affiliation Tendency") +
  geom_errorbar(aes(ymin=affil_mean_mean-se, ymax=affil_mean_mean+se),
                width=.2, position=position_dodge(.85)) +
  #scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  scale_x_discrete(labels=c("ASD", "HT","LT")) +
  geom_jitter(data=db_full_matched, aes(y=affil_mean_mean, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color=F) +
  theme_classic(base_size = 20)  # white background


#### ~~ Power Tendency ~~ ####

aggregate(power_mean_mean ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit <- lmer(power_mean_mean ~ ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Plot power
sum1 <- summarySE(db_full_matched, measurevar="power_mean_mean", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum1, aes(y=power_mean_mean, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Power Tendency") +
  geom_errorbar(aes(ymin=power_mean_mean-se, ymax=power_mean_mean+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("ASD", "HT","LT")) +
  geom_jitter(data=db_full_matched, aes(y=power_mean_mean, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color=F) +
  theme_classic(base_size = 20)  # white background

#### ~ Step 5.3 - Test for group x symptom interactions on behavior ~ ####

#### ~~ Character liking ~~ ####

#Mixed-Effects Model with random intercept for pair
fit <- lmer(liking_mean ~ bapq_score*ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

#Plot relationship
ggplot(db_full_matched, aes(y=liking_mean, x=bapq_score, color=ASD_group)) + 
  geom_point(size=2) + 
  labs(x = "Self-Reported ASD Traits", y = "Character Liking") + 
  scale_color_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group",
                     labels=c("ASD","HT", "LT")) +
  
  guides(color = FALSE) +
  geom_smooth(method='lm', se=T, size=.5) +
  theme_classic(base_size = 20) +
  theme(axis.title.x=element_text(size=18))

#### ~~ Affiliation tendency ~~ ####
aggregate(liking_mean ~ ASD_group, db_full_matched, function(x) c(mean = mean(x), sd = sd(x)))

#Mixed-Effects Model with random intercept for pair
fit <- lmer(affil_mean_mean ~ bapq_score*ASD_group + age + sex + (1 | pair_id), data=db_full_matched)

#Analysis of deviance table to review results
anova_fit <- anova(fit)
anova_fit

# Get effect size
eta_squared <- effectsize::eta_squared(anova_fit, partial = TRUE)
print(eta_squared)

# Follow up correlations to parse direction of effect
cor.test(db_full_ASD$affil_mean_mean, db_full_ASD$bapq_score)
cor.test(db_full_HT$affil_mean_mean, db_full_HT$bapq_score)
cor.test(db_full_LT$affil_mean_mean, db_full_LT$bapq_score)

# Adjust p-values
p_bapq_affil_ASD = 0.003736
p_bapq_affil_HT = 0.2684
p_bapq_affil_LT = 0.1907

pvalues_symptoms = c(p_bapq_affil_ASD, p_bapq_affil_HT, p_bapq_affil_LT)
p.adjust(pvalues_symptoms,method="BH")

# Plot symptom x group interaction on affilation
ggplot(db_full_matched, aes(y=affil_mean_mean, x=bapq_score, color=ASD_group)) + 
  geom_point(size=2) + 
  labs(x = "Self-Reported ASD Traits", y = "Affiliation Tendency") + 
  scale_color_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group",
                     labels=c("ASD","HT", "LT")) +
  coord_cartesian(ylim=c(-0.50,0.75)) +
  guides(color=F) +
  geom_smooth(method='lm', se=T, size=.5) +
  theme_classic(base_size = 20) +
  theme(axis.title.x=element_text(size=18))

