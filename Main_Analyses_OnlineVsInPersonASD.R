
### Data Analysis for "pheno. divergence btwn self-reported and clinically ascertained autism" ####
## AKA "online vs. in-person autism research" ##

#### Step 1 - Load libraries ####
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(multcomp)

#### Step 2 - Load functions and data ####
source("SummarySE.R") #To help with bar plotting
db_full_sn_matched <- read_excel("ASD_MatchedDataset.xlsx") #data

#Split dataframe by group
db_full_sn_matched_groups <- split(db_full_sn_matched, db_full_sn_matched$ASD_group)
db_full_sn_ASD <- db_full_sn_matched_groups$'2'
db_full_sn_HT <- db_full_sn_matched_groups$'1'
db_full_sn_LT <- db_full_sn_matched_groups$'0'

#### Step 3 - Compare demographics ####

# Age
aggregate(age ~ ASD_group, db_full_sn_matched, function(x) c(mean = mean(x), sd = sd(x)))
fit <- lm(age ~ ASD_group, data=db_full_sn_matched)
anova(fit) # 3-Group difference?

# Sex
aggregate(sex ~ ASD_group, db_full_sn_matched, FUN = table)
kruskal.test(sex ~ ASD_group, data = db_full_sn_matched)

# Race (White yes/no)
aggregate(demo_race_white ~ ASD_group, db_full_sn_matched, FUN =table )
kruskal.test(demo_race_white ~ ASD_group, data = db_full_sn_matched) # 3-Group difference?

# Ethnicity 
aggregate(demo_race_latino_hispanic ~ ASD_group, db_full_sn_matched, FUN =table )
kruskal.test(demo_race_latino_hispanic ~ ASD_group, data = db_full_sn_matched) # 3-Group difference?

#### Step 4 - Compare symptoms ####

#### ~ Step 4.1 - Symptom agreement within ASD ~ ####
# First, normalize each measure and sub-scale
db_full_sn_ASD$ados_normed <- (db_full_sn_ASD$ados4natot - mean(db_full_sn_ASD$ados4natot, na.rm=TRUE)) / sd(db_full_sn_ASD$ados4natot, na.rm=TRUE)
db_full_sn_ASD$bapq_normed <- (db_full_sn_ASD$bapq_score - mean(db_full_sn_ASD$bapq_score, na.rm=TRUE)) / sd(db_full_sn_ASD$bapq_score, na.rm=TRUE)

# Get sub-scales 
# For BAPQ, combine pragmatic language and aloof subscores into one "Social" score
db_full_sn_ASD$bapq_SA <- (db_full_sn_ASD$bapq_prag_lang_score + db_full_sn_ASD$bapq_aloof_score)/2

# Norm the sub-scales
# Repetitive and Restrictive Behaviors and 
db_full_sn_ASD$ados_rrb_normed <- (db_full_sn_ASD$ados4narrb - mean(db_full_sn_ASD$ados4narrb, na.rm=TRUE)) / sd(db_full_sn_ASD$ados4narrb, na.rm=TRUE)
db_full_sn_ASD$bapq_rrb_normed <- (db_full_sn_ASD$bapq_rigid_score - mean(db_full_sn_ASD$bapq_rigid_score, na.rm=TRUE)) / sd(db_full_sn_ASD$bapq_rigid_score, na.rm=TRUE)

#Social
db_full_sn_ASD$ados_soc_normed <- (db_full_sn_ASD$ados4nas - mean(db_full_sn_ASD$ados4nas, na.rm=TRUE)) / sd(db_full_sn_ASD$ados4nas, na.rm=TRUE)
db_full_sn_ASD$bapq_soc_normed <- (db_full_sn_ASD$bapq_SA - mean(db_full_sn_ASD$bapq_SA, na.rm=TRUE)) / sd(db_full_sn_ASD$bapq_SA, na.rm=TRUE)

# Second, test for agreement
# Overall agreement (total scores)
fit <- lm(bapq_normed ~ ados_normed + age + sex, data=db_full_sn_ASD)
anova(fit)

ggplot(db_full_sn_ASD, aes(y=ados_normed, x=bapq_normed)) + 
  geom_point(size=3,  color="#A975FC") + 
  labs(x = "Self-Reported ASD Symptoms", y = "Clinician-Rated ASD Symptoms") + 
  coord_cartesian(ylim=c(-2.5,2.5), xlim=c(-2.8,2.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method='lm', color="Black", se=T, size=.5) +
  theme_classic(base_size = 18) 

# Sub-scale agreement 
fit <- lm(bapq_rrb_normed ~ ados_rrb_normed + age_years_mri + sex, data=db_full_sn_ASD)
anova(fit)

ggplot(db_full_sn_ASD, aes(y=ados_rrb_normed, x=bapq_rrb_normed)) + 
  geom_point(size=3,  color="#A975FC") + 
  labs(x = "Self-Reported RRB Symptoms", y = "Clinician-Rated RRB Symptoms") + 
  #scale_color_manual(values=c("navy","slateblue1"), name="ASD Group",
  #labels=c("TD","ASD")) +
  coord_cartesian(ylim=c(-2.5,2.5), xlim=c(-2.8,2.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method='lm', color="Black", se=T, size=.5) +
  theme_classic(base_size = 18) 


fit <- lm(bapq_soc_normed ~ ados_soc_normed + age + sex , data=db_full_sn_ASD)
anova(fit)

ggplot(db_full_sn_ASD, aes(y=ados_soc_normed, x=bapq_soc_normed)) + 
  geom_point(size=3,  color="#A975FC") + 
  labs(x = "Self-Reported Social Symptoms", y = "Clinician-Rated Social Symptoms") + 
  #scale_color_manual(values=c("navy","slateblue1"), name="ASD Group",
  #labels=c("TD","ASD")) +
  coord_cartesian(ylim=c(-2.5,2.5), xlim=c(-2.8,2.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  geom_smooth(method='lm', color="Black", se=T, size=.5) +
  theme_classic(base_size = 18) 

#### ~ Step 4.2 - Group difference in symptoms?  ~####

#### ~~ ASD symptoms (BAPQ) ~~ ####
#Overall 3-group ANOVA
fit <- aov(bapq_score ~ ASD_group + age + sex, data=db_full_sn_matched)
anova(fit)

# Follow up t-tests to parse direction of effect
t.test(db_full_sn_HT$bapq_score, db_full_sn_ASD$bapq_score) 
t.test(db_full_sn_ASD$bapq_score, db_full_sn_LT$bapq_score) 
t.test(db_full_sn_HT$bapq_score, db_full_sn_LT$bapq_score) 

# Adjust p-values for post-hoc
a <- aov(bapq_score ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# Plot ASD Symtpom Scores
sum_BAPQ_Symptoms <- summarySE(db_full_sn_matched, measurevar="bapq_score", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum_BAPQ_Symptoms, aes(y=bapq_score, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Self-Reported ASD Symptoms") +
  geom_errorbar(aes(ymin=bapq_score-se, ymax=bapq_score+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  geom_jitter(data=db_full_sn_matched, aes(y=bapq_score, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  coord_cartesian(ylim=c(0,6)) +
  guides(fill=FALSE, color =F) +
  theme_classic(base_size = 18)  # white background

#### ~~ Social Anxiety symptoms (LSAS) ~~ ####
#Overall 3-group ANOVA
fit <- aov(lsas_av_score ~ ASD_group + age + sex , data=db_full_sn_matched)
anova(fit)

# Follow up t-tests to parse direction of effect
t.test(db_full_sn_HT$lsas_av_score, db_full_sn_ASD$lsas_av_score) 
t.test(db_full_sn_ASD$lsas_av_score, db_full_sn_LT$lsas_av_score) 
t.test(db_full_sn_HT$lsas_av_score, db_full_sn_LT$lsas_av_score) 

# Adjust p-values for post-hoc
a <- aov(lsas_av_score ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# Plot Social Anxiety Symptom Scores
sum_social_anxiety <- summarySE(db_full_sn_matched, measurevar="lsas_av_score", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum_social_anxiety, aes(y=lsas_av_score, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  #scale_fill_manual(values=c("#008F7A","#0081CF", "#845EC2"), name="Group") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Self-Reported SA Symptoms") +
  geom_errorbar(aes(ymin=lsas_av_score-se, ymax=lsas_av_score+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  geom_jitter(data=db_full_sn_matched, aes(y=lsas_av_score, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color =F) +
  theme_classic(base_size = 18)  # white background

#### ~~ Avoidant Personality Disorder (AVPD; APDIS) symptoms (LSAS) ~~ ####
#Overall 3-group ANOVA
fit <- aov(apdis_score ~ ASD_group + age + sex , data=db_full_sn_matched)
anova(fit)

# Follow up t-tests to parse direction of effect
t.test(db_full_sn_HT$apdis_score, db_full_sn_ASD$apdis_score) 
t.test(db_full_sn_ASD$apdis_score, db_full_sn_LT$apdis_score) 
t.test(db_full_sn_HT$apdis_score, db_full_sn_LT$apdis_score) 

# Adjust p-values for post-hoc
a <- aov(apdis_score ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# Plot Avoidant Symptom Scores
sum_avpd <- summarySE(db_full_sn_matched, measurevar="apdis_score", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum_avpd, aes(y=apdis_score, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  #scale_fill_manual(values=c("#008F7A","#0081CF", "#845EC2"), name="Group") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Self-Reported AVPD Symptoms") +
  geom_errorbar(aes(ymin=apdis_score-se, ymax=apdis_score+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  geom_jitter(data=db_full_sn_matched, aes(y=apdis_score, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color =F) +
  theme_classic(base_size = 18)  # white background

#### Step 5 - Compare social behavior ####

#### ~ Step 5.1 - Compare behavior during social controllability ~ ####

#### ~~ Overall Rejection Rate ~~ ####
# Put data into "long" format
db_full_sn_matched_rejrate <- subset(db_full_sn_matched, select=c(sub_id, ASD_group, sex, age, rejR_ic, rejR_nc))
db_full_sn_matched_RRLong<- pivot_longer(db_full_sn_matched_rejrate, rejR_ic:rejR_nc, values_to = "Rejection Rate", names_to = "Condition")

# 3-group difference?
fit <- lm(`Rejection Rate` ~ ASD_group*Condition + age + sex, db_full_sn_matched_RRLong)
anova(fit)

# Plot overall rejection rate by group and condition
sum_RejRate <- summarySE(db_full_sn_matched_RRLong, measurevar="Rejection Rate", groupvars=c("Condition","ASD_group"))

ggplot(data=sum_RejRate, aes(y=`Rejection Rate`, x=(factor(Condition)), group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group", labels= c("Low-trait", "High-trait", "ASD")) +
  labs(x="Condition", y="Rejection Rate") +
  geom_errorbar(aes(ymin=`Rejection Rate`-se, ymax=`Rejection Rate`+se),
                width=.2, position=position_dodge(.9)) +
  scale_x_discrete(labels=c("Controllable", "Uncontrollable")) +
  geom_point(data=db_full_sn_matched_RRLong, aes(color=ASD_group), position=position_jitterdodge(dodge.width = .9, jitter.width = 0.2), alpha=0.6) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  guides(color=F, fill=F) +
  theme_classic(base_size = 20)  # white background

#### ~~ Rejection Rate by Offer Size ~~ ####
library(plotrix)

db_rr_ic <- subset(db_full_sn_matched, select=c(sub_id, ASD_group, rejR_L_ic, rejR_M_ic, rejR_H_ic, age, sex)) #get rejection rates for controllable condition
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


db_rr_nc <- subset(db_full_sn_matched, select=c(sub_id, ASD_group, rejR_L_nc, rejR_M_nc, rejR_H_nc,  age, sex)) #get rejection rates for controllable condition
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
                     labels=c("LT", "HT", "ASD")) +
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
                     labels=c("LT", "HT", "ASD")) +
  geom_errorbar(width=.2, size=0.6, alpha=1, aes(color=Group)) +
  coord_cartesian(ylim=c(0,100)) +
  theme_classic(base_size = 20) 

## 3-Group difference in rejection rates by offer size

## Uncontrollable Condition (NC, "No Control")
fit <- lm(`Rejection Rate` ~ ASD_group*`Offer Size` + sex + age, data=db_trials_rr_nc_long)
anova(fit)

## Controllable Condition (IC, "In Control")
fit <- lm(`Rejection Rate` ~ ASD_group*`Offer Size` + sex + age, data=db_trials_rr_ic_long)
anova(fit)


# Which offer size drives the effect?
db_full_sn_matched$rejR_H_ic <- as.numeric(db_full_sn_matched$rejR_H_ic)
db_full_sn_matched$rejR_M_ic <- as.numeric(db_full_sn_matched$rejR_M_ic)
db_full_sn_matched$rejR_L_ic <- as.numeric(db_full_sn_matched$rejR_L_ic)

fit <- lm(rejR_L_ic ~ ASD_group + sex + age, data=db_full_sn_matched)
anova(fit)
fit <- lm(rejR_M_ic ~ ASD_group + sex + age, data=db_full_sn_matched)
anova(fit)
fit <- lm(rejR_H_ic ~ ASD_group + sex + age, data=db_full_sn_matched)
anova(fit)

#### ~~ Perceived Control ~~ ####

# Put data into "long" format
db_full_sn_matched_pc<- subset(db_full_sn_matched, select=c(sub_id, ASD_group, sex, age, pc_ic, pc_nc))
db_full_sn_matched_LongControl<- pivot_longer(db_full_sn_matched_pc, pc_ic:pc_nc, values_to = "PC", names_to = "Condition")

db_full_sn_matched_LongControl$PC <-as.numeric(db_full_sn_matched_LongControl$PC)

# 3-group difference?
fit <- lm(PC ~ ASD_group*Condition + age + sex, db_full_sn_matched_LongControl)
anova(fit)

# change variable to numeric in all dataframes for stats
db_full_sn_matched$pc_ic <-as.numeric(db_full_sn_matched$pc_ic)
db_full_sn_matched$pc_nc <-as.numeric(db_full_sn_matched$pc_nc)
db_full_sn_ASD$pc_ic <-as.numeric(db_full_sn_ASD$pc_ic)
db_full_sn_ASD$pc_nc <-as.numeric(db_full_sn_ASD$pc_nc)
db_full_sn_HT$pc_ic <-as.numeric(db_full_sn_HT$pc_ic)
db_full_sn_HT$pc_nc <-as.numeric(db_full_sn_HT$pc_nc)
db_full_sn_LT$pc_ic <-as.numeric(db_full_sn_ASD$pc_ic)
db_full_sn_LT$pc_nc <-as.numeric(db_full_sn_LT$pc_nc)

# follow up t-tests to parse direction of effect - by condition
t.test(db_full_sn_ASD$pc_ic, db_full_sn_HT$pc_ic)
t.test(db_full_sn_HT$pc_ic, db_full_sn_LT$pc_ic)
t.test(db_full_sn_LT$pc_ic, db_full_sn_ASD$pc_ic)

# Adjust p-values
a <- aov(pc_ic ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# follow up t-tests to parse direction of effect - by condition
t.test(db_full_sn_ASD$pc_nc, db_full_sn_HT$pc_nc)
t.test(db_full_sn_HT$pc_nc, db_full_sn_LT$pc_nc)
t.test(db_full_sn_LT$pc_nc, db_full_sn_ASD$pc_nc)

# Adjust p-values
a <- aov(pc_nc ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# follow up t-tests to parse direction of effect - by group

t.test(db_full_sn_ASD$pc_ic, db_full_sn_ASD$pc_nc, paired = T)
t.test(db_full_sn_HT$pc_ic, db_full_sn_HT$pc_nc, paired = T)
t.test(db_full_sn_LT$pc_ic, db_full_sn_LT$pc_nc, paired = T)

# Adjust p-values 
pvalues = c(0.3821, 9.211e-10, 0.0003534)
p.adjust(pvalues,method="BH")

# Plot perceived control by group and condition
db_full_sn_matched_LongControl_pcfull <- subset(db_full_sn_matched_LongControl, db_full_sn_matched_LongControl$PC != "NaN")

sum_pc <- summarySE(db_full_sn_matched_LongControl_pcfull, measurevar="PC", groupvars=c("Condition","ASD_group"))

ggplot(data=sum_pc, aes(y=PC, x=(factor(Condition)), group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group", labels= c("Low-trait", "High-trait", "ASD")) +
  labs(x="Condition", y="Perceived Control") +
  geom_errorbar(aes(ymin=PC-se, ymax=PC+se),
                width=.2, position=position_dodge(.9)) +
  scale_x_discrete(labels=c("Controllable", "Uncontrollable")) +
  geom_point(data=db_full_sn_matched_LongControl, aes(color=ASD_group), position=position_jitterdodge(dodge.width = .9, jitter.width = 0.2), alpha=0.6) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(color=F, fill=F) +
  theme_classic(base_size = 20)  # white background

#### ~ Step 5.2 - Compare behavior during social navigation ~ ####

#### ~~ Character Liking (self-report) ~~ ####
#Overall 3-group ANOVA
fit <- lm(liking_mean ~ ASD_group + age + sex, data=db_full_sn_matched)
anova(fit)

# Follow up t-tests to parse direction of effect
t.test(db_full_sn_ASD$liking_mean, db_full_sn_HT$liking_mean)
t.test(db_full_sn_HT$liking_mean, db_full_sn_LT$liking_mean)
t.test(db_full_sn_LT$liking_mean, db_full_sn_ASD$liking_mean)

# Adjust p-values for post-hoc
a <- aov(liking_mean ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# Plot character liking
sum1 <- summarySE(db_full_sn_matched, measurevar="liking_mean", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum1, aes(y=liking_mean, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Character Liking") +
  geom_errorbar(aes(ymin=liking_mean-se, ymax=liking_mean+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  geom_jitter(data=db_full_sn_matched, aes(y=liking_mean, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color=F) +
  theme_classic(base_size = 20)  # white background


#### ~~ Affiliation Tendency ~~ ####
#Overall 3-group ANOVA
fit <- lm(affil_mean_mean ~ ASD_group + age + sex , data=db_full_sn_matched)
anova(fit)

# Follow up t-tests to parse direction of effect
t.test(db_full_sn_ASD$affil_mean_mean, db_full_sn_HT$affil_mean_mean)
t.test(db_full_sn_HT$affil_mean_mean, db_full_sn_LT$affil_mean_mean)
t.test(db_full_sn_LT$affil_mean_mean, db_full_sn_ASD$affil_mean_mean)

# Adjust p-values for post-hoc
a <- aov(affil_mean_mean ~ ASD_group, data=db_full_sn_matched)
TukeyHSD(a)

# Plot affilliation
sum1 <- summarySE(db_full_sn_matched, measurevar="affil_mean_mean", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum1, aes(y=affil_mean_mean, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Affiliation Tendency") +
  geom_errorbar(aes(ymin=affil_mean_mean-se, ymax=affil_mean_mean+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  geom_jitter(data=db_full_sn_matched, aes(y=affil_mean_mean, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color=F) +
  theme_classic(base_size = 20)  # white background


#### ~~ Power Tendency ~~ ####
#Overall 3-group ANOVA
fit <- lm(power_mean_mean ~ ASD_group + age + sex +demo_ladder_rate, data=db_full_sn_matched)
anova(fit)

# Plot power
sum1 <- summarySE(db_full_sn_matched, measurevar="power_mean_mean", groupvars=c("ASD_group"), na.rm=T)

ggplot(data=sum1, aes(y=power_mean_mean, x=ASD_group, group=ASD_group)) +
  geom_bar(aes(fill=ASD_group), stat = "summary", position = "dodge", fun = "mean") +
  scale_fill_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group") +
  labs(x="Group", y="Power Tendency") +
  geom_errorbar(aes(ymin=power_mean_mean-se, ymax=power_mean_mean+se),
                width=.2, position=position_dodge(.85)) +
  scale_x_discrete(labels=c("Low-trait", "High-trait", "ASD")) +
  geom_jitter(data=db_full_sn_matched, aes(y=power_mean_mean, x=ASD_group, color=ASD_group),
              alpha =0.6, width=0.15) +
  scale_color_manual(values=c("#587F65","#1A53B6", "#512C8E"), name="Group") +
  #coord_cartesian(ylim=c(0.6,0.85)) +
  guides(fill=FALSE, color=F) +
  theme_classic(base_size = 20)  # white background

#### ~ Step 5.3 - Test for group x symptom interactions on behavior ~ ####

#### ~~ Character liking ~~ ####

# Overall 3-group ANOVA
fit <- lm(liking_mean ~ bapq_score*ASD_group + age + sex, data=db_full_sn_matched)
anova(fit)

# No interaction - drop the interaction term to look for main effect
fit <- lm(liking_mean ~ bapq_score + age + sex, data=db_full_sn_matched)
anova(fit)

ggplot(db_full_sn_matched, aes(y=liking_mean, x=bapq_score)) + 
  geom_point(size=2, aes(color=ASD_group)) + 
  labs(x = "Self-Reported ASD Symptoms", y = "Character Liking") + 
  scale_color_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group",
                     labels=c("LT","HT", "ASD")) +
  guides(color = FALSE) +
  geom_smooth(method='lm', se=T, size=.5, color = "navy") +
  theme_classic(base_size = 20) +
  theme(axis.title.x=element_text(size=18))

#### ~~ Affiliation tendency ~~ ####

# Overall 3-group ANOVA
fit <- lm(affil_mean_mean ~ bapq_score*ASD_group + age + sex, data=db_full_sn_matched)
anova(fit)

# Follow up correlations to parse direction of effect
cor.test(db_full_sn_ASD$affil_mean_mean, db_full_sn_ASD$bapq_score)
cor.test(db_full_sn_HT$affil_mean_mean, db_full_sn_HT$bapq_score)
cor.test(db_full_sn_LT$affil_mean_mean, db_full_sn_LT$bapq_score)

# Adjust p-values
p_bapq_affil_ASD = 0.003736
p_bapq_affil_HT = 0.4994
p_bapq_affil_LT = 0.2249

pvalues_symptoms = c(p_bapq_affil_ASD, p_bapq_affil_HT, p_bapq_affil_LT)
p.adjust(pvalues_symptoms,method="BH")

# Plot symptom x group interaction on affilation
ggplot(db_full_sn_matched, aes(y=affil_mean_mean, x=bapq_score, color=ASD_group)) + 
  geom_point(size=2) + 
  labs(x = "Self-Reported ASD Symptoms", y = "Affiliation Tendency") + 
  scale_color_manual(values=c("#03A28A","#32A2E7", "#A975FC"), name="Group",
                     labels=c("LT","HT", "ASD")) +
  coord_cartesian(ylim=c(-0.50,0.75)) +
  guides(color=F) +
  geom_smooth(method='lm', se=T, size=.5) +
  theme_classic(base_size = 20) 

#### ~~ Rejection rate - high offers during controllable ~~ ####

# Overall 3-group ANOVA
fit <- lm(rejR_H_ic ~ bapq_score*ASD_group + age + sex, data=db_full_sn_matched)
anova(fit)

# No interaction - drop the interaction term to look for main effect
fit <- lm(rejR_H_ic ~ bapq_score + age + sex, data=db_full_sn_matched)
anova(fit)

# No main effect

#### ~~ Perceived control ~~ ####

# calculate the discrepancy in perceived control between the two conditions
db_full_sn_matched <- mutate(db_full_sn_matched, pc_discrepency = pc_ic - pc_nc)

#Re-split dataframe by group with new variable
db_full_sn_matched_groups <- split(db_full_sn_matched, db_full_sn_matched$ASD_group)
db_full_sn_ASD <- db_full_sn_matched_groups$'2'
db_full_sn_HT <- db_full_sn_matched_groups$'1'
db_full_sn_LT <- db_full_sn_matched_groups$'0'

# Overall 3-group ANOVA
fit <- lm(pc_discrepency ~ bapq_score*ASD_group + age + sex, data=db_full_sn_matched)
anova(fit)

# No interaction - drop the interaction term to look for main effect
fit <- lm(pc_discrepency ~ bapq_score + age + sex, data=db_full_sn_matched)
anova(fit)


