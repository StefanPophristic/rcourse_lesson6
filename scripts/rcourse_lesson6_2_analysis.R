############
# Stefan Pophristic
# STATS ANOVA Course — Lesson 6 — Pt. 2
# Jan. 9, 2023
# Script for Analysis
############

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## READ IN DATA ####
source("rcourse_lesson6_cleaning.R")

## LOAD PACKAGES ####
library(lme4)

## ORGANIZE DATA ####
# Accuracy data
data_accuracy_stats = data_accuracy_clean %>%
  mutate(congruency_contrast = ifelse(congruency == "con", -0.5, 0.5)) %>%
  mutate(half_contrast = ifelse(half == "first", -0.5, 0.5))

xtabs(~congruency+congruency_contrast, data_accuracy_stats)
xtabs(~half+half_contrast, data_accuracy_stats)

# RT data
data_rt_stats = data_rt_clean %>%
  mutate(congruency_contrast = ifelse(congruency == "con", -0.5, 0.5)) %>%
  mutate(half_contrast = ifelse(half == "first", -0.5, 0.5))

xtabs(~congruency+congruency_contrast, data_rt_stats)
xtabs(~half+half_contrast, data_rt_stats)

## BUILD MODELS FOR ACCURACY ANALYSIS ####

# Full model
accuracy.glmer = glmer(accuracy ~ congruency_contrast * half_contrast +
                         (1+congruency_contrast*half_contrast|subject_id) +
                         (1+half_contrast|item), family = "binomial",
                       data = data_accuracy_stats)
# model failed to converge

accuracy.glmer = glmer(accuracy ~ congruency_contrast * half_contrast +
                         (1|subject_id) +
                         (0+half_contrast|subject_id) +
                         (1|item), family = "binomial",
                       data = data_accuracy_stats)

# Summarise model and save
accuracy.glmer_sum = summary(accuracy.glmer)
accuracy.glmer_sum

# Test for effect of congruency
accuracy_congruency.glmer = glmer(accuracy ~ congruency_contrast * half_contrast -
                                    congruency_contrast +
                                    (1|subject_id) +
                                    (0+half_contrast|subject_id) +
                                    (1|item), family = "binomial",
                                  data = data_accuracy_stats)

# Model Comparison
accuracy_congruency.anova = anova(accuracy.glmer, accuracy_congruency.glmer)
accuracy_congruency.anova

# Test for effect of experiment half
accuracy_half.glmer = glmer(accuracy ~ congruency_contrast * half_contrast -
                              half_contrast +
                              (1|subject_id) +
                              (0+half_contrast|subject_id) +
                              (1|item), family = "binomial",
                            data = data_accuracy_stats)

accuracy_half.anova = anova(accuracy.glmer, accuracy_half.glmer)
accuracy_half.anova

# Test for interaction of congruency x experiment half
accuracy_congruencyxhalf.glmer = glmer(accuracy ~ congruency_contrast * half_contrast -
                                         congruency_contrast:half_contrast +
                                         (1|subject_id) +
                                         (0+half_contrast|subject_id) +
                                         (1|item), family = "binomial",
                                       data = data_accuracy_stats)

accuracy_congruencyxhalf.anova = anova(accuracy.glmer, accuracy_congruencyxhalf.glmer)
accuracy_congruencyxhalf.anova


## BUILD MODELS FOR REACTION TIME ANALYSIS ####
# Full model
rt_log10.lmer = lmer(rt_log10 ~ congruency_contrast * half_contrast +
                       (1+congruency_contrast*half_contrast|subject_id) +
                       (1+half_contrast|item), REML = F,
                     data = data_rt_stats)
#The lesson says this model does not converge but it did for me

rt_log10.lmer = lmer(rt_log10 ~ congruency_contrast * half_contrast +
                       (1+congruency_contrast*half_contrast|subject_id) +
                       (1|item) +
                       (0+half_contrast|item), REML = F,
                     data = data_rt_stats)

rt_log10.lmer_sum = summary(rt_log10.lmer)
rt_log10.lmer_sum

# Test for effect of congruency
rt_log10_congruency.lmer = lmer(rt_log10 ~ congruency_contrast * half_contrast -
                                  congruency_contrast +
                                  (1+congruency_contrast*half_contrast|subject_id) +
                                  (1|item) +
                                  (0+half_contrast|item), REML = F,
                                data = data_rt_stats)

rt_log10_congruency.anova = anova(rt_log10.lmer, rt_log10_congruency.lmer) 
rt_log10_congruency.anova

# Test for effect of experiment half
rt_log10_half.lmer = lmer(rt_log10 ~ congruency_contrast * half_contrast -
                            half_contrast +
                            (1+congruency_contrast*half_contrast|subject_id) +
                            (1|item) +
                            (0+half_contrast|item), REML = F,
                          data = data_rt_stats)

rt_log10_half.anova = anova(rt_log10.lmer, rt_log10_half.lmer)
rt_log10_half.anova

# Test for interaction of congruency and experiment half
rt_log10_congruencyxhalf.lmer = lmer(rt_log10 ~ congruency_contrast * half_contrast -
                                       congruency_contrast:half_contrast +
                                       (1+congruency_contrast*half_contrast|subject_id) +
                                       (1|item) +
                                       (0+half_contrast|item), REML = F,
                                     data = data_rt_stats)

rt_log10_congruencyxhalf.anova = anova(rt_log10.lmer, rt_log10_congruencyxhalf.lmer)
rt_log10_congruencyxhalf.anova

