library(tidyverse)
library(emmeans) #for getting marginal means
library(lme4) # for linear mixed effects regression
library(lmerTest) #for getting p values from lme
options(scipen=999)
library(lme4) # for linear mixed effects regression
library(lmerTest) #for getting p values from lme
library(broom.mixed) #for turning lmm results into tidy dataframe
library(knitr) #for turning tidy lmm results into latex table
setwd("path/to/contextual-stability-flexibility-tradeoffs")

#' function to do analyze
prepareDataSets <- function(df){
  # get means by subject
  acc_means <- df %>% 
    group_by(subject, congruency, taskSequence, switchProp, incongruentProp) %>% 
    summarise(mean_acc = mean(acc, na.rm = TRUE))
  
  acc_sub_means <- df %>% 
    group_by(subject) %>% 
    summarise(mean_acc = mean(acc, na.rm = TRUE))
  
  return(list(acc_means = acc_means, acc_sub_means = acc_sub_means))
}

getMixedLatex <- function(lmm_model){
  clean_lmm <- tibble(tidy(lmm_model)) %>% 
    filter(! effect == "ran_pars") %>% #remove random effect rows
    select(-c("effect", "group")) %>%  # remove bad columns
    mutate(p.value = case_when(p.value <= .001 ~ .001, 
                               p.value > .999 ~ .999, 
                               .default = round(p.value, digits=3))) %>% #correct p-values
    # mutate(sig = case_when(p.value <= .001 ~ "***",
    #                        p.value <= .01 ~ "**",
    #                        p.value <= .05 ~ "*",
    #                        .default="")) %>% 
    mutate(term = gsub("congruencyi", "congruency", term),
           term = gsub("taskSequences", "taskSeq", term),
           term = gsub("switchProp75%", "switchProp", term),
           term = gsub("incongruentProp75%", "incProp", term)) %>% 
    mutate(across(c("estimate", "std.error", "statistic"), function(x){round(x, digits=2)})) %>% 
    mutate(term = ifelse(term == "(Intercept)", "Intercept", term))
  kable(clean_lmm, format="latex",align="lcccc", vline="", linesep="")
}

printEMMeans <- function(aovObj){
  defaultW <- getOption("warn") 
  options(warn = -1) 
  
  # task sequence
  emm <- emmeans(aovObj, ~ taskSequence, type="response")
  print(summary(emm))
  
  #congruency
  emm <- emmeans(aovObj, ~ congruency, type="response")
  print(summary(emm))
  
  # switchProp
  emm <- emmeans(aovObj, ~ switchProp, type="response")
  print(summary(emm))
  
  #incongruentProp
  emm <- emmeans(aovObj, ~ incongruentProp, type="response")
  print(summary(emm))
  
  #LWPS
  emm <- emmeans(aovObj, ~ taskSequence | switchProp, regrid="response")
  print(contrast(emm, interaction = "pairwise"))
  
  #LWPC
  emm <- emmeans(aovObj, ~ congruency | incongruentProp, regrid="response")
  print(contrast(emm, interaction = "pairwise"))
  
  #cross interaction
  emm <- emmeans(aovObj, ~ congruency | switchProp, regrid="response")
  print(contrast(emm, interaction = "pairwise"))
  
  emm <- emmeans(aovObj, ~ taskSequence | incongruentProp, regrid="response")
  print(contrast(emm, interaction = "pairwise"))
  
  emm <- emmeans(aovObj, ~ switchProp | incongruentProp, regrid="response")
  print(contrast(emm, interaction = "pairwise"))
  
  options(warn = defaultW)
}

#############################################################

# Same for full data set
all <- read_csv('data/exp2_embedded_diagnostic_trials/acc_all.csv') %>% 
  mutate(across(c(congruency, taskSequence, subject, switchProp, incongruentProp), as.factor))

all_dfs <- prepareDataSets(all)

#check for normality
sub_means <- all_dfs[[2]]
print(mean(sub_means$mean_acc))
print(sd(sub_means$mean_acc))
hist(sub_means$mean_acc)

all_lmm <- glmer(acc ~ congruency * taskSequence * switchProp * incongruentProp + 
                   (1|subject), data=all, family=binomial)
summary(all_lmm)
getMixedLatex(all_lmm)

printEMMeans(all_lmm)

#############################################################

# sequence data analysis
sequence <- read_csv('data/exp2_embedded_diagnostic_trials/acc_sequence.csv') %>% 
  mutate(across(c(congruency, taskSequence, subject, switchProp, incongruentProp), as.factor))

sequence_dfs <- prepareDataSets(sequence)

#check for normality
sub_means <- sequence_dfs[[2]]

print(mean(sub_means$mean_acc))
print(sd(sub_means$mean_acc))

hist(sub_means$mean_acc)

sequence_lmm <- glmer(acc ~ congruency * taskSequence * switchProp * incongruentProp + 
                       (1|subject), data=sequence, family=binomial)
summary(sequence_lmm)
getMixedLatex(sequence_lmm)

printEMMeans(sequence_lmm)

#############################################################

lmer_sum <- function(x) {
  m <- glmer(acc ~ congruency * taskSequence + (1|subject), family = binomial, data=x)
  tibble(tidy(m))
}

ts_con_interaction_analysis <- sequence %>% 
  group_by(switchProp, incongruentProp) %>% 
  do(lmer_sum(.)) %>% 
  filter(term == "congruencyi:taskSequences") %>% 
  mutate(p.value = case_when(p.value <= .001 ~ .001, 
                             p.value > .999 ~ .999, 
                             .default = round(p.value, digits=3)))
ts_con_interaction_analysis

final <- ts_con_interaction_analysis %>% 
  ungroup() %>% 
  mutate(block = paste0(gsub("%", "", switchProp), "/", gsub("%", "", incongruentProp))) %>% 
  select(block, term, estimate, statistic, p.value) %>% 
  mutate(estimate = round(estimate, 2),
         statistic = round(statistic, 2))
kable(final, format="latex",vline="", linesep="")