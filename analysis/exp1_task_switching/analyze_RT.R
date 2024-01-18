library(tidyverse)
library(lme4) # for linear mixed effects regression
library(lmerTest) #for getting p values from lme
library(broom.mixed) #for turning lmm results into tidy dataframe
library(knitr) #for turning tidy lmm results into latex table
library(emmeans) #for getting marginal means

setwd("path/to/contextual-stability-flexibility-tradeoffs/")

#load data
df <- read_csv('data/exp1_task_switching/RT_df.csv')

#fit fixed model
lmm <- lmer(RT ~ congruency * taskSequence * switchProp * incProp + 
                  (1|subject), data=RT_df)
summary(lmm)

getMixedLatex <- function(lmm_model){
  clean_lmm <- tibble(tidy(lmm_model)) %>% 
    filter(! effect == "ran_pars") %>% #remove random effect rows
    select(-c("effect", "group", "df")) %>%  # remove bad columns
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
           term = gsub("incProp75%", "incProp", term)) %>% 
    mutate(across(c("estimate", "std.error", "statistic"), function(x){round(x, digits=2)})) %>% 
    mutate(term = ifelse(term == "(Intercept)", "Intercept", term))
  kable(clean_lmm, format="latex",align="lcccc", vline="", linesep="")
}

printEMMeans <- function(aovObj){
  defaultW <- getOption("warn") 
  options(warn = -1) 
  
  # task sequence
  emm <- emmeans(aovObj, ~ taskSequence)
  print(summary(emm))
  
  #congruency
  emm <- emmeans(aovObj, ~ congruency)
  print(summary(emm))
  
  # switchProp
  emm <- emmeans(aovObj, ~ switchProp)
  print(summary(emm))
  
  #incongruentProp
  emm <- emmeans(aovObj, ~ incProp)
  print(summary(emm))
  
  #LWPS
  emm <- emmeans(aovObj, ~ taskSequence | switchProp)
  print(contrast(emm, interaction = "pairwise"))
  
  #LWPC
  emm <- emmeans(aovObj, ~ congruency | incProp)
  print(contrast(emm, interaction = "pairwise"))
  
  #cross interaction
  emm <- emmeans(aovObj, ~ congruency | switchProp)
  print(contrast(emm, interaction = "pairwise"))
  
  emm <- emmeans(aovObj, ~ taskSequence | incProp)
  print(contrast(emm, interaction = "pairwise"))
  
  emm <- emmeans(aovObj, ~ switchProp | incProp)
  print(contrast(emm, interaction = "pairwise"))
  
  options(warn = defaultW)
}

getMixedLatex(lmm)
printEMMeans(lmm)

#look at congruency by task sequence interaction within block type
lmer_sum <- function(x,...) {
  m <- lmer(x,...)
  tibble(tidy(m))
}

ts_con_interaction_analysis <- RT_df %>% 
  group_by(switchProp, incProp) %>% 
  do(lmer_sum(., formula = RT ~ congruency * taskSequence + (1|subject))) %>% 
  filter(term == "congruencyi:taskSequences") %>% 
  mutate(p.value = case_when(p.value <= .001 ~ .001, 
                             p.value > .999 ~ .999, 
                             .default = round(p.value, digits=3)),
         term = "congruency:taskSequence")
ts_con_interaction_analysis

final <- ts_con_interaction_analysis %>% 
  ungroup() %>% 
  mutate(block = paste0(gsub("%", "", switchProp), "/", gsub("%", "", incProp))) %>% 
  select(block, term, estimate, statistic, p.value) %>% 
  mutate(estimate = round(estimate, 2),
         statistic = round(statistic, 2))
kable(final, format="latex",vline="", linesep="")
