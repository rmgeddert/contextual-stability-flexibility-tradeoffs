library(tidyverse)
setwd("path/to/contextual-stability-flexibility-tradeoffs")

# load in data
df <- read_csv('data/exp1_task_switching/raw_data.csv')

#filter out none relevant rows
df <- df %>% 
  filter(sectionType == "mainTask")

#exclude participants
acc_cutoff = .75
accuracies <- df %>% 
  group_by(subject) %>% 
  summarise(mean_acc = mean(acc, na.rm = TRUE)) 
excluded_subs <- accuracies %>% 
  filter(mean_acc < acc_cutoff) %>% 
  select(subject, mean_acc)

#final acc df
df <- df %>% 
  filter(!subject %in% excluded_subs$subject) %>% 
  filter(switchType != "n") %>% 
  mutate(switchProp = ifelse(blockType == "B" | blockType == "D", "75%", "25%")) %>% 
  mutate(incProp = ifelse(blockType == "A" | blockType == "B", "75%", "25%")) %>% 
  rename(congruency = stimCongruency, taskSequence = switchType)
write_csv(df, 'data/exp1_task_switching/acc_df.csv')

# final RT df
RT_df <- df %>% #30480
  group_by(subject) %>% 
  filter(acc == 1) %>% #28081
  filter((abs(RT - mean(RT,na.rm = TRUE)) <= 3*sd(RT, na.rm = TRUE))) %>% #27911
  filter(RT >= 300) %>% 
  filter(RT <= 1500) #27886
write_csv(RT_df, 'data/exp1_task_switching/RT_df.csv')