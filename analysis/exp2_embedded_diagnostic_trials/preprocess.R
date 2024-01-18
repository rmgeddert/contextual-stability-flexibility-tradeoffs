library(tidyverse)

setwd("path/to/contextual-stability-flexibility-tradeoffs")

#load raw data
df <- read_csv("data/exp2_embedded_diagnostic_trials/raw_data.csv") %>% 
  filter(sectionType == 'mainTask') %>% 
  select(c(subject, block, blockType, trialCount, acc, RT, congruency, cuedTask, taskSequence, sequenceType, sequenceKind, sequencePosition)) %>% 
  mutate(switchProp = ifelse(blockType == "B" | blockType == "D", "75%", "25%")) %>% 
  mutate(incongruentProp = ifelse(blockType == "A" | blockType == "B", "75%", "25%"))
str(df)

#accuracy exclusion
acc_cutoff <- .75
accuracies <- df %>% 
  group_by(subject) %>% 
  summarise(mean_acc = mean(acc, na.rm = TRUE)) 
excluded_subs <- accuracies %>% 
  filter(mean_acc < acc_cutoff) %>% 
  select(subject, mean_acc)

#remove subjects
df <- df %>% 
  filter(! subject %in% excluded_subs$subject) 

#accuracy
acc_all <- df %>% 
  filter(taskSequence != 'n')

#save acc all
write_csv(acc_all, 'data/acc_all.csv')
  
#and remove filler trials
acc_sequence <- df %>% #30208
  filter(sequenceType == 's') %>% #15104
  filter(sequencePosition != 1) #11328

# save data
write_csv(acc_sequence, 'data/acc_sequence.csv')

#RT all
RT_all <- df %>% #36480
  filter(acc == 1) %>% #32416
  filter((abs(RT - mean(RT,na.rm = TRUE)) <= 3*sd(RT, na.rm = TRUE))) %>% #32385
  filter(RT >= 300) %>% 
  filter(RT <= 1500) %>% #32349
  filter(taskSequence != "n") #32186

write_csv(RT_all, 'data/RT_all.csv')

#RT sequence
RT_sequence <- RT_all %>% 
  filter(sequenceType == 's') %>% 
  filter(sequencePosition != 1) 

write_csv(RT_sequence, 'data/RT_sequence.csv')