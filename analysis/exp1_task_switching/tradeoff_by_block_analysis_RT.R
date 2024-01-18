library(tidyverse)
library(plotrix)
library(ggnewscale)
library(cowplot)

setwd("path/to/contextual-stability-flexibility-tradeoffs")

#load data
df <- read_csv('data/exp1_task_switching/RT_df.csv', show_col_types = FALSE)

# PLOT 1
RT_sub_means <- df %>% 
  group_by(subject, taskSequence, congruency, switchProp, incProp) %>% 
  summarise(mean_RT = mean(RT, na.rm=TRUE)) 

RT_means <- RT_sub_means %>% 
  group_by(taskSequence, congruency, switchProp, incProp) %>% 
  summarise(sem_RT = std.error(mean_RT, na.rm=TRUE), mean_RT = mean(mean_RT, na.rm=TRUE)) %>% 
  mutate(taskSequence = as.factor(taskSequence))

#congruency effects by switch costs
labs <- levels(RT_means$taskSequence)
plt <- ggplot(RT_means, aes(x = as.numeric(RT_means$taskSequence), y = mean_RT, 
                     fill=paste0(RT_means$taskSequence, RT_means$congruency))
) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean_RT - sem_RT, ymax = mean_RT + sem_RT), position=position_dodge(0.9), width=0.3) +
  coord_cartesian(ylim=c(600,1000)) +
  scale_fill_manual(name="Congruency: ",
                    breaks=c("ri", "rc"),
                    values= c("si" = "#023047", "ri" = "#023047", "sc" = "#8ecae6", "rc" = "#8ecae6"),
                    labels=c("rc" = "Congruent", "ri" = "Incongruent")) +
  scale_x_continuous(breaks = 1:length(labs),
                     labels = c("r" = "Repeat", "s" = "Switch"),
                     expand = c(0, 0.6),
                     sec.axis = sec_axis(~ . , name = "Switch Proportion", breaks = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incongruent Proportion", breaks = NULL, labels = NULL)) +
  labs(fill = "Task Sequence") +
  # scale_x_discrete(labels = c('s' = 'Switch', 'r' = 'Repeat')) +
  xlab("Task Sequence") +
  ylab("Reaction Time (ms)") +
  theme_cowplot(font_size=18) +
  facet_grid(incProp ~ switchProp) +
  theme(panel.border = element_rect(color="black", fill=NA, size=0.5),
        panel.spacing = unit(-0.05, "lines"),
        axis.line.x.top = element_blank(),
        axis.line.y.right  = element_blank())
plt_legend <- get_legend(plt + theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))
plt <- plt + theme(legend.position="none")
plt_final <- plot_grid(plt, plt_legend, rel_heights = c(1, 0.1), align = "v", ncol=1)
plt_final

# PLOT 2
RT_sub_congruencyeffects <- RT_sub_means %>% 
  group_by(subject, taskSequence, switchProp, incProp) %>% 
  mutate(RT_total = sum(mean_RT)) %>% 
  filter(congruency == "i") %>% 
  mutate(congruent_RT = RT_total - mean_RT,
         congruency_effect = mean_RT - congruent_RT) %>% 
  select(subject, taskSequence, switchProp, incProp, congruency_effect)

RT_congruencyeffects <- RT_sub_congruencyeffects %>% 
  group_by(taskSequence, switchProp, incProp) %>% 
  summarise(sem_ce = std.error(congruency_effect), mean_ce = mean(congruency_effect)) %>% 
  mutate(taskSequence = as.factor(taskSequence))
labs <- levels(RT_congruencyeffects$taskSequence)

plt2 <- ggplot(RT_congruencyeffects, aes(x = as.numeric(RT_congruencyeffects$taskSequence), y = mean_ce, fill=taskSequence)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean_ce - sem_ce, ymax = mean_ce + sem_ce), position=position_dodge(0.9), width=0.3) +
  scale_fill_manual(values=c("s" = "darkslategrey", "r" = "grey")) +
  # scale_x_discrete(labels = c("r" = "Repeat", "s" = "Switch")) +
  scale_x_continuous(breaks = 1:length(labs),
                     labels = c("r" = "Repeat", "s" = "Switch"),
                     expand = c(0, 0.6),
                     sec.axis = sec_axis(~ . , name = "Switch Proportion", breaks = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Incongruent Proportion", breaks = NULL, labels = NULL)) +
  facet_grid(incProp ~ switchProp) +
  theme_cowplot(font_size = 18) +
  xlab("Task Sequence") +
  ylab("Congruency Effect (ms)") +
  theme(legend.position = "none",
        panel.border = element_rect(color="black", fill=NA, size=0.5),
        panel.spacing = unit(-0.05, "lines"),
        axis.line.x.top = element_blank(),
        axis.line.y.right  = element_blank()) +
  coord_cartesian(ylim=c(0,150))
plt2

# BLOCK LEVEL 
RT_sub_ce_diffs <- RT_sub_congruencyeffects %>%
  ungroup() %>%
  group_by(subject, switchProp, incProp) %>%
  mutate(ce_total = sum(congruency_effect)) %>%
  filter(taskSequence == "s") %>%
  mutate(repeat_ce = ce_total - congruency_effect,
         ce_diff = congruency_effect - repeat_ce) %>%
  select(subject, switchProp, incProp, ce_diff)

RT_ce_diffs <- RT_sub_ce_diffs %>%
  group_by(subject, switchProp, incProp) %>%
  summarise(mean_ce_diff = mean(ce_diff)) %>%
  mutate(proportion = paste(switchProp, incProp, sep="_"))

ggplot(RT_ce_diffs, aes(x = proportion, y = mean_ce_diff, fill=proportion)) +
  geom_boxplot() +
  xlab("Switch_Incongruent Proportion")

# PAIRWISE
pairwise.t.test(RT_ce_diffs$mean_ce_diff, RT_ce_diffs$proportion, p.adjust.method = "bonferroni")
