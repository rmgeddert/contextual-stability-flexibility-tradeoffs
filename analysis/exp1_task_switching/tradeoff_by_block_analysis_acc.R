library(tidyverse)
library(plotrix)
library(ggnewscale)

setwd("path/to/contextual-stability-flexibility-tradeoffs")

#load data
df <- read_csv('data/exp1_task_switching/acc_df.csv', show_col_types = FALSE)

# PLOT 1
acc_sub_means <- df %>% 
  group_by(subject, taskSequence, congruency, switchProp, incProp) %>% 
  summarise(mean_acc = mean(acc, na.rm=TRUE)) 

acc_means <- acc_sub_means %>% 
  group_by(taskSequence, congruency, switchProp, incProp) %>% 
  summarise(sem_acc = std.error(mean_acc, na.rm=TRUE), mean_acc = mean(mean_acc, na.rm=TRUE)) %>% 
  mutate(taskSequence = as.factor(taskSequence))

#congruency effects by switch costs
labs <- levels(acc_means$taskSequence)
plt <- ggplot(acc_means, aes(x = as.numeric(acc_means$taskSequence), y = mean_acc, 
                            fill=paste0(acc_means$taskSequence, acc_means$congruency))
) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean_acc - sem_acc, ymax = mean_acc + sem_acc), position=position_dodge(0.9), width=0.3) +
  coord_cartesian(ylim=c(0.7,1)) +
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
  ylab("Accuracy (%)") +
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
acc_sub_congruencyeffects <- acc_sub_means %>% 
  group_by(subject, taskSequence, switchProp, incProp) %>% 
  mutate(acc_total = sum(mean_acc)) %>% 
  filter(congruency == "i") %>% 
  mutate(congruent_acc = acc_total - mean_acc,
         congruency_effect = mean_acc - congruent_acc) %>% 
  select(subject, taskSequence, switchProp, incProp, congruency_effect)

acc_congruencyeffects <- acc_sub_congruencyeffects %>% 
  group_by(taskSequence, switchProp, incProp) %>% 
  summarise(sem_ce = std.error(congruency_effect), mean_ce = mean(congruency_effect)) %>% 
  mutate(taskSequence = as.factor(taskSequence))
labs <- levels(acc_congruencyeffects$taskSequence)

plt2 <- ggplot(acc_congruencyeffects, aes(x = as.numeric(acc_congruencyeffects$taskSequence), y = mean_ce, fill=taskSequence)) +
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
  ylab("Congruency Effect (%)") +
  theme(legend.position = "none",
        panel.border = element_rect(color="black", fill=NA, size=0.5),
        panel.spacing = unit(-0.05, "lines"),
        axis.line.x.top = element_blank(),
        axis.line.y.right  = element_blank()) +
  coord_cartesian(ylim=c(-0.2, 0))
plt2

# BLOCK LEVEL 
acc_sub_ce_diffs <- acc_sub_congruencyeffects %>%
  ungroup() %>%
  group_by(subject, switchProp, incProp) %>%
  mutate(ce_total = sum(congruency_effect)) %>%
  filter(taskSequence == "s") %>%
  mutate(repeat_ce = ce_total - congruency_effect,
         ce_diff = congruency_effect - repeat_ce) %>%
  select(subject, switchProp, incProp, ce_diff)

acc_ce_diffs <- acc_sub_ce_diffs %>%
  group_by(subject, switchProp, incProp) %>%
  summarise(sem_ce_diff = std.error(ce_diff), mean_ce_diff = mean(ce_diff)) %>%
  mutate(proportion = paste(switchProp, incProp, sep="_"))

ggplot(acc_ce_diffs, aes(x = proportion, y = mean_ce_diff, fill=proportion)) +
  geom_boxplot() +
  xlab("Switch_Incongruent Proportion")

# PAIRWISE
pairwise.t.test(acc_ce_diffs$mean_ce_diff, acc_ce_diffs$proportion, p.adjust.method = "bonferroni")