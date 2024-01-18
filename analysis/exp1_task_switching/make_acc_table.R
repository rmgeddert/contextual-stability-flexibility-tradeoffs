library(tidyverse)
library(plotrix)
library(patchwork)
library(gt)

setwd("path/to/contextual-stability-flexibility-tradeoffs/")

df <- read_csv("data/exp1_task_switching/acc_df.csv") %>% 
  group_by(subject, switchProp, incProp, taskSequence, congruency) %>% 
  summarise(mean_acc = mean(acc))

df_means <- df %>%
  group_by(switchProp, incProp, taskSequence, congruency) %>%
  summarise(acc = mean(mean_acc), sem=std.error(mean_acc))

vals <- df_means['acc'] * 100
vals$sem = df_means$sem * 100
vals$acc <- round(vals$acc, digits=1)
vals$sem <- format(round(vals$sem, digits = 2),nsmall=1)
vals$acc <- as.character(vals$acc)
vals$sem <- as.character(vals$sem)
vals$sem <- paste('(',vals$sem,')',sep="")
new_vals <- paste(vals$acc, vals$sem)
new_vals <- matrix(new_vals, nrow = 4, ncol=4)
new_labs <- matrix(c('25/25','25/75','75/25','75/75'),ncol=1)
CTI <- cbind(new_labs,new_vals)
data_CTI <- as.data.frame(CTI)

data_CTI

table_CTI <- gt(data_CTI) %>%
  tab_header(
    title = "Exp 1 Accuracies"
  ) %>%
  cols_label(
    V1 = "", #Block Type:
    V2 = "Congruent",
    V3 = "Incongruent",
    V4 = "Congruent",
    V5 = "Incongruent"
  ) %>%
  #tab_spanner(
  #  label = "", #Trial Type:
  #  columns = c(V1)
  # ) %>%
  tab_spanner(
    label = "Repeat",
    columns = c(V2, V3)
  ) %>%
  tab_spanner(
    label = "Switch",
    columns = c(V4, V5)
  ) %>%
  # tab_source_note(
  #   source_note = "25/25, 25/75, etc. refer to block-wise switch percentage / incongruency percentage"
  # ) %>%
  
  
  tab_style(
    style = list(
      cell_fill(color="whitesmoke"),
      cell_text(weight="bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color="whitesmoke"),
      cell_text(weight="bold")
    ),
    locations = cells_column_spanners()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color="whitesmoke")
    ),
    locations = cells_body(
      columns = c(V1)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(3),
      style = "solid"
    ),
    locations = cells_body(
      columns = c(V1)
    )
  ) %>%
 
  tab_style(
    style = cell_borders(
      sides = c("left"),
      color = "#BBBBBB",
      weight = px(3),
      style = "solid"
    ),
    locations = cells_column_labels(
      columns = c(V2)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(3),
      style = "solid"
    ),
    locations = cells_column_labels(
      columns = c(V3)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(3),
      style = "solid"
    ),
    locations = cells_body(
      columns = c(V3)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_column_labels(
      columns = c(V2, V4, V5)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(3),
      style = "solid"
    ),
    locations = cells_column_labels(
      columns = c(V5)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = c(V2, V4)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      color = "#BBBBBB",
      weight = px(3),
      style = "solid"
    ),
    locations = cells_body(
      columns = c(V5)
    )
  ) %>%
  tab_options(
    data_row.padding = px(5)
  )


table_CTI

