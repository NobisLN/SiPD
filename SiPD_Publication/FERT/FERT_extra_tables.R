
library(gt)
library(viridis)
library(tidyverse)
library(paletteer)
library(webshot)


dataset <- read.csv("/Users/lnobis/Documents/GitHub/Serotonin_in_PD/SSRI_Analysis/FERT/table1.csv", stringsAsFactors = FALSE)


dems <- dataset %>%
  gt(groupname_col = "group") %>%
  cols_label(X = html("<b></b>"),
             effect = html("<b>Effect size (n2p)</b>"),
             F = html("<b>F-test</b>"),
             p = html("<b>p-value</b>")) %>%
  tab_options(table.border.top.color = 'white',
              table_body.hlines.style = 'none',   table.font.size = 17) %>%
  cols_align(align = "center") %>%
  cols_width(
    starts_with("F") ~ px(200),
    starts_with("p") ~ px(200)) %>%
  data_color(
    columns = "X",
    colors = '#EBEBEB', 
    alpha = 0.9)
dems



dataset <- read.csv("/Users/lnobis/Documents/GitHub/Serotonin_in_PD/SSRI_Analysis/FERT/table2.csv", stringsAsFactors = FALSE)

dems <- dataset %>%
  gt(groupname_col = "group") %>%
  cols_label(X = html("<b></b>"),
             effect = html("<b>Effect size (n2p)</b>"),
             F = html("<b>F-test</b>"),
             p = html("<b>p-value</b>")) %>%
  tab_options(table.border.top.color = 'white',
              table_body.hlines.style = 'none',   table.font.size = 17) %>%
  cols_align(align = "center") %>%
  cols_width(
    starts_with("F") ~ px(200),
    starts_with("p") ~ px(200)) %>%
  data_color(
    columns = "X",
    colors = '#EBEBEB', 
    alpha = 0.9)
dems

