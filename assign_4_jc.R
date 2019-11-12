# -------
# Janelle Christensen
# Lauren Manzo
# 11/11/19
# Lobster Abudance Report
# ESM 206 Assignment 4
# -------

# -------
# attach packages
# read in csv
# -------

library(tidyverse)
library(janitor)

lobster_df <- read_csv("lobster_abundance_sbc_lter.csv", na = "-99999") %>% 
  clean_names()

lobster_year <- lobster_df %>% 
  group_by(site, year) %>% 
  summarize("total" = sum(count)) %>% 
  mutate("mpa" = site %in% c("IVEE", "NAPL"))

ggplot(data = lobster_year, aes(x = year, y = total)) +
  geom_line(aes()) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))