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
library(directlabels)

lobster_df <- read_csv("lobster_abundance_sbc_lter.csv", na = "-99999") %>% 
  clean_names()

lobster_year <- lobster_df %>% 
  group_by(site, year) %>% 
  summarize("total" = sum(count)) %>% 
  mutate("MPA" = ifelse(site == "IVEE", "MPA",
                        ifelse(site == "NAPL", "MPA", "Non-MPA"))) %>% 
  mutate("site_name" = ifelse(site == "IVEE", "Isla Vista",
                              ifelse(site == "NAPL", "Naples",
                                     ifelse(site == "CARP", "Carpinteria",
                                            ifelse(site == "AQUE", "Arroyo Quemado", "Mohawk")))))

ggplot(data = lobster_year, aes(x = year, y = total, group = site)) +
  geom_line(aes(color = MPA), size = 1.2) +
  labs(color = "Marine Protected Area (MPA)") +
  theme_light() +
  geom_dl(aes(label = site_name, color = MPA), method = list(dl.combine("last.points"), cex = 0.8))+
  theme(legend.position = c(0.29, 0.849)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(2012,2020.5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1000)) +
  scale_color_manual(breaks = c("MPA", "Non-MPA"), values = c("royalblue4", "sandybrown")) +
  labs(caption = bold("Figure 1.")~italic("more caption"))
  


# ggplot(data = lobster_year, aes(x = year, y = total, group = site)) +
#   geom_col()

# geom_text(data = lobster_year %>% filter(year == last(year)), aes(label = site, 
#                                                                   x = year + 0.5, 
#                                                                   y = total, 
#                                                                   color = site)) ugly
# Move on to next graph