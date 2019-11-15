# -------
# Janelle Christensen
# Lauren Manzo
# 11/12/19
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
library(plotly)

lobster_size<- read_csv("lobster_abundance_sbc_lter.csv", na = "-99999") %>% 
  clean_names() %>% 
  filter(year %in% c("2012", "2018")) %>% 
  group_by(site, year) %>% 
  select(year, site, size_mm) %>% 
  mutate("MPA" = ifelse(site == "IVEE", "MPA",
                        ifelse(site == "NAPL", "MPA", "Non-MPA"))) %>% 
  mutate("site_name" = ifelse(site == "IVEE", "Isla Vista",
                              ifelse(site == "NAPL", "Naples",
                                     ifelse(site == "CARP", "Carpinteria",
                                            ifelse(site == "AQUE", "Arroyo Quemado", "Mohawk")))))
  
lobster_size$site_name <- factor(lobster_size$site_name , levels=c("Mohawk", "Carpinteria", "Arroyo Quemado", "Naples", "Isla Vista"))

# --------
# Graph B
# --------

ggplot(data = lobster_size, 
       aes(y = size_mm, x = site_name, fill = as.character(year))) +
  geom_point(position=position_jitterdodge(), alpha=0.2, aes(color = site_name),
              show.legend = FALSE,
              size = 1) +
  scale_color_manual(breaks = c("MPA", "Non-MPA"), values = c("sandybrown", "sandybrown", "sandybrown", "royalblue4","royalblue4")) +
  geom_boxplot(alpha = 0.75, outlier.color = NA) +
  scale_fill_grey()+
  theme_minimal() +
  labs(title = "x",
     x = "year", 
     y = "size (mm)",
     caption = "caption",
     color = "year")




# -------
# Graph C
# -------

lobster_size_av <- lobster_size %>% 
  group_by(MPA, year) %>% 
  summarize(mpa_av = mean(size_mm, na.rm = TRUE)) %>% 
  filter(year %in% c(2012, 2018))

ggplot(data = lobster_size_av, aes(x = year)) +
  geom_histogram()

# -----------
# Graph A
# -----------


