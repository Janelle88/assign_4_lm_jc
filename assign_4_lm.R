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

##### daphne's comments/advice/love
# scale_x_discrete(site_name1, site_name2, etc.)


ggplot(data = lobster_size, 
       aes(y = size_mm, x = site_name)) +
  geom_jitter(aes(color = site_name),
              show.legend = FALSE) +
  scale_color_manual(breaks = c("MPA", "Non-MPA"), values = c("sandybrown", "sandybrown", "sandybrown", "royalblue4","royalblue4")) +
  geom_boxplot(alpha = 0.5, outlier.color = NA) +
  theme_minimal() +
  labs(title = "x",
     x = "year", 
     y = "size (mm)",
     caption = "caption")




# -------
# Graph C
# -------

