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
library(kableExtra)

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
                                            ifelse(site == "AQUE", "Arroyo Quemado", "Mohawk"))))) # this allows me to have a new column with the non-abbreviated site names


ggplot(data = lobster_year, aes(x = year, y = total, group = site)) + # want individual lines by site, need group = site
  geom_line(aes(color = MPA), size = 1.2) +
  labs(color = "Marine Protected Area (MPA)") + #for some reason color changes the legend title
  theme_light() +
  geom_dl(aes(label = site_name), method = list(dl.combine("last.points"), cex = 0.8))+ #geom_dl used for direct label, cex is font size, method needs to be written like this
  theme(legend.position = c(0.29, 0.849)) + # manual repositioning of legend
  scale_x_continuous(expand = c(0,0),
                     limits = c(2012,2020.5)) + # needed to expand my limits for site names
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 1000)) +
  scale_color_manual(breaks = c("MPA", "Non-MPA"), values = c("royalblue4", "sandybrown")) +
  labs(caption = bolditalic("Figure 1.")~italic("more caption")) # caption
  

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

lobster_year_test <- t.test(lobster_year$year, ) # I don't think this shows anything at all

# --------
# Graph B
# --------

##### daphne's comments/advice/love
# scale_x_discrete(site_name1, site_name2, etc.)
# breaks = c("Non-MPA", "MPA") was in scale_color_manual but removed it

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

# let's visually explore this data and explore the means of the data

ggplot(data = lobster_size, aes(x = size_mm)) +
  geom_histogram() +
  facet_wrap(~site)

# means look evenly distributed, but let's do a qq plot to make sure

ggplot(data = lobster_size, aes(sample = size_mm)) +
  geom_qq() +
  facet_wrap(~site)

# means look normally distributed, despite a few outliers at carpinteria and IV

# to really see the data, I'm going to make a summary table

lobster_table <- lobster_size %>% 
  group_by(site_name) %>% 
  summarize(mean_size = mean(size_mm, na.rm = TRUE),
            sd_size = sd(size_mm, na.rm = TRUE),
            lobster_number = n())

# to make the table accessible on an html I will use the kable function

lobster_table %>% 
  kable(col.names = c("Site Name",
                      "Mean Lobster Size (cm)",
                      "Standard Deviation of Lobster Size",
                      "Number of Lobsters")) %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center") %>% 
  add_header_above(bold = TRUE,line = TRUE, c("Statistics of Lobsters in the Santa Barbara Channel" = 4))


