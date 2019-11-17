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

# -------
# graph A data cleaning
# -------
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

# ------
# Graph A
# ------

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

lobster_year_test <- t.test(lobster_year$year, ) # I don't think this shows anything at all

# --------
# Graph B data cleaning
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


# -------
# Graph C
# -------

lobster_size_av <- lobster_size %>% 
  group_by(MPA, year) %>% 
  summarize(mpa_av = mean(size_mm, na.rm = TRUE)) %>% 
  filter(year %in% c(2012, 2018))

# ------
# graph for MPA sites
# ------

lobster_size_mpa <- lobster_size %>% 
  filter(MPA %in% c("MPA"))
  
ggplot(data = lobster_size_mpa, aes(x = size_mm,
                                    fill = as.character(year),
                                    color = as.character(year))) +
  # as.character allows me to group them by year
  geom_density(alpha = 0.3) +
  scale_fill_manual(breaks = c("2012", 
                               "2018"), 
                    values = c("royalblue4", 
                               "royalblue1")) +
  # color for fill of the curve
  scale_color_manual(breaks = c("2012", 
                                "2018"), 
                     values = c("royalblue4", 
                                "royalblue1")) +
  # color for lines
  theme_minimal() +
  theme(legend.position = "none") + # don't want a legend, label manually
  annotate(
    geom = "curve", # geom="curve" for curved line, geom="segment" for straight
    x = 120, # line start
    y = 0.035, # line start
    xend = 86,  # line end
    yend = 0.028,  # line end
    curvature = .2, # level of curve
    color = "royalblue1"
  ) + 
  # values entered manually for where I want lines to begin and end
  annotate(geom = "text", 
           x = 121, # where my text will be
           y = 0.035, # where my text will be
           label = "2018", 
           hjust = "left", 
           color = "royalblue1") + # annotation for 2018
  annotate(
    geom = "curve", # geom="curve" for curved line, geom="segment" for straight
    x = 85, # start of line
    y = 0.045, # start of line
    xend = 65, # end of line
    yend = 0.037, # end of line
    curvature = .2, # level of curve
    color = "royalblue4"
  ) + 
  # values entered manually for where I want the lines to begin and end
  annotate(geom = "text", 
           x = 86, # where my label will sit
           y = 0.047, # where my label will sit
           label = "2012", 
           hjust = "left", 
           color = "royalblue4") + # annotation for 2012
  geom_vline(xintercept = 67.7,
             color = "royalblue4",
             linetype = "dashed",
             size = 0.5) +
  geom_vline(xintercept = 77.76581,
             color = "royalblue1",
             linetype = "dashed",
             size = 0.5) + # means taken from lobster_size_av dataframe
  # vline allows me to draw a vertical line of the mean
  labs(x = "Lobster Length (mm)",
       y = "Kernel Density",
       caption = bolditalic("Figure 3.")~italic("more caption"))

# ------
# graph for Non-MPA sites
# ------
  

lobster_size_non_mpa <- lobster_size %>% 
  filter(MPA %in% c("Non-MPA"))

ggplot(data = lobster_size_non_mpa, aes(x = size_mm, 
                                        fill = as.character(year),
                                        color = as.character(year))) +
  # as.character allows me to graph by year
  geom_density(alpha = 0.3) +
  scale_fill_manual(breaks = c("2012", 
                               "2018"), 
                    values = c("tomato3", 
                               "sandybrown")) +
  # color for fill of the curve
  scale_color_manual(breaks = c("2012", 
                                "2018"), 
                     values = c("tomato3", 
                                "sandybrown")) +
  # color for lines
  theme_minimal() +
  theme(legend.position = "none") + # don't want a legend, label manually
  annotate(
    geom = "curve", # geom="curve" for curved line, geom="segment" for straight
    x = 117, # line start
    y = 0.035, # line start
    xend = 84, # line end
    yend = 0.028, # line end
    curvature = .2, # level of curve
    color = "sandybrown"
  ) + 
  annotate(geom = "text", 
           x = 119, # where my annotation text is
           y = 0.037, # where my annotation text is
           label = "2018", 
           hjust = "left", 
           color = "sandybrown") + # label for 2018
  annotate(
    geom = "curve", # geom="curve" for curved line, geom="segment" for straight
    x = 120, # line start
    y = 0.015, # line start
    xend = 95,  # line end
    yend = 0.007, # line end
    curvature = .2, # level of curve
    color = "tomato3"
  ) + 
  annotate(geom = "text", 
           x = 121, # where my annotation will be
           y = 0.016, # where my annotation will be
           label = "2012", 
           hjust = "left", 
           color = "tomato3") + # label for 2012
  geom_vline(xintercept = 74.22581,
             color = "tomato3",
             linetype = "dashed",
             size = 0.5) +
  geom_vline(xintercept = 73.82468,
             color = "sandybrown",
             linetype = "dashed",
             size = 0.5) + # means taken from lobster_size_av dataframe
  # vline allows me to draw a vertical line for the means
  labs(x = "Lobster Length (mm)",
       y = "Kernel Density",
       caption = bolditalic("Figure 4.")~italic("more caption"))

# need to filter the dataframes because a t.test within the columns doesn't work

lobster_size_non_mpa_2012 <- lobster_size_non_mpa %>% 
  filter(year == 2012)

lobster_size_non_mpa_2018 <- lobster_size_non_mpa %>% 
  filter(year == 2018)

lobster_size_mpa_2012 <- lobster_size_mpa %>% 
  filter(year == 2012)

lobster_size_mpa_2018 <- lobster_size_mpa %>% 
  filter(year == 2018)

# mpa 2012-2018 t.test
# null hypothesis - there is no significant difference between means in the mpa sites between 2012 and 2018
# alternative - there is a significant difference between means in the mpa sites between 2012 and 2018


mpa_2012_2018_ttest <- t.test(lobster_size_mpa_2012$size_mm, lobster_size_mpa_2018$size_mm)

#non-mpa 2012-2018 t.test
# null hypothesis - there is no significant difference between means in the non-mpa sites between 2012 and 2018
# alternative - there is a significant difference between means in the non-mpa sites between 2012 and 2018

non_mpa_2012_2018_ttest <- t.test(lobster_size_non_mpa_2012$size_mm, lobster_size_non_mpa_2018$size_mm)

# mpa vs non-mpa 2012 t.test
# null hypothesis - there is no significant difference between means between the mpa and non-mpa sites in 2012
# alternative - there is a significant difference between means in the mpa and non-mpa sites between in 2012

mpa_non_mpa_2012_ttest <- t.test(lobster_size_mpa_2012$size_mm, lobster_size_non_mpa_2012$size_mm)

#mpa vs non-mpa 2018 t.test
# null hypothesis - there is no significant difference between means between the mpa and non-mpa sites in 2018
# alternative - there is a significant difference between means in the mpa and non-mpa sites between in 2018

mpa_non_mpa_2018_ttest <- t.test(lobster_size_mpa_2018$size_mm, lobster_size_non_mpa_2018$size_mm)

MPA_lobster_year_2012 <- lobster_year %>%
  filter(year == 2012) %>% 
  filter(MPA == c("MPA")) %>% 
  group_by(year) %>% 
  summarise(total_count = sum(total))

MPA_lobster_year_2012 <- lobster_year %>%
  filter(year %in% c(2012, 2018)) %>% 
  filter(MPA == c("MPA")) %>% 
  group_by(year) %>% 
  summarise(total_count = sum(total)) %>% 
  mutate(avg_growth_mpa = )

#avg_growth_mpa = MPA_lobster_year_2012
  
  
MPA_lobster_year_2018 <- lobster_year %>%
  filter(year == 2018) %>% 
  filter(MPA == c("MPA")) %>% 
  group_by(year) %>% 
  summarise(total_count = sum(total)) 



MPA_join_counts <- 
  full_join(MPA_lobster_year_2012, MPA_lobster_year_2018,
            by = "total_count")