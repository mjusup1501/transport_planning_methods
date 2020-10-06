# 0 set up ----
pkgs <- c("tidyverse", "lubridate", "broom", "dplyr", "here", "purrr")
new_packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(lubridate)
library(broom)
library(dplyr)
library(purrr)
library(here)

figs <- "/Users/matej/Projects/transport_planning_methods/figures/"

# 0 load data ----
load("/Users/matej/Projects/transport_planning_methods/Data-20201001/Mobidrive_2002.RData")

# 1 preps ----
# filter for karlsruhe and mainstudy participants
hh <- household_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)
p <- person_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)
trip <- trip_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)


# join by household number, city code and studycode
# create UID (user ID) based on city code, study code, household number and
# person number; sort it ascending based on UID
p_hh <- p %>% inner_join(hh, by = c("HH_NR","CITYCODE","STUDYCOD")) %>%
  mutate(UID = CITYCODE*1000000 + STUDYCOD*100000 + HH_NR*10 + P_NR) %>%
  arrange(UID, HH_NR, P_NR) %>%
  select(UID, HH_NR, P_NR, everything())


# 2.1 ----
# basic info
nrow(hh)
nrow(p)
nrow(p_hh)

# household size
# compute tidy summary of each var

# summary of household size
p_hh %>%
  select(N_O_HHM) %>%
  map_df(~tidy(summary(.x)), .id = "variable")  

# age distribution
# by sex

# add character sex variable
p_hh$sex_ch <- ifelse(p_hh$SEX == 0, "female", "male")

# age density
ggplot(p_hh, aes(x=AGE)) +
  geom_density(alpha=0.4) + theme_bw() +
  theme(legend.title=element_blank(),legend.position="bottom") +
  xlab("Age") +
  ylab("Density") +
  ylim(c(0,0.02))

# age boxplot
ggplot(p_hh, aes(x="", y=AGE)) +
  geom_boxplot(alpha=0.4, na.rm=TRUE) +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position="bottom") + 
  ylab("Age")

# age density by sex
ggplot(p_hh, aes(x=AGE, fill=sex_ch)) +
  geom_density(alpha=0.4) + theme_bw() +
  theme(legend.title=element_blank(),legend.position="bottom") +
  xlab("Age") +
  ylab("Density") +
  ylim(c(0,0.02))
ggsave(file = here::here("plots","age_distr_density.pdf"), width = 20, height = 15, units = "cm")

# age boxplot by sex
ggplot(p_hh, aes(y=AGE, x=sex_ch)) +
  geom_boxplot(alpha=0.4, na.rm=TRUE) +
  theme_bw() +
  theme(legend.title=element_blank(),legend.position="bottom") + 
  xlab("Sex") +
  ylab("Age")
ggsave(file = here::here("plots","age_distr_boxplot.pdf"), width = 20, height = 15, units = "cm")

# age violin plot by sex (boxplot + density)
ggplot(p_hh, aes(x=sex_ch, y=AGE)) + 
  geom_violin(alpha = 0.5) +
  #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.2, size = 0.5, linetype = "dotted") +
  theme_bw() +
  labs(x="Gender", y = "Age", caption = "Dotted: Mean\nSolid: Median") +
  theme(text = element_text(size = 18))
ggsave(file = here::here("plots","age_distr_violinboxplot.pdf"), width = 20, height = 15, units = "cm")

# education
str(list(p_hh$EDUC,p_hh$EMPLOYED))
p_hh$educ_f <- factor(p_hh$EDUC, 0:11, c("None","Primary","Obligatory schooling","Intermediate 16y",
                                         "Limited highschool","Full highschool","Full highschool (east)",
                                         "Apprenticeship","Craft master","Technical college (FH/PH)",
                                         "University (Science/Engeneering)","University (Arts/Social sciences)"))

p_hh$employed_f <- factor(p_hh$EMPLOYED, 0:1, c("no","yes"))

p_hh %>%
  group_by(educ_f, employed_f) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = educ_f, y = n, fill = employed_f)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  xlab("Education level") +
  ylab("Frequency") +
  labs(fill = "Employed?") +
  ylim(c(0,20)) +
  coord_flip() +
  ggtitle("Education levels by employment status") + 
  theme(legend.position = "bottom")
ggsave(file = here::here("plots","educlevels_by_employstatus.pdf"), width = 20, height = 15, units = "cm")

# income
# page 80 & 81 sch?nfelder
# N_O_V: Number of all vehicles: motorized and non-motorized.
# N_O_VEH: only cars and trucks
# HH_INC: household income

str(p_hh$HH_INC)
unique(p_hh$HH_INC)
p_hh$hh_inc_f <- factor(p_hh$HH_INC,
                          c(0.7,1.4,2.2,2.8,3.5,4.5,6,9),
                          c("Up to 1000 DM","1000 - 1799 DM","1800 - 2499 DM",
                            "2500 - 2999 DM","3000 - 3999 DM","4000 - 4999 DM",
                            "5000 - 7499 DM","7500+ DM"))

p_hh %>%
  filter(!is.na(hh_inc_f) & !is.na(N_O_V)) %>%
  ggplot(aes(x = hh_inc_f, y = N_O_V)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 315, vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1), name = "Number of vehicles") +
  scale_x_discrete(name = "Household income") #+ coord_flip()
ggsave(file = here::here("plots","educlevels_by_employstatus.pdf"), width = 20, height = 15, units = "cm")


# 2.2 ----
trip <- trip %>%
  mutate(UID = CITYCODE*1000000 + STUDYCOD*100000 + HH_NR*10 + P_NR) %>%
  arrange(UID, HH_NR, P_NR, T_NR) %>%
  select(UID, HH_NR, P_NR, T_NR, everything())

# D_O_W: day-of-week
trip$D_O_W
trip$dow_f <- factor(trip$D_O_W, 1:7, c("Monday", "Tuesday", "Wednesday","Thursday",
                                        "Friday", "Saturday", "Sunday"))

# Convert the data time format from SPSS to Date, http://scs.math.yorku.ca/index.php/R:_Importing_dates_from_SPSS
trip$doy <- as.Date(trip$D_O_Y/86400, origin = ISOdate(1582,10,14))


# trips per doy
trip %>%
  group_by(UID, dow_f) %>%
  summarise(count = n()) %>%
  ggplot(aes(y = count, x = dow_f)) +
  geom_boxplot() +
  ylab("Number of trips") +
  xlab("Weekday")
ggsave(file = paste0(figs,"nroftrips_per_weekday.jpg"), width = 20, height = 15, units = "cm")

# number of trips vs income
trip_p_hh <- trip %>%
  inner_join(p_hh, by = c("UID","CITYCODE","STUDYCOD","HH_NR","P_NR"))

# Eta correlation
eta <- function(df, squared = FALSE) {
  ## group mean
  mg <- df %>%
    group_by(category) %>%
    summarise(mean = mean(count)) %>%
    pull(mean)
  
  ## group size
  ng <- df %>%
    group_by(category) %>%
    summarise(count = n()) %>%
    pull(count)
  
  ## total mean
  mtot <- df %>%
    summarise(mean = mean(count)) %>%
    pull(mean)
  
  ## SSb
  ssb <- sum(ng * (mg - mtot) ^ 2)
  ## SSt
  sst <- sum((y - mtot) ^ 2)
  # get eta-squared
  if (squared) {
    res <- ssb/sst
    # get eta
  } else {
    res <- sqrt(ssb/sst)
  }
  return(res)
}

# Eta correlation - trips per day-of-week
df <- trip_p_hh %>%
  filter(!is.na(dow_f)) %>%
  group_by(UID, dow_f) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  select(category=dow_f, count)
eta(df)


trip_p_hh %>%
  filter(!is.na(hh_inc_f)) %>%
  group_by(UID, hh_inc_f) %>%
  summarise(count = n()) %>%
  ggplot(aes(y = count, x = hh_inc_f)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 315, vjust = 0.5)) +
  ylab("Number of trips") + xlab("Income")
ggsave(file = paste0(figs, "nroftrips_per_income.jpg"), width = 20, height = 15, units = "cm")

df <- trip_p_hh %>%
  filter(!is.na(hh_inc_f)) %>%
  group_by(UID, hh_inc_f) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  select(category=hh_inc_f, count)
eta(df)


# total distance travelled (vs income)
tick_labels_2.2 <- trip_p_hh %>%
  filter(!is.na(hh_inc_f)) %>%
  group_by(hh_inc_f) %>%
  summarise(total_dist = round(sum(T_DIST)/1000,1)) #%>% select(total_dist))

trip_p_hh %>%
  filter(!is.na(hh_inc_f)) %>%
  group_by(UID, hh_inc_f) %>%
  summarise(total_dist = sum(T_DIST)/1000) %>%
  ggplot(aes(y = total_dist, x = hh_inc_f)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 315, vjust = 0.5)) +
  ylab("Total distance per ID (in km)") + xlab("Household income") +
  scale_y_continuous(breaks = c(seq(0,16, by = 2))) +
  annotate("label", x = tick_labels_2.2$hh_inc_f, y = 10, label = str_wrap(paste("Total:",as.character(tick_labels_2.2$total_dist),"km"),7)) +
  theme(plot.margin = margin(t = 10, unit = "pt")) + ## pad "t"op region of the plot
  coord_cartesian(clip = "off")
ggsave(file = paste0(figs,"totdist_per_income.jpg"), width = 20, height = 15, units = "cm")

# mode choice
trip_p_hh$mainmode_time <- factor(ifelse(between(trip_p_hh$T_MM_B_T,1,2),"Slow modes",
                                                 ifelse(between(trip_p_hh$T_MM_B_T,3,5),"Personal vehicle",
                                                        ifelse(between(trip_p_hh$T_MM_B_T,6,9),"Public transport",""))),
                                  levels = c("Personal vehicle","Slow modes","Public transport"))

# modal split by time
trip_p_hh %>%
  group_by(dow_f, mainmode_time) %>%
  summarise(n_T = n()) %>%
  mutate(freq_T = round((n_T/sum(n_T))*100,1)) %>%
  ggplot(aes(x = dow_f, y = freq_T, fill = mainmode_time)) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = freq_T), position = position_fill(vjust = 0.5, reverse = TRUE)) +
  ylab("Modal split") + xlab("Weekday") + labs(fill = "Modes:") +
  scale_y_continuous(breaks = c(seq(0,1, by = 0.2))) +
  ggtitle("Mode commute time per weekday")
ggsave(file = paste0(figs, "modechoicetime_weekday.jpg"), width = 20, height = 15, units = "cm")

# peak hours

# peak hour definition? Departure? or Arrival? Or the whole trip in a category?
# we consider trips departure time!

str(trip$T_DEP) # no value labels in original trips df
head(trip_p_hh[,c("T_DEP","T_ARR","T_DUR")])

# 1. total trips by hour for all days in a week, otherwise you might exclude weekend days

# make categories for each half an hour (xx:01-xx:30 ->x.5 and xx:31-xx:00-->x.0)
for (row in c(1:nrow(trip_p_hh)))
{ 
  for (i in 1:48)
  { 
    if ((trip_p_hh$T_DEP[row] > ((i-1)*1800) && (trip_p_hh$T_DEP[row] <= (i*1800))))
    {trip_p_hh$T_DEP_cat[row] = (i/2)}
  }
}

# check NA's
which(is.na(trip_p_hh$T_DEP_cat))

# create two classes (1=morning peak hour and evening peak hour, 2=rest of the day)
for (row in c(1:nrow(trip_p_hh)))
{  
  if (trip_p_hh$T_DEP_cat[row] > 7 && trip_p_hh$T_DEP_cat[row] <=9 || trip_p_hh$T_DEP_cat[row] > 16 && trip_p_hh$T_DEP_cat[row] <=18.5)
  {trip_p_hh$T_DEP_ph[row] = 1} #1=Morning peak hour and evening peak hour
  else
  {trip_p_hh$T_DEP_ph[row] = 2} #2=rest of the day
}

# make factors
trip_p_hh$T_DEP_ph = as.factor(trip_p_hh$T_DEP_ph)

# exclude weekend
weekday_tr<-subset(trip_p_hh, dow_f !="Satruday" & dow_f!="Sunday")

# plot it
weekday_tr %>%
  group_by(T_DEP_cat,T_DEP_ph) %>%
  summarise(Trip_Nr = length(T_DEP_cat)) %>%
  ggplot(aes(x = T_DEP_cat-0.25, y = Trip_Nr, fill = T_DEP_ph)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = c('lightblue','dimgrey'), name="Period of day",
                    breaks = c("1", "2"), labels = c("Peak hour", "Non-peak hour")) +
  scale_x_continuous(breaks=seq(0,24,1)) +
  labs(title = "Number of trips by hour", x = "Hour", y = "Total trips")
ggsave(file = paste0(figs, "numberoftripsbyhour_peakvsnonpeak.jpg"), width = 20, height = 10, units = "cm")


# 2. total trip distances by hh_nr peak hour and non-peak hour
weekday_tr %>%
  group_by(HH_NR, T_DEP_ph) %>%
  summarise(total_dist = sum(T_DIST)/1000) %>%
  ggplot(aes(y = total_dist, x = T_DEP_ph)) +
  geom_boxplot() +
  labs(title = "Trip distance by household in peak and non-peak hours", x = "", y = "Trip distance by household (in km)") +
  scale_x_discrete(labels = c("Peak hour (7-9 & 16-18:30 hrs)","Non-peak hour"))
ggsave(file = here::here("plots","tripdistbyhhnr_peakvsnonpeak.pdf"), width = 20, height = 15, units = "cm")













