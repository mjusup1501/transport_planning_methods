######################################################################################
#     TPM TRIP PRODUCTION (REGRESSION) MOBI DRIVE DATA
######################################################################################

# 00 set up ----

pkgs <- c("tidyverse", "lubridate", "broom", "dplyr", "here", "purrr", "AER", "stargazer", "leaps", "reshape")
new_packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


# load packages
library(tidyverse) # see cheatsheet for dplyr and ggplot2
library(lubridate) # see cheatsheet for lubridate
library(here)
library(AER) # used for regression
library(stargazer) # nice latex summary output
library(leaps) # computing stepwise regression
library(MASS)
library(reshape)

# see where your path goes
here()

# rm(list=ls())
# 0 create variables ----
var_names <- c("MN_O_PT", "SEX", "AGE", "HH_H", "PARENT", "CHILD", "MARRIED", "EDUC", "EMPLOYED", "WORKING", "N_O_J", "N_O_WP",
          "N_O_WH", "STUDENT", "LIC_VEH", "LIC_MC", "M_CAR_U", "N_O_HHM", "N_O_EM", "N_O_D", "N_O_PV", "N_O_CYC", "N_O_MC",
          "N_O_MP", "N_O_V", "N_O_MV", "HH_INC", "N_O_VEH")

train_df <- p_hh %>% 
            dplyr::select(var_names) %>% 
            mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
summary(train_df)
ggplot(gather(train_df), aes(x=value)) + 
  geom_histogram(bins=10) + 
  facet_wrap(~key, scales = 'free_x')

train_df$AGE_G <- cut(train_df$AGE, 
                    breaks=c(0, 18, 30, 55, 70, 100), 
                    include.lowest=TRUE, 
                    right=TRUE, 
                    labels=c("young", "adult", "middle-age", "pension", "old"))

train_df$HH_INC_LN <- log(train_df$HH_INC)
train_df$HH_INC_EXP <- exp(train_df$HH_INC)
train_df <- train_df %>%
            mutate(EDUC_F = replace(EDUC, EDUC %in% 0:4, "low")) %>%
            mutate(EDUC_F = replace(EDUC_F, EDUC_F %in% 5:8, "middle")) %>%
            mutate(EDUC_F = replace(EDUC_F, EDUC_F %in% 9:11, "high"))

vars_factor <- c("SEX", "AGE_G", "HH_H", "PARENT", "CHILD", "EDUC_F", "MARRIED", "STUDENT",
                 "LIC_VEH", "LIC_MC", "M_CAR_U" ) #"EMPLOYED", "WORKING"
vars_numeric <- c("HH_INC", "N_O_WP", "N_O_WH", "N_O_HHM", "N_O_EM",
                  "N_O_D", "N_O_CYC", "N_O_MC", "N_O_MP", "N_O_VEH") #, "N_O_J", "N_O_PV",  "N_O_V", "N_O_MV", "HH_INC_LN", "HH_INC_EXP"
target <- c("MN_O_PT")

train_df[,vars_factor] <- lapply(train_df[,vars_factor], factor)
train_df <- train_df %>% dplyr::select(c(all_of(target), vars_factor, vars_numeric)) 

# 1 fit a model ----
# Fit the full model
full.model <- lm(formula=MN_O_PT ~ ., data=train_df)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)


#basic stuff
summary(full.model)
summary(step.model) # get a summary output


step.model$na.action # see if there were NA values -> missingness
names(step.model) # see what you can access in the model
coef(step.model) # extract the coefficients
confint(step.model) # get the confidence interval for your parameters


# 2 test assumptions ----
# basic infos, but many insights
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(step.model)
par(mfrow=c(1,1)) # Change back to 1 x 1


# heteroskedasticity: what is H0 and what is HA?
bptest(step.model)


# Studentized residuals
plot(step.model$fitted.value, rstudent(step.model), main="Studentized residuals vs. Fitted", xlab="Fitted values", ylab="Studentized residuals")
abline(h=0, lty=2)
# We want to get rid of observations where studentized residuals have an absolute value higher than 3
stud <- abs(rstudent(step.model)) > 3
sum(stud)
train_df <- train_df[!stud, ] # refit the model
# After two iterations we are good

# We want to discard high-leverage points as well
lev <- cooks.distance(step.model) > (4/nrow(train_df))
sum(lev)
nrow(train_df)
train_df <- train_df[!lev, ]

# (multi-) collinearity
vif(step.model)

# latex output
stargazer(step.model) # copy output directly to overleaf


# SYNTHETIC POPULATION
load(here("data", "Mobidrive_2002_syntheticpopulation.RData"))
hh_syn <- household_syn_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)
p_syn <- person_syn_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)
p_hh_syn <- p_syn %>% inner_join(hh_syn, by = c("HH_NR","CITYCODE","STUDYCOD")) %>%
  mutate(UID = CITYCODE*1000000 + STUDYCOD*100000 + HH_NR*10 + P_NR) %>%
  arrange(UID, HH_NR, P_NR) %>%
  dplyr::select(UID, HH_NR, P_NR, everything())

regression_df <- function(population, var_names, vars_factor, vars_numeric, target){
df <- population %>% 
  dplyr::select(var_names) %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

df$AGE_G <- cut(df$AGE, 
                breaks=c(0, 18, 30, 55, 70, 100), 
                include.lowest=TRUE, 
                right=TRUE, 
                labels=c("young", "adult", "middle-age", "pension", "old"))

df$HH_INC_LN <- log(df$HH_INC)
df$HH_INC_EXP <- exp(df$HH_INC)
df <- df %>%
  mutate(EDUC_F = replace(EDUC, EDUC %in% 0:4, "low")) %>%
  mutate(EDUC_F = replace(EDUC_F, EDUC_F %in% 5:8, "middle")) %>%
  mutate(EDUC_F = replace(EDUC_F, EDUC_F %in% 9:11, "high"))

vars_factor <- c("SEX", "AGE_G", "HH_H", "PARENT", "CHILD", "EDUC_F", "MARRIED", "STUDENT",
                 "LIC_VEH", "LIC_MC", "M_CAR_U" ) #"EMPLOYED", "WORKING"
vars_numeric <- c("HH_INC", "N_O_WP", "N_O_WH", "N_O_HHM", "N_O_EM",
                  "N_O_D", "N_O_CYC", "N_O_MC", "N_O_MP", "N_O_VEH") #, "N_O_J", "N_O_PV",  "N_O_V", "N_O_MV", "HH_INC_LN", "HH_INC_EXP"
target <- c("MN_O_PT")

df[,vars_factor] <- lapply(df[,vars_factor], factor)
df <- df %>% dplyr::select(c(all_of(target), vars_factor, vars_numeric))

return(df)
}

test_df <- regression_df(p_hh_syn, var_names, vars_factor, vars_numeric, target)
pred <- paste0(target, "_PRED")
test_df$pred <- predict(step.model, test_df)
train_df$pred <- predict(step.model, train_df)
plot_train <- train_df %>% dplyr::select(pred)
plot_train$type <- "Train"
plot_test <- test_df %>% dplyr::select(pred)
plot_test$type <- "Test"
plot_df <- rbind(plot_train, plot_test)

ggplot(plot_df, aes(x=type, y=pred, fill=type)) + 
  geom_violin(alpha = 0.5) +
  #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0.2) +
  stat_summary(fun = mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.2, size = 0.5, linetype = "dotted") +
  theme_bw() +
  labs(x="Population", y = "Predicted Mean Trips per Day", caption = "Dotted: Mean\nSolid: Median") +
  theme(text = element_text(size = 18))

trip %>%  dplyr::select(T_ORIG)

######################################################################################
#     TPM TRIP DISTRIBUTION MOBI DRIVE DATA
######################################################################################

# follow the assignment
# Add dplyr shortcuts
# install.packages("devtools")
# devtools::install_github("damien-dupre/dplyrshortcut")

zone1 <- 1:30
zone2 <- 31:60
zone3 <- c(61:70, 271:280)
zone4 <- 71:80
zone5 <- 81:110
zone6 <- 111:160
zone7 <- 161:190
zone8 <- 191:210
zone9 <- 211:260
zone10 <- 261:270
zones <- c(zone1, zone2, zone3, zone4, zone5, zone6, zone7, zone8, zone9, zone10)
trip_g <- trip_df %>% 
          dplyr::filter(CITYCODE == 1) %>% 
          dplyr::filter(T_ORIG %in% zones) %>% 
          dplyr::filter(T_DEST %in% zones)
nrow(trip_df)
nrow(trip)
nrow(trip_g)
trip_g <- trip_g %>% mutate(zone_orig = case_when(T_ORIG %in% zone1 ~ 1,
                                        T_ORIG %in% zone2 ~ 2,
                                        T_ORIG %in% zone3 ~ 3,
                                        T_ORIG %in% zone4 ~ 4,
                                        T_ORIG %in% zone5 ~ 5,
                                        T_ORIG %in% zone6 ~ 6,
                                        T_ORIG %in% zone7 ~ 7,
                                        T_ORIG %in% zone8 ~ 8,
                                        T_ORIG %in% zone9 ~ 9,
                                        T_ORIG %in% zone10 ~ 10))

trip_g <- trip_g %>% mutate(zone_dest = case_when(T_DEST %in% zone1 ~ 1,
                                        T_DEST %in% zone2 ~ 2,
                                        T_DEST %in% zone3 ~ 3,
                                        T_DEST %in% zone4 ~ 4,
                                        T_DEST %in% zone5 ~ 5,
                                        T_DEST %in% zone6 ~ 6,
                                        T_DEST %in% zone7 ~ 7,
                                        T_DEST %in% zone8 ~ 8,
                                        T_DEST %in% zone9 ~ 9,
                                        T_DEST %in% zone10 ~ 10))


zonal_trips <- trip_g %>%
  group_by(zone_orig, zone_dest) %>%
  summarise(nr_trips = n()) %>% 
  pivot_wider(names_from=zone_dest, values_from=nr_trips) %>%
  ungroup() %>%
  dplyr::select(-c(1))


zonal_tt <- trip_g %>%
  group_by(zone_orig, zone_dest) %>%
  summarise(avg_tt = mean(T_TT)) %>% 
  pivot_wider(names_from=zone_dest, values_from=avg_tt) %>%
  ungroup() %>%
  dplyr::select(-c(1))


zonal_impedance <- 1/(zonal_tt^2)
zonal_impedance <- data.matrix(zonal_impedance)

A <- zonal_trips %>%
    summarise_all(list(sum)) %>% 
    data.matrix(zonal_impedance)
A
P <- zonal_trips %>% 
    mutate(sum=rowSums(.[1:ncol(zonal_trips)])) %>% 
    dplyr::select(sum) %>% 
    data.matrix(zonal_impedance) %>% 
    t(.)

gravity_step <- function(impedance, A, P, b=1, a=1){
  len <- length(A)
  t <- matrix(nrow=len, ncol=len)
  a <- 1 / (b * A) %*% impedance  
  for (i in 1:len){
    for (j in 1:len){
      t[i, j] = a[i] * b[j] * P[i] * A[j] * impedance[i, j]
    }
  }
  b <- 1 / (a * P) %*% impedance
  
  for (i in 1:len){
    for (j in 1:len){
      t[i, j] = a[i] * b[j] * P[i] * A[j] * impedance[i, j]
    }
  }
  
  r <- list("trips"=t, "a"=a, "b"=b)
  return (r)
}

r <- gravity_step(zonal_impedance, A, P)
t <- r$trips
rowSums(t)
colSums(t)
# have a look here to see how to code loops:
# https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=255798340456&utm_targetid=aud-390929969673:dsa-473406582635&utm_loc_interest_ms=&utm_loc_physical_ms=1003297&gclid=CjwKCAiAz7TfBRAKEiwAz8fKOCIL6X9eZMrF2fpvSKODUgK4BKzNtmeqpo3LgqQJTN6LOXmz3bwlZRoCHxsQAvD_BwE


trip_df %>% dplyr::filter(karlsu_a)





