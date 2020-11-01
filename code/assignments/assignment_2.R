######################################################################################
#     TPM TRIP PRODUCTION (REGRESSION) MOBI DRIVE DATA
######################################################################################

# 00 set up ----

pkgs <- c("tidyverse", "lubridate", "broom", "dplyr", "here", "purrr", "AER", "stargazer", "leaps")
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

# see where your path goes
here()

rm(list=ls())
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


######################################################################################
#     TPM TRIP DISTRIBUTION MOBI DRIVE DATA
######################################################################################

# follow the assignment

# have a look here to see how to code loops:
# https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=255798340456&utm_targetid=aud-390929969673:dsa-473406582635&utm_loc_interest_ms=&utm_loc_physical_ms=1003297&gclid=CjwKCAiAz7TfBRAKEiwAz8fKOCIL6X9eZMrF2fpvSKODUgK4BKzNtmeqpo3LgqQJTN6LOXmz3bwlZRoCHxsQAvD_BwE








