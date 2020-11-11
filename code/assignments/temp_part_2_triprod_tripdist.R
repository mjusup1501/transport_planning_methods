######################################################################################
#     TPM TRIP PRODUCTION (REGRESSION) MOBI DRIVE DATA
######################################################################################

# 00 set up ----

# install packages
install.packages(tidyverse)
install.packages(lubridate)
install.packages(here)
install.packages(AER)
install.packages("stargazer")

# load packages
library(tidyverse) # see cheatsheet for dplyr and ggplot2
library(lubridate) # see cheatsheet for lubridate
library(here)
library(AER) # used for regression
library(stargazer) # nice latex summary output

# Clear workspace aka environment: be careful with this ;-)
rm(list = ls())

# see where your path goes
here::here()

# load data if necessary
# load(here::here("data","Mobidrive_2002.RData"))

# Check function here::here() to see how you can define the path to the file location
# e.g. load(here::here("data","Mobidrive_2002.RData")) if you have a folder called "data" in your project directory


# 0 create variables ----




# 1 fit a model ----
model1 <- lm(MN_O_PT ~ AGE + factor(EDUC), data = p_hh)

model2 <- lm(MN_O_PT ~ AGE + log(HH_INC), data = p_hh)


#basic stuff
summary(model1) # get a summary output
summary(model2)

model1$na.action # see if there were NA values -> missingness
names(model1) # see what you can access in the model
coef(model1) # extract the coefficients
confint(model1) # get the confidence interval for your parameters


# 2 test assumptions ----
# basic infos, but many insights
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(model1)
par(mfrow=c(1,1)) # Change back to 1 x 1


# heteroskedasticity: what is H0 and what is HA?
bptest(model1)


# (multi-) collinearity
vif(model1)

# latex output
stargazer(model1) # copy output directly to overleaf


######################################################################################
#     TPM TRIP DISTRIBUTION MOBI DRIVE DATA
######################################################################################

# follow the assignment

# have a look here to see how to code loops:
# https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=255798340456&utm_targetid=aud-390929969673:dsa-473406582635&utm_loc_interest_ms=&utm_loc_physical_ms=1003297&gclid=CjwKCAiAz7TfBRAKEiwAz8fKOCIL6X9eZMrF2fpvSKODUgK4BKzNtmeqpo3LgqQJTN6LOXmz3bwlZRoCHxsQAvD_BwE








