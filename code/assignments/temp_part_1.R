######################################################################################
#                       TPM DESCRIPTIVE ANALYSIS MOBI DRIVE DATA
######################################################################################

# 1 set up ----

# install packages
install.packages(tidyverse)
install.packages(lubridate)
install.packages(here)

# load packages
library(tidyverse) # see cheatsheet for dplyr and ggplot2
library(lubridate) # see cheatsheet for lubridate
library(here)

# Clear workspace aka environment: be careful with this ;-)
rm(list = ls())

# see where your path goes
here::here()

# load data
load(here::here("Mobidrive_2002.RData"))

# Check function here::here() to see how you can define the path to the file location
# e.g. load(here::here("data","Mobidrive_2002.RData")) if you have a folder called "data" in your project directory

#HINTS:
# subset household_df, person_df, trip_df to only focus on data for Karlsruhe and Mainstudy (check CITYCODE and STUDYCOD)
# think about merging dataframes -> check cheatsheet dplyr
# think about a unique ID -> check the code book Schönfelder p.7


# 2.1 socio-economic characteristics ----






# 2.2 travel behaviour ----













