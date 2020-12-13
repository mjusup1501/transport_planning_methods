######################################################################################
#     TPM TRIP PRODUCTION (REGRESSION) MOBI DRIVE DATA
######################################################################################

# 00 set up ----

pkgs <- c("tidyverse", "lubridate", "broom", "dplyr", "here", "purrr", "AER", "stargazer", "leaps", "reshape", "knitr",
          "kableExtra", "magrittr")
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
library(knitr)
library(kableExtra)
library(magrittr)


# see where your path goes
here()


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

# PT travel times between zones 1, 3 and 7 are lowered by 40% due to a new LRT line
# If trip main mode by time (T_MM_B_T) is bus, LRT or heavy rail (6, 7, 8)
# trip travel time (T_TT)
cond <- (trip_g$zone_orig %in% c(1,3,7)) & (trip_g$zone_dest %in% c(1,3,7)) & (trip_g$T_MM_B_T %in% c(6,7,8))
sum(trip_g$T_TT)
trip_g <- trip_g %>% mutate(T_TT = ifelse(cond, T_TT * 0.6, T_TT))
sum(trip_g$T_TT)

zonal_trips <- trip_g %>%
  group_by(zone_orig, zone_dest) %>%
  summarise(nr_trips = n()) %>% 
  pivot_wider(names_from=zone_dest, values_from=nr_trips) %>%
  ungroup() %>%
  dplyr::select(-c(1)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  data.matrix()

zonal_tt <- trip_g %>%
  group_by(zone_orig, zone_dest) %>%
  summarise(avg_tt = mean(T_TT)) %>% 
  pivot_wider(names_from=zone_dest, values_from=avg_tt) %>%
  ungroup() %>%
  dplyr::select(-c(1)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  data.matrix()

zonal_impedance <- 1/(zonal_tt^2)

A <- colSums(zonal_trips)
P <- rowSums(zonal_trips)

# iterative proportional fitting IPF
ipf <- function(zones,outgoing,incoming,gcosts){
  
  # number of zones
  n <- length(zones)
  
  # vectors containing sum of rows (origins) and columns (destinations)
  orig <- outgoing
  dest <- incoming
  dims <- c("City Centre","South Town","North Town","East Town","West-Suburb","South-Suburb","North-Suburb",
            "East-Suburb","Wettersbach","Neureuth")
  
  # balancing factors vectors, checking alpha_d (one could also do it the other way, by checking alpha_o)
  alpha_o_new <- rep(1,n)
  alpha_d <- rep(1,n)
  alpha_d_new <- rep(1,n)
  
  # define the minimal error for the balancing factors
  error <- 0.000001
  
  # iteration
  iteration <- 1L
  
  # matrix with flows
  trips <- matrix(NA_real_,n+1,n+1,dimnames = list(c(dims,"Column sum"),c(dims,"Row sum")))
  
  repeat {
    for (i in 1:n) {
      alpha_o_new[i] <- 1/(sum(alpha_d*dest*gcosts[i,]))
      alpha_d_new[i] <- 1/(sum(alpha_o_new*orig*gcosts[,i]))
    }
    if(sum(abs(alpha_d_new-alpha_d)/alpha_d) < error){break}
    iteration = iteration + 1
    alpha_d <- alpha_d_new
  }
  
  # Compute the flows
  for (i in zones) {
    for (j in zones) {
      trips[i,j] <- alpha_o_new[i]*orig[i]*alpha_d_new[j]*dest[j]*gcosts[i,j]
    }
  }
  trips[1:n,n+1] <- rowSums(trips[1:n,1:n])
  trips[n+1,1:n] <- colSums(trips[1:n,1:n])
  trips[n+1,n+1] <- sum(trips[1:n,n+1])
  
  return(list("iteration"=iteration, "trips"=trips, "a"=alpha_o_new, "b"=alpha_d_new))
}


# It's a lot simpler to use IPF function which yields the same results in a more elegant manner
r <- ipf(1:10, P, A, zonal_impedance)

dims <- c("City Centre","South Town","North Town","East Town","West-Suburb","South-Suburb","North-Suburb",
          "East-Suburb","Wettersbach","Neureuth")
impedance <- matrix(zonal_impedance, 10, 10,dimnames = list(dims,dims))
impedance %>%
  kable(format = 'latex', booktabs = TRUE) %>%
  add_header_above(header = c("Text" = 2, "Values" = 2))

n <- 10
trips <- matrix(0, 11, 11, dimnames = list(c(dims,"Column sum"),c(dims,"Row sum")))
trips[1:n, 1:n] <- zonal_trips
trips[1:n,n+1] <- rowSums(trips[1:n,1:n])
trips[n+1,1:n] <- colSums(trips[1:n,1:n])
trips[n+1,n+1] <- sum(trips[1:n,n+1])
trips %>%
  kable(format = 'latex', booktabs = TRUE) %>%
  add_header_above(header = c("Text" = 2, "Values" = 2))

r$trips
r$trips %>%
  kable(format = 'latex', booktabs = TRUE) %>%
  add_header_above(header = c("Text" = 2, "Values" = 2))
# have a look here to see how to code loops:
# https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=255798340456&utm_targetid=aud-390929969673:dsa-473406582635&utm_loc_interest_ms=&utm_loc_physical_ms=1003297&gclid=CjwKCAiAz7TfBRAKEiwAz8fKOCIL6X9eZMrF2fpvSKODUgK4BKzNtmeqpo3LgqQJTN6LOXmz3bwlZRoCHxsQAvD_BwE

