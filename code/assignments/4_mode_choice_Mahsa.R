
### Clear memory
rm(list = ls())

#install.packages("dplyr")
install.packages("apollo")
install.packages("here")
library(tidyverse)
library(dplyr)
library(here)


load(here::here("data","Mobidrive_2002.RData"))

#filter NA
#trip_df_filtered <- trip_df %>% filter(!(is.na(BIKETIME) & is.na(WALKTIME)& is.na(MOTIVTT) & is.na(TOTALTT)))

colnames(trip_df)
str(trip_df)

attr(trip_df, "variable.labels") <- NULL

trip <- trip_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)
person <- person_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)
hh <- household_df %>% filter(CITYCODE == 1 & STUDYCOD == 2)

database <- trip %>%
  select(HH_NR, P_NR, T_NR, PETROCOS, CARVARCO, PARKCOST, T_MM_B_T, TOTALTT, LRT_TT, MOTIVTT, WALKTIME, BIKETIME, T_EXPCOR, T_WT, T_TT, T_CTT, T_DIST) %>%
  left_join(hh %>% select(HH_NR, N_O_CYC, N_O_PV), by = c("HH_NR")) %>%
  left_join(person %>% select(HH_NR, P_NR, SEASON_L, LICENCE), by = c("HH_NR","P_NR")) %>%
  filter(!is.na(TOTALTT) | !is.na(MOTIVTT) | !is.na(WALKTIME) | !is.na(BIKETIME)) %>%
  mutate(across(c(TOTALTT, LRT_TT, MOTIVTT, WALKTIME, BIKETIME), .fns = ~ .x/60)) %>%
  mutate(choice_name = case_when(T_MM_B_T == 1 ~ "walking",
                                 T_MM_B_T == 2 ~ "cycling",
                                 T_MM_B_T == 4 ~ "car",
                                 T_MM_B_T == 5 ~ "car",
                                 T_MM_B_T == 6 ~ "pt",
                                 T_MM_B_T == 7 ~ "pt",
                                 T_MM_B_T == 8 ~ "pt",
                                 TRUE ~ NA_character_),
         choice = case_when(choice_name == "walking" ~ 1,
                            choice_name == "cycling" ~ 2,
                            choice_name == "car" ~ 3,
                            choice_name == "pt" ~ 4),
         WALKTIME = if_else(is.na(WALKTIME), 0, WALKTIME),
         BIKETIME = if_else(is.na(BIKETIME), 0, BIKETIME), 
         time_car = MOTIVTT,
         time_walk = WALKTIME,
         time_bike = BIKETIME,
         time_pt = TOTALTT,
         PARKCOST = if_else(is.na(PARKCOST), 0, PARKCOST),
         CARVARCO = if_else(is.na(CARVARCO), 0, CARVARCO),
         PETROCOS = if_else(is.na(PETROCOS), 0, PETROCOS),
         cost_pt = if_else(time_pt == 0, 0, 0.35 * T_DIST),
         cost_car = if_else(T_MM_B_T == 5 | time_car == 0, 0, CARVARCO + PETROCOS + PARKCOST),
         av_walk = if_else(time_walk > 0, 1, 0),
         #av_bike = 1,
         av_bike = if_else(N_O_CYC > 0 & time_bike > 0, 1, 0),
         av_car = if_else(N_O_PV > 0 & LICENCE > 0 & time_car > 0, 1, 0),
         #av_pt = 1,
         #av_pt = if_else(SEASON_L > 0, 1, 0),
         av_pt = if_else(time_pt > 0, 1, 0),
         ID = as.numeric(factor(paste0(HH_NR,"_",P_NR)))) %>%
  arrange(ID) %>%
  # filter for choice 1 to 4
  filter(between(choice, 1, 4)) %>%
  # filter for rows where availability = 1 if chosen
  filter(choice == 1 & av_walk == 1 | choice == 2 & av_bike == 1 | choice == 3 & av_car == 1 | choice == 4 & av_pt == 1) %>%
  select(ID, choice, choice_name, everything(), -c(HH_NR, P_NR, T_NR)) %>%
  as.data.frame()


summary(database$MOTIVTT)
summary(database$PETROCOS)
summary(database$choice)


test <- database %>%
  filter(choice == 3, time_car == 0)

test <- database %>%
  filter(TOTALTT == 0 & MOTIVTT == 0)



### Load libraries
# install.packages("apollo")
# set your working directory: Do: Session -> Set Working Directory -> To Source File Location
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MNL",
  modelDescr ="Simple MNL model",
  indivID    ="ID"
)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car   = 0,
              asc_bike   = 0,
              asc_walk   = 0,
              asc_pt  = 0,
              b_tt_walk  = 0,
              b_tt_bike = 0,
              b_tt_car = 0,
              b_tt_pt = 0,
              b_cost    = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_car")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['car']]  =  asc_car  +  b_tt_car * time_car  +   b_cost * cost_car
  V[['bike']]  = asc_bike  + b_tt_bike * time_bike
  V[['walk']]  = asc_walk  + b_tt_walk * time_walk   
  V[['pt']] =    asc_pt +    b_tt_pt * time_pt +      b_cost * cost_pt  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, bike=2, car=3, pt=4), 
    avail         = list(car=av_car, bike=av_bike, walk=av_walk, pt=av_pt), 
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

# ################################################################# #
##### ADDITIONAL RESULTS ANALYSIS AND DIAGNOSTICS                ####
# ################################################################# #

### Print the outputs of additional diagnostics to file (comment out sink to file command below if not desired)
### Remember to run the line closing any open sinks at the end of this file
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- LIKELIHOOD RATIO TEST AGAINST BASE MODEL                   ----
# ----------------------------------------------------------------- #

#apollo_lrTest("MNL",model) cannot have the same name

# ----------------------------------------------------------------- #
#---- STANDARD ERRORS OF PARAMETER TRANSFORMATIONS               ----
# ----------------------------------------------------------------- #

### Value of Travel time for car in minutes
apollo_deltaMethod(model,deltaMethod_settings = list(operation="ratio",
                                                     parName1="b_tt_car",
                                                     parName2="b_cost"))

### Value of Travel time for car in hours
apollo_deltaMethod(model,deltaMethod_settings = list(operation="ratio",
                                                     parName1="b_tt_car",
                                                     parName2="b_cost",
                                                     multPar1=60))
### Value of Travel time in minutes
apollo_deltaMethod(model,deltaMethod_settings = list(operation="ratio",
                                                     parName1="b_tt_pt",
                                                     parName2="b_cost"))

### Value of Travel time in hours
apollo_deltaMethod(model,deltaMethod_settings = list(operation="ratio",
                                                     parName1="b_tt_pt",
                                                     parName2="b_cost",
                                                     multPar1=60))

################################################################# #
##### POST-ANALYSIS                                         ####
# ################################################################# #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs,
                                     prediction_settings=list())

### Look at a summary of the predicted choice probabilities
summary(predictions_base)

### Now imagine the cost for rail increases by 10%
attribute_increase <- 1.1
database$cost_rail = attribute_increase*database$cost_rail

### Rerun apollo validate inputs
apollo_inputs = apollo_validateInputs()

### Rerun predictions with the new data, and save into a separate matrix
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs,
                                    prediction_settings=list())

### Look at a summary of the predicted choice probabilities
summary(predictions_new)

### Return to original data
database$cost_rail = (1/attribute_increase)*database$cost_rail

### Compute own elasticity for rail (approximation):
log(sum(predictions_new[,6],na.rm=TRUE)/sum(predictions_base[,6],na.rm=TRUE))/log(attribute_increase)

### Cross-elasticities for other modes
### car:
log(sum(predictions_new[,3],na.rm=TRUE)/sum(predictions_base[,3],na.rm=TRUE))/log(attribute_increase)
### bus:
log(sum(predictions_new[,4],na.rm=TRUE)/sum(predictions_base[,4],na.rm=TRUE))/log(attribute_increase)
### air:
log(sum(predictions_new[,5],na.rm=TRUE)/sum(predictions_base[,5],na.rm=TRUE))/log(attribute_increase)

# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
if(sink.number()>0) sink()





