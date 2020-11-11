# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Thank you to Prof. Hess from Leeds!

### Clear memory
rm(list = ls())

### Load libraries
# install.packages("apollo")
# set your working directory: Do: Session -> Set Working Directory -> To Source File Location
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MNL",
  modelDescr ="Simple MNL model on mode choice SP data",
  indivID    ="ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_modeChoiceData.csv",header=TRUE)

### Use only SP data
database = subset(database,database$SP==1)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_car   = 0,
              asc_bus   = 0,
              asc_air   = 0,
              asc_rail  = 0,
              b_tt      = 0,
              b_access  = 0,
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
  V[['car']]  = asc_car  + b_tt * time_car                           + b_cost * cost_car
  V[['bus']]  = asc_bus  + b_tt * time_bus  + b_access * access_bus  + b_cost * cost_bus 
  V[['air']]  = asc_air  + b_tt * time_air  + b_access * access_air  + b_cost * cost_air   
  V[['rail']] = asc_rail + b_tt * time_rail + b_access * access_rail + b_cost * cost_rail  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, air=av_air, rail=av_rail), 
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
#---- STANDARD ERRORS OF PARAMETER TRANSFORMATIONS               ----
# ----------------------------------------------------------------- #

### Value of Travel time in minutes
apollo_deltaMethod(model,deltaMethod_settings = list(operation="ratio",
                                                     parName1="b_tt",
                                                     parName2="b_cost"))

### Value of Travel time in hours
apollo_deltaMethod(model,deltaMethod_settings = list(operation="ratio",
                                                     parName1="b_tt",
                                                     parName2="b_cost",
                                                     multPar1=60))


# ################################################################# #
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



