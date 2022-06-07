
library(tidyverse)

new <- read_rds("data/processed_data/regression_ready.rds")


#######lasso######
set.seed(2)
#install.packages("glmnet")
'%ni%' <- Negate('%in%')
library(glmnet)

x<-new %>% select(!c("REG_ID", "Region", "SUBJ_LIFE_SAT")) %>% as.data.frame()
y<-new$SUBJ_LIFE_SAT

#crossvalidation
crossval <-  cv.glmnet(x = as.matrix(x), y = as.matrix(y)) #alpa=1 (lasso) default option
plot(crossval)
penalty <- crossval$lambda.min #optimal lambda
penalty #minimal shrinkage
fit1<-glmnet(as.matrix(x),as.matrix(y),alpha = 1, lambda = penalty)#this lambda performs the minimal shrinkage
c.crval<-coef(fit1)


#get the list of variables that are selected after lasso estimation
inds.crval<-which(c.crval!=0)
variables.crval<-row.names(c.crval)[inds.crval]
variables.crval<-variables.crval[variables.crval %ni% '(Intercept)']

write_rds(variables.crval, "data/processed_data/lasso_variables.rds")

#merge dataset for random forest
new.sample<-new[,names(new) %in% c(variables.crval, "SUBJ_LIFE_SAT")]

# save random forest dataset
write_rds(new.sample, "data/processed_data/final_forest_dataset.rds")

#######random forest#######

library(party)
library(partykit)

#set seed to enable replication of results
set.seed(2)

## load data ----

new.sample <- read_rds("data/processed_data/final_forest_dataset.rds")

#Train the model:
#important: use the "control= unbiased (..)" option since we have different types of variables (e.g. continuous variables on different scales)

# for party package
# Rf_fit.shr<-cforest(SUBJ_LIFE_SAT~ ., data = new.sample, 
#                     controls = cforest_unbiased(mtry = 2, ntree = 5000))

# for partykit package

Rf_fit.shr<-cforest(SUBJ_LIFE_SAT~ ., data = new.sample,
                    ntree = 5000,
                    mtry = 2)

## using ranger package 
library(ranger)
Rf_fit.shr <- ranger(SUBJ_LIFE_SAT~ .,
       data = new.sample,
       num.trees = 5000,
       mtry = 2,
       importance = "permutation")

importance(Rf_fit.shr)



  v <- importance(Rf_fit.shr) 

#sort v by importance measure
v <-sort(v,decreasing=TRUE)

#plot the first 10 most imporant measures from v
par(mfrow=c(1,2))
dotchart(v[1:15][order(v[1:15])], xlab = "VarImp")
dotchart(v[16:30][order(v[1:15])], xlab = "VarImp")


plot(v)


## try a workflow as suggested here: https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/  and rewrite the script accordingly-----

# todo list: 
# *  initialise iml package
# *  ALE-plots
# *  ICE-plots


## initialise iml package, as in https://uc-r.github.io/iml-pkg

library(iml)


## add parallisation

library("future")
library("future.callr")

plan("callr", workers = 4)


# 1. create a data frame with just the features
features <- as.data.frame(new.sample) %>% select(-SUBJ_LIFE_SAT)

# 2. Create a vector with the actual responses
response <- as.numeric(as.vector(new.sample$SUBJ_LIFE_SAT))

# 3. Create custom predict function that returns the predicted values as a
#    vector (probability of purchasing in our example)
pred <- function(model, data)  {
  results <- predict(model, newdata = data, cores = 2 )
  return(results)
}

# create predictor object to pass to explainer functions

#predict(Rf_fit.shr, newdata = features)

predictor.rf <- Predictor$new(
  model = Rf_fit.shr, 
  data = features, 
  y = response, 
  predict.fun = predict,
  class = "regression"
)

plan("callr", workers = 4)
imp.rf <- FeatureImp$new(predictor.rf, loss = "mse")
plot(imp.rf)

ale <- FeatureEffects$new(predictor.rf, method = "ale")

plot(ale, scale = 0.7)


variables <- names(new.sample %>% select(-SUBJ_LIFE_SAT))
### ranger workflow -----


library(ranger)

rf2_ranger <- ranger(SUBJ_LIFE_SAT~ ., 
                     data = new.sample,
                     num.tree = 5000,
                     mtry = 2,
                     importance = "permutation")
vip::vip(rf2_ranger, bar =FALSE)

features <- as.data.frame(new.sample %>% dplyr::select(-SUBJ_LIFE_SAT))
response <- as.data.frame(new.sample %>% pull(SUBJ_LIFE_SAT))
pfun <- function(object, newdata) predict(object, data = newdata)$predictions


components <- Predictor$new(
  model = rf2_ranger,
  data = features,
  y = response,
  predict.fun = pfun
)





plan("callr", workers = 4)
imp.rf <- FeatureImp$new(components, loss = "mse")
plot(imp.rf)


# ale plots
plan("callr", workers = 4)
ale <- FeatureEffects$new(components)

plot(ale)
order(ale)



pdp_ice <- FeatureEffects$new(components, method = "pdp+ice", center.at = 0)

plot(pdp_ice)

