library(doParallel)
library(foreach)

# Calculate the number of cores
getDoParWorkers()                                          
detectCores()                                                      
cl=makeCluster(7)                                              
registerDoParallel(cl)                                                                
getDoParWorkers()   

#for(m in 1:sim)
myfunc = function(m)
{
  options(warn=-1)
  library(geepack);library(MASS);library(ResourceSelection);library(ltmle); library(SuperLearner); library(survival)
  library(dplyr); library(lme4); library(glm2); library(mice)
  library(data.table); library(splines)
  setDTthreads(1)
  #library(reshape2)  #do not use for data frame only
  
  logit <- function(term) {
    return( ifelse(!is.na(term),log(term/(1-term)),NA) )
  }
  
  EXPIT <- function(term) {
    return( ifelse(!is.na(term),exp(term)/(1+exp(term)),NA) )
  }
  
  source("datagen.R")
  set.seed(112789)
  seeds = floor(runif(1000)*10^8);
  set.seed(seeds[m])
  
  N <- 2500
  df <- datagen(N)
  tmpdata = na.omit(df)
  
  fitA = glm(A~LO+LM2, data=tmpdata, family = binomial(link=logit))
  tmpdata$preda = predict(fitA, newdata = tmpdata, type="response")
  tmpdata$preda = ifelse(tmpdata$A==1,tmpdata$preda, 1-tmpdata$preda)
  
  fit_glm <- glm2(Y ~ LO+LM1+LM2 + LM1*LM2 + LM1*LO, data = tmpdata[tmpdata$A==1,], weights=1/preda,family=binomial())
  tmpdat = tmpdata;
  tmpdata_pred <- predict(fit_glm, newdata=tmpdat, type="response")
  
  return(mean(tmpdata_pred))
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"cc_mis_ipw.csv")

stopCluster(cl)
