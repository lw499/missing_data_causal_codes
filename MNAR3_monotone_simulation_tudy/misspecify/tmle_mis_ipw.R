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
  tmpdata = df
  tmpdata$LM2 = ifelse(is.na(tmpdata$LM1), NA, tmpdata$LM2)
  tmpdata$RL2 = ifelse(is.na(tmpdata$LM2), 0, 1)
  
  ### Models for A and R
  Afit = glm(A~LO+LM2, data=tmpdata[tmpdata$R==1,], family = binomial(link=logit))
  tmpdata$preda = predict(Afit, newdata = tmpdata, type="response")
  tmpdata$preda = ifelse(tmpdata$A==1,tmpdata$preda, 1-tmpdata$preda)
  
  RAfit = glm(RA~LO*LM1*LM2, family = binomial(link=logit), data=tmpdata[tmpdata$RL1==1 & tmpdata$RL2==1,])
  tmpdata$predra = predict(RAfit, newdata = tmpdata, type="response")
  
  RLfit2 = glm(RL2~LO+LM1+LO*LM1, family = binomial(link=logit), data=tmpdata[tmpdata$RL1==1,])
  tmpdata$predrl2 = predict(RLfit2, newdata = tmpdata, type="response")
  
  RLfit1 = glm(RL1~LO, family = binomial(link=logit), data=tmpdata)
  tmpdata$predrl1 = predict(RLfit1, newdata = tmpdata, type="response")
  
  ## begin outcome regression
  mydat = tmpdata[tmpdata$R==1,]
  fity1o = glm2(Y ~ A+LO+LM1+LM2 + LM1*LM2 + LM1*LO,family = binomial(), data = mydat)
  mydat = tmpdata[tmpdata$A==1 & tmpdata$R==1,]
  mydat$predo = predict(fity1o, newdata = mydat)
  fity1 = glm2(Y ~ 1, offset = predo, weight = 1/(preda*predra*predrl1*predrl2), family = binomial(link=logit), data = mydat)
  mydat = tmpdata[tmpdata$RL1==1 & tmpdata$RL2==1,]; mydat$A=1;
  mydat$predo = predict(fity1o, newdata = mydat)
  mydat$pred1 = predict(fity1, newdata = mydat, type="response")
  
  fity2o = glm2(pred1 ~ LO+LM1+LO*LM1, data = mydat, family = quasibinomial(link=logit))
  mydat$predo = predict(fity2o, newdata = mydat)
  fity2 = glm2(pred1 ~ 1, offset = predo, weight = 1/(predrl1*predrl2), family = quasibinomial(link=logit), data = mydat)
  mydat = tmpdata[tmpdata$RL1==1,]
  mydat$predo = predict(fity2o, newdata = mydat)
  mydat$pred0 = predict(fity2, newdata = mydat, type="response")
  
  fity3o = glm2(pred0 ~ LO, data = mydat, family = quasibinomial(link=logit))
  mydat$predo = predict(fity3o, newdata = mydat)
  fity3 = glm2(pred0 ~ 1, offset = predo, weight = 1/(predrl1), family = quasibinomial(link=logit), data = mydat)
  mydat = tmpdata
  mydat$predo = predict(fity3o, newdata = mydat)
  mydat$pred = predict(fity3, newdata = mydat, type="response")
  mean(mydat$pred)
  
  return(mean(mydat$pred))
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"tmle_mis_ipw.csv")

stopCluster(cl)
