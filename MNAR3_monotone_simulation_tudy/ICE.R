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
  
  #make monotone missing
  tmpdata$LM2 = ifelse(is.na(tmpdata$LM1), NA, tmpdata$LM2)
  tmpdata$RL2 = ifelse(is.na(tmpdata$LM2), 0, 1)
  
  ## begin outcome regression
  mydat = tmpdata[tmpdata$R==1,]
  fity1 = glm2(Y ~ A+LO+LM1+LM2 + LM1*LM2 + LM1*LO,family = binomial(), data = mydat)
  mydat = tmpdata[tmpdata$RL1==1 & tmpdata$RL2==1,]; mydat$A=1;
  mydat$pred1 = predict(fity1, newdata = mydat, type="response")
  
  fity2 = glm2(pred1 ~ LO+LM1+LO*LM1,family = binomial(), data = mydat)
  mydat = tmpdata[tmpdata$RL1==1,]
  mydat$pred0 = predict(fity2, newdata = mydat, type="response")
  
  fity3 = glm2(pred0 ~ LO,family = binomial(), data = mydat)
  mydat = tmpdata
  mydat$pred = predict(fity3, newdata = mydat, type="response")
  
  mean(mydat$pred)
  
  return(mean(mydat$pred))
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"ICE.csv")

stopCluster(cl)
