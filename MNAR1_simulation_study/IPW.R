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
  
  ### Models for A and R
  Afit = glm(A~LO+LM1+LM2+LO*LM1, data=tmpdata[tmpdata$R==1,], family = binomial(link=logit))
  tmpdata$preda = predict(Afit, newdata = tmpdata, type="response")
  tmpdata$preda = ifelse(tmpdata$A==1,tmpdata$preda, 1-tmpdata$preda)
  
  Rfit = glm(R~LO, family = binomial(link=logit), data=tmpdata)
  tmpdata$predr = predict(Rfit, newdata = tmpdata, type="response")
  
  ## begin outcome regression
  mydat = tmpdata[tmpdata$A==1 & tmpdata$R==1,]
  fity1 = glm2(Y ~ 1, weight = 1/(preda*predr), family = binomial(link=logit), data = mydat)
  mydat = tmpdata
  mydat$pred0 = predict(fity1, newdata = mydat, type="response")
  mean(mydat$pred0)
  
  return(mean(mydat$pred0))
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"IPW.csv")

stopCluster(cl)
