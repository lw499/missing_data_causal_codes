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
  mydat = tmpdata[tmpdata$R==1,]
  fity1o = glm2(Y ~ A+LO+I(LM1*LM2),family = binomial(), data = mydat)
  mydat = tmpdata[tmpdata$A==1 & tmpdata$R==1,]
  mydat$predo = predict(fity1o, newdata = mydat)
  fity1 = glm2(Y ~ 1, offset = predo, weight = 1/(preda*predr), family = binomial(link=logit), data = mydat)
  mydat = tmpdata[tmpdata$R==1,]; mydat$A=1;
  mydat$predo = predict(fity1o, newdata = mydat)
  mydat$pred1 = predict(fity1, newdata = mydat, type="response")
  
  fity2o = glm2(pred1 ~ LO, data = mydat, family = quasibinomial(link=logit))
  mydat$predo = predict(fity2o, newdata = mydat)
  fity2 = glm2(pred1 ~ 1, offset = predo, weight = 1/(predr), family = quasibinomial(link=logit), data = mydat)
  mydat = tmpdata
  mydat$predo = predict(fity2o, newdata = mydat)
  mydat$pred0 = predict(fity2, newdata = mydat, type="response")
  mean(mydat$pred0)
  
  return(mean(mydat$pred0))
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"tmle_mis_out.csv")

stopCluster(cl)
