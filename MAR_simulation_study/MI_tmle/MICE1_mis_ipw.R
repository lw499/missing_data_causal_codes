library(doParallel)
library(foreach)

# Calculate the number of cores
getDoParWorkers()                                          
detectCores()                                                      
cl=makeCluster(30)                                              
registerDoParallel(cl)                                                                
getDoParWorkers()   

#for(m in 1:sim)
myfunc = function(m)
{
  options(warn=-1)
  library(geepack);library(MASS);library(ResourceSelection);library(ltmle); library(SuperLearner); library(survival)
  library(dplyr); library(glm2); library(mice)
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
  
  ### Impute missing A using MICE (1. imputation include L1 and Y in the imputation model)
  impnum = 50
  tmpdata$LOY = tmpdata$LO*tmpdata$Y
  tmpdata$LM1LM2 = tmpdata$LM1*tmpdata$LM2
  tmpdata$LM1LO = tmpdata$LM1*tmpdata$LO
  tmpdataimp <- mice(tmpdata[,c('A','LO','LM1','LM2','Y','LOY')], m=impnum, seed=1989, printFlag=F, method='logreg')
  
  meany = NULL
  for (i in 1:impnum)
  {
    tmpdataimp_complete = cbind(tmpdata[, c('ids', 'R')], complete(tmpdataimp, i))
    
    fitA = glm(A~LO+LM2, data=tmpdataimp_complete, family = binomial(link=logit))
    tmpdataimp_complete$preda = predict(fitA, newdata = tmpdataimp_complete, type="response")
    tmpdataimp_complete$preda = ifelse(tmpdataimp_complete$A==1,tmpdataimp_complete$preda, 1-tmpdataimp_complete$preda)
    
    fit_glm <- glm2(Y ~ LO+LM1+LM2 + LM1*LM2 + LM1*LO, data = tmpdataimp_complete[tmpdataimp_complete$A==1,], weights=1/preda,family=quasibinomial())
    tmpdat = tmpdataimp_complete;
    tmpdata_pred <- predict(fit_glm, newdata=tmpdat, type="response")
    meany <- c(meany, mean(tmpdata_pred))
  }

  return(mean(meany))
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"MICE1_mis_ipw.csv")

stopCluster(cl)
