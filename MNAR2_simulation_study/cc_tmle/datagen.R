#time fixed case:

datagen <- function(N)
{
  ids <- seq(1,N,1)
  U1 = rbinom(N, 1, 0.5); U2 = rnorm(N,0,1);
  U4 = rbinom(N, 1, 0.5); U5 = rbinom(N, 1, 0.5)
  U3 = rnorm(N,0,1)
  U6 = rnorm(N,0,1);
  LM1star = rbinom(N, 1, plogis(1-4*U1+U4))
  LM2star = rbinom(N, 1, plogis(1+3*U1-2*U4))
  
  LO = rbinom(N, 1, plogis(U1-U5))
  Astar = rbinom(N, 1, plogis(-1+LM1star-2*LM2star+LO+3*U2+U6)); 
  Y = rbinom(N, 1, plogis(-1 + Astar - LM1star - LM2star -2*LM1star*LM2star + LO - 2*LM1star*LO))
  RL = rbinom(N, 1, plogis(1+U3-U6+3*LO))
  RA = rbinom(N, 1, plogis(0.5+2*U2-U3+Astar+3*LO+LM1star-LM2star+RL)) #missing covariate indicator
  A = ifelse(RA==1, Astar, NA)#observed exposure (=A* if RA=1, NA otherwise)
  LM1 = ifelse(RL==1, LM1star, NA)#observed exposure (=LM* if RL=1, NA otherwise)
  LM2 = ifelse(RL==1, LM2star, NA)#observed exposure (=LM* if RL=1, NA otherwise)
  R = ifelse(RA+RL==2, 1, 0)
  
  temp_data = data.frame(cbind(ids, LO, LM1, LM2 ,RA,RL,R, A,Y))
  return(temp_data)
}




