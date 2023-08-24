#Natural
set.seed(112789)
rexpit <- function(x) rbinom(n=length(x), size=1, prob=plogis(x))
N <- 100000000
U1 = rbinom(N, 1, 0.5); U2 = rnorm(N,0,1);
U4 = rbinom(N, 1, 0.5); U5 = rbinom(N, 1, 0.5)
LO = rbinom(N, 1, plogis(U1-U5))
LM1 = rbinom(N, 1, plogis(1-4*U1+U4))
LM2 = rbinom(N, 1, plogis(1+3*U1-2*U4))
A = rep(1,N)
Y = rbinom(N, 1, plogis(-1 + A - LM1 - LM2 -2*LM1*LM2 + LO - 2*LM1*LO))

mean(Y)
#0.2875733