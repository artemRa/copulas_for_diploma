library(tidyverse)
library(CDVine)
library(VineCopula)

N <- 1000
M <- 10

x1 <- map(seq(M), ~ rnorm(N, .x))

map_dbl(x1, mean)

d <- 3
dd <- d*(d-1)/2
fam1 <- rep(1,dd)
par1 <- c(0.5,-0.8,-0.5)

U <- CDVineSim(N, family = fam1, par = par1, type = 2)
hist(U[,3])
plot(as_tibble(U))

test <- cbind(rnorm(N,0,5), rnorm(N,0,10))
plot(test)

plot(pobs(as.matrix(test))[,1], test[,1])

hist(m[,2])
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)

rho.1      df 
0.43563 3.84453 
It is nice to see that the parameters of the fitted copula are the same as those suggested by the BiCopSelect() function. Let’s take a look at the density of the copula we have just estimated

rho <- coef(fit)[1]
df <- coef(fit)[2]
persp(tCopula(dim=2,rho,df=df),dCopula)