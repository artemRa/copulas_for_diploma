library(tidyverse)
library(CDVine)
library(VineCopula)
library(MASS)
library(scorecard)

# Подгрузка функций
setwd("./Script/Functions")
sapply(list.files(), source) %>% invisible()


# Искусственный датасет
# Генерация динамических данных

set.seed(91710)

N <- 5000
M <- 3
rep_cnt <- 10

X <- list()
bads <- list()
bads_cnt <- 500


# Генерация датасета
for (i in seq(rep_cnt)) {
  
  X[[i]] <- map(seq(M), ~ seq(N) + rnorm(N, sd = N/10)) %>% as.data.frame()
  colnames(X[[i]]) <- paste0('x', 1:M)
  
  X[[i]][,2] <- X[[i]][N:1, 2]
  
  sorting <- sample(1:N, N/2)
  sorting2 <- sample(1:N, N/(rep_cnt + 1 -i)^2)
  
  
  X[[i]][sort(sorting), 3] <- X[[i]][sorting, 3]
  X[[i]][sort(sorting2), 1] <- X[[i]][sorting2, 1]
  
  # Генерация bad-ов
  
  bads[[i]] <- rep(0, N)
  bads[[i]][sample(1:N, bads_cnt, prob = abs(apply(X[[i]], 1, prod))/max(X[[i]])^M)] <- 1
  
}

plot(X[[10]])



U <- pobs(as.matrix(X[[1]]))

# Поиск подходящего набора копул
estimation <- CDVineCopSelect(U, type = 2)
fam <- estimation$family
par <- estimation$par
par2 <- estimation$par2

est <- list()
AIC <- list()
BIC <- list()

for (i in seq(rep_cnt)) {
  
  U <- pobs(as.matrix(X[[i]]))
  est[[i]] <- CDVineSeqEst(U, family = fam, type = 2)
  AIC[[i]] <- CDVineAIC(U, family = fam, par = est[[i]]$par, par2 = est[[i]]$par2, type = 2)
  BIC[[i]] <- CDVineBIC(U, family = fam, par = est[[i]]$par, par2 = est[[i]]$par2, type = 2)
  
}

par_dyn  <- map_dfc(est, ~ .x$par)  %>% t() %>% as.data.frame()
par2_dyn <- map_dfc(est, ~ .x$par2) %>% t() %>% as.data.frame()
AIC_dyn <- map_dfc(AIC, ~ .x$pair.AIC) %>% t() %>% as.data.frame()
colnames(par_dyn) <- colnames(par2_dyn) <- c("C12", "C23", "C13_2")

plot(par_dyn[,2], type = 'b')

plot(AIC_dyn[,3])
