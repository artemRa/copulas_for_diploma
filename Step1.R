library(tidyverse)
library(CDVine)
library(VineCopula)
library(MASS)

library(scorecard)
# library("devtools")
# install_github("davpinto/fastknn")
# 
# library(fastknn)

# Feature engineering packages
# library(caret)    # for various ML tasks
# library(recipes)  # for feature engineering tasks
# 
# library(woeBinning)

N <- 5000
M <- 3

# Генерация датасета
X <- map(seq(M), ~ seq(N) + rnorm(N, sd = N/10)) %>% as.data.frame()
colnames(X) <- paste0('x', 1:M)

X[,2] <- X[N:1, 2]
sorting <- sample(1:N, N/2)
X[sort(sorting), 3] <- X[sorting, 3]

bads_cnt <- 500
bads <- rep(0, N)
bads[sample(1:N, bads_cnt, prob = abs(apply(X, 1, prod))/max(X)^M)] <- 1

# График сгенерированные данные
plot(X, col = alpha(bads + 1, 0.2 + bads * 0.5), pch = bads + 1)

# Бининг + риск кривые
bins <- woebin(data.frame(X, bads = as.factor(bads)), y = "bads", x = colnames(X), method = "tree", bin_num_limit = 5)
woebin_plot(bins)



# Число разрезов плоскости + минимальный статистически значимый размер группы
m <- 5
significant_size <- 50

breaks <- 
  map(X, ~ {
      pre <- quantile(.x, probs = seq(0, 1, 1/m))
      pre[1] <- -Inf
      pre[m + 1] <- Inf
      pre
    })

raw_xtr <-   
  map2(X, breaks, ~ cut(.x, .y, labels = seq(m))) %>% 
  as_tibble() %>% 
  mutate_if(is.factor, as.integer) %>%
  mutate(bads = bads)
  
xtr <- 
  raw_xtr %>% 
  group_by(x1, x2, x3) %>%
  summarise(cnt = n(), bads = sum(bads)) %>% 
  ungroup() %>% 
  arrange(desc(cnt))

xtr <- 
  expand.grid(x1 = 1:m, x2 = 1:m, x3 = 1:m) %>% 
  left_join(xtr, by = colnames(X)) %>% 
  mutate_all(~ ifelse(is.na(.x), 0, .x))

xtr$group <- 1L
xtr$cnt2 <- as.integer(NA)
xtr$bads2 <- as.integer(NA)
xtr$group2 <- as.integer(NA)


N2 <- NROW(xtr)

exp_grid <- expand.grid(1:N2, 1:N2)
exp_grid <- exp_grid[which(exp_grid$Var1 - exp_grid$Var2 != 0),]
dist <- exp_grid %>% apply(1, function(x) dist(rbind(xtr[x[1],1:3], xtr[x[2],1:3])))


for (i in seq(N2)) {
  
  k <- 1
  
  xtr$cnt2[i] <- xtr$cnt[i]
  xtr$bads2[i] <- xtr$bads[i]
  xtr$group2[i] <- xtr$group[i]
  
  if (xtr$cnt[i] < significant_size) {
    
    neigbors <- which(exp_grid$Var1 == i)
    nearest <- neigbors[order(dist[neigbors])]
    
    while (xtr$cnt2[i] < significant_size) {
      
      j <- exp_grid[nearest,]$Var2[k]
      
      xtr$cnt2[i] <- xtr$cnt2[i] + xtr$cnt[j]
      xtr$bads2[i] <- xtr$bads2[i] + xtr$bads[j]
      xtr$group2[i] <- xtr$group2[i] + xtr$group[j]
      
      k<- k+1
      
    }
    
    rm(j)
  }
  
  rm(k)
  
}


# График структуры мета-риска

xtr %>% 
  mutate(risk = bads2/cnt2) %>% 
  ggplot() +
  geom_raster(aes(x = x1, y = x2, fill = risk, alpha = 1/group2^0.01)) +
  facet_grid(cols = vars(x3)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_jitter(aes(x = x1, y = x2, color = as.factor(bads)), raw_xtr, alpha = 0.3) +
  geom_label(aes(x = x1, y = x2, label = round(risk, 2)), alpha = 0.5, color = NA) + 
  geom_text(aes(x = x1, y = x2, label = round(risk, 2))) +
  facet_grid(cols = vars(x3), labeller = label_both) +
  scale_color_manual(breaks = c(0, 1), values=c("black", "red")) +
  labs(color = 'bads', fill = "risk level") +
  scale_alpha(guide = 'none')


# График числа объединённых ячеек

xtr %>% 
  ggplot() +
  geom_raster(aes(x = x1, y = x2, alpha = 1/group2^0.01)) +
  geom_text(aes(x = x1, y = x2, label = group2), color = 'red') +
  facet_grid(cols = vars(x3), labeller = label_both) +
  scale_alpha(guide = 'none')

# Сравнение
xtr %>% summarise(risk = sum(bads)/sum(cnt), risk2 = sum(bads2)/sum(cnt2))
xtr %>% mutate(bads3 = cnt * bads2/cnt2) %>% summarise(risk3 = sum(bads3)/sum(cnt))



U <- pobs(as.matrix(X))

# Calculate kernel density estimate
# from MASS package
bivn_kde12 <- kde2d(U[,1], U[,2], n = 50)   
bivn_kde13 <- kde2d(U[,1], U[,3], n = 50)  
bivn_kde23 <- kde2d(U[,2], U[,3], n = 50)  


# Распределения
par(mfrow = c(1,3))

image(bivn_kde12, xlab = 'u1', ylab = 'u2')
contour(bivn_kde12, add = TRUE)

image(bivn_kde13, xlab = 'u1', ylab = 'u3')
contour(bivn_kde13, add = TRUE)

image(bivn_kde23, xlab = 'u2', ylab = 'u3')
contour(bivn_kde23, add = TRUE)

# Трансформация
par(mfrow = c(1,3))

plot(U[,1], X[,1], xlab = 'u1', ylab = 'x1')
plot(U[,2], X[,2], xlab = 'u2', ylab = 'x2')
plot(U[,3], X[,3], xlab = 'u2', ylab = 'x3')

par(mfrow = c(1,1))



# Поиск подходящего набора копул
estimation <- CDVineCopSelect(U, type = 2)

fam <- estimation$family
par <- estimation$par
par2 <- estimation$par2

CDVineAIC(U, family = fam, par = par, par2 = par2, type = 2)
CDVineBIC(U, family = fam, par = par, par2 = par2, type = 2)

# extra_estimation <- CDVineMLE(U, family = estimation$family, type = 2)
# sequential_estimation <- CDVineSeqEst(U, family = first_estimation$family, type = 2)

c12 <- BiCopPDF(U[,1], U[,2], family = fam[1], par = par[1], par2 = par2[1])
c23 <- BiCopPDF(U[,2], U[,3], family = fam[2], par = par[2], par2 = par2[2])

F1_2 <- BiCopHfunc2(U[,1], U[,2], BiCop(family = fam[1], par = par[1], par2 = par2[1]))
F3_2 <- BiCopHfunc2(U[,3], U[,2], BiCop(family = fam[2], par = par[2], par2 = par2[2]))

c13_2 <- BiCopPDF(F1_2, F3_2, family = fam[3], par = par[3], par2 = par2[3])

f <- c12*c23*c13_2


plot(BiCop(family = fam[1], par = par[1], par2 = par2[1]), xlab = 'F(u1)', ylab = 'F(u2)', main = 'C12')
plot(BiCop(family = fam[2], par = par[2], par2 = par2[2]), xlab = 'F(u2)', ylab = 'F(u3)', main = 'C23')
plot(BiCop(family = fam[3], par = par[3], par2 = par2[3]), xlab = 'F(u1|u2)', ylab = 'F(u3|u2)', main = 'C13|2')

contour(BiCop(family = fam[1], par = par[1], par2 = par2[1]), xlab = 'F(u1)', ylab = 'F(u2)', main = 'C12')
contour(BiCop(family = fam[2], par = par[2], par2 = par2[2]), xlab = 'F(u2)', ylab = 'F(u3)', main = 'C23')
contour(BiCop(family = fam[3], par = par[3], par2 = par2[3]), xlab = 'F(u1|u2)', ylab = 'F(u3|u2)', main = 'C13|2')



U2 <- CDVineSim(N,family = fam, par = par, par2 = par2, type = 2)

reverse_data <- 
  map(1:M, ~ approxfun(U[,.x], X[,.x])(U2[,.x])) %>% 
  as.data.frame()

colnames(reverse_data) <- colnames(X)

plot(reverse_data)

merge_xtr <- xtr %>% mutate(risk = bads2/cnt2) %>% dplyr::select(colnames(X), risk)

raw_xtr2 <-   
  map2(reverse_data, breaks, ~ cut(.x, .y, labels = seq(m))) %>% 
  as_tibble() %>% 
  mutate_if(is.factor, as.integer)

xtr2 <- 
  raw_xtr2 %>% 
  group_by(x1, x2, x3) %>%
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  left_join(merge_xtr, by = colnames(X))

xtr2 %>% summarise(risk = sum(risk*cnt, na.rm = T)/sum(cnt, na.rm = T))

plot(reverse_data, col = alpha(1, 0.2))




plot(U2)

# calculate the log-likelihood
logLik1 = CDVineLogLik(U, fam,  type=2)
BiCopHfunc(Data[,1], Data[,2], BiCop(family = 1, par = 0.2))



  
xtr %>% print(n = 50)

hist(X$x3)


x <- map(seq(M), ~ rnorm(N, .x))
map_dbl(x1, mean)


# Simulate bivariate normal data
mu <- c(0,0,0)                         # Mean
Sigma <- matrix(c(1, .5, -.5, -.5, 1, .5, -.5, .5, 1), 3)  # Covariance matrix
# > Sigma
# [,1] [,2]
# [1,]  1.0  0.1
# [2,]  0.1  1.0

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(N, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)                                      
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package

image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package


as.data.frame(bivn)[,1:2] %>% plot()
as.data.frame(bivn)[replace_set,1:2] %>% lines(col = 'red', type = 'p')

prob <- 0.2
replace_set <- sample(seq(N), size = prob * N)
bivn[,1][replace_set] <- bivn[,1][replace_set] + 2



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