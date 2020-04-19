library(tidyverse)
library(CDVine)
library(VineCopula)
library(MASS)

library(scorecard)

# Подгрузка функций
setwd("./Script/Functions")
sapply(list.files(), source) %>% invisible()


# Искусственный датасет
# Генерация данных ----
# runif(1)
set.seed(91710)

N <- 5000
M <- 3

# Генерация датасета
X <- map(seq(M), ~ seq(N) + rnorm(N, sd = N/10)) %>% as.data.frame()
colnames(X) <- paste0('x', 1:M)

X[,2] <- X[N:1, 2]
sorting <- sample(1:N, N/2)
X[sort(sorting), 3] <- X[sorting, 3]

# Генерация bad-ов
bads_cnt <- 500
bads <- rep(0, N)
bads[sample(1:N, bads_cnt, prob = abs(apply(X, 1, prod))/max(X)^M)] <- 1


# Визуализация + бининг ----
# График сгенерированные данные
plot(X, col = alpha(bads + 1, 0.2 + bads * 0.5), pch = bads + 1)

# Бининг + риск кривые
bins <- woebin(data.frame(X, bads = as.factor(bads)), y = "bads", x = colnames(X), method = "tree", bin_num_limit = 5)
woebin_plot(bins)

# Расчет корреляци
cor_table <- expand.grid(1:3, 1:3)

pearson <- cor_table %>% t() %>% 
  as.data.frame() %>% 
  map_dbl(~ cor.test(X[,.x[1]], X[,.x[2]], method=c("pearson"))$estimate)

kendall <- cor_table %>% t() %>% 
  as.data.frame() %>% 
  map_dbl(~ cor.test(X[,.x[1]], X[,.x[2]], method=c("kendall"))$estimate)

spearman <- cor_table %>% t() %>% 
  as.data.frame() %>% 
  map_dbl(~ cor.test(X[,.x[1]], X[,.x[2]], method=c("spearman"))$estimate)

cbind(cor_table, pearson, kendall, spearman)


# Подготовка данных ----
m <- 5 # Число разрезов плоскости
significant_size <- 50 # Минимальный статистически значимый размер группы


# Подготовка таблицы
breaks <- make_breaks(X, m)
raw_xtr <- split_it(X, breaks) %>% mutate(bads = bads)
xtr <- make_risk_hypercube(raw_xtr, m)


# Графики ----
# График структуры мета-риска
xtr %>% 
  mutate(risk = bads2/cnt2) %>% 
  ggplot() +
  geom_raster(aes(x = x1, y = x2, fill = risk, alpha = 1/group^0.01)) +
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
  geom_raster(aes(x = x1, y = x2, alpha = 1/group^0.01)) +
  geom_text(aes(x = x1, y = x2, label = round(1/group, 2)), color = 'red') +
  facet_grid(cols = vars(x3), labeller = label_both) +
  scale_alpha(guide = 'none')

# Сравнение

random_order <- 1:N
randow_xtr_raw <- split_it(X[random_order,], breaks)

randow_xtr_raw %>% 
  mutate(
    random_group = sort(rep(1:10, N/10)),
    bads = bads[random_order]
  ) %>% 
  group_by(.dots = c(colnames(X), "random_group")) %>% 
  summarise(cnt = n(), bads = sum(bads)) %>% 
  ungroup() %>% 
  left_join(xtr[, c(colnames(X), "bads2","cnt2", "group")], by = colnames(X)) %>% 
  group_by(random_group) %>% 
  mutate(risk = bads2/cnt2) %>% 
  mutate(bads3 = risk * cnt) %>% 
  mutate(cl = cnt * 1/group) %>% 
  summarise(
    fact_risk = sum(bads) / sum(cnt),
    predict_risk = sum(bads3) / sum(cnt),
    cl = sum(cl)/sum(cnt)
  ) %>% 
  ggplot(aes(x = random_group, size = cl)) +
  geom_line(aes(y = fact_risk, color = "fact risk")) +
  geom_line(aes(y = predict_risk, color = "predict risk")) +
  labs(x = "group", y = "risk level", color = "risk estimation", size = "CL") +
  scale_x_continuous(breaks = 1:10, labels = 1:10)

# Создание копулы ----

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

# c12 <- BiCopPDF(U[,1], U[,2], family = fam[1], par = par[1], par2 = par2[1])
# c23 <- BiCopPDF(U[,2], U[,3], family = fam[2], par = par[2], par2 = par2[2])
# 
# F1_2 <- BiCopHfunc2(U[,1], U[,2], BiCop(family = fam[1], par = par[1], par2 = par2[1]))
# F3_2 <- BiCopHfunc2(U[,3], U[,2], BiCop(family = fam[2], par = par[2], par2 = par2[2]))
# 
# c13_2 <- BiCopPDF(F1_2, F3_2, family = fam[3], par = par[3], par2 = par2[3])
# 
# f <- c12*c23*c13_2


plot(BiCop(family = fam[1], par = par[1], par2 = par2[1]), xlab = 'F(u1)', ylab = 'F(u2)', main = 'C12')
plot(BiCop(family = fam[2], par = par[2], par2 = par2[2]), xlab = 'F(u2)', ylab = 'F(u3)', main = 'C23')
plot(BiCop(family = fam[3], par = par[3], par2 = par2[3]), xlab = 'F(u1|u2)', ylab = 'F(u3|u2)', main = 'C13|2')

contour(BiCop(family = fam[1], par = par[1], par2 = par2[1]), xlab = 'F(u1)', ylab = 'F(u2)', main = 'C12')
contour(BiCop(family = fam[2], par = par[2], par2 = par2[2]), xlab = 'F(u2)', ylab = 'F(u3)', main = 'C23')
contour(BiCop(family = fam[3], par = par[3], par2 = par2[3]), xlab = 'F(u1|u2)', ylab = 'F(u3|u2)', main = 'C13|2')


# Создание новой копулы
U2 <- CDVineSim(N,family = fam, par = par, par2 = par2, type = 2)
M <- NCOL(X)

reverse_data <- 
  map(1:M, ~ approxfun(U[,.x], X[,.x])(U2[,.x])) %>% 
  as.data.frame()

colnames(reverse_data) <- colnames(X)

plot(reverse_data, col = alpha(1, 0.2))



merge_xtr <- xtr %>% mutate(risk = bads2/cnt2) %>% dplyr::select(colnames(X), risk, cnt)

raw_xtr2 <- split_it(reverse_data, breaks)

xtr2 <- 
  raw_xtr2 %>% 
  group_by(x1, x2, x3) %>%
  summarise(cnt3 = n()) %>% 
  ungroup() %>% 
  left_join(merge_xtr, by = colnames(X))

xtr2 %>% 
  summarise(
    risk_new = sum(risk*cnt3, na.rm = T)/sum(cnt3, na.rm = T), 
    risk_old = sum(risk*cnt, na.rm = T)/sum(cnt, na.rm = T)
  )



# germancredit ----


str(germancredit)


# Бининг + риск кривые


bins2 <- 
  woebin(
    germancredit, 
    y = "creditability", 
    x = c("age.in.years", "credit.amount","duration.in.month"), 
    method = "tree", 
    bin_num_limit = 4
  )

woebin_plot(bins2)

g_bads <- 2 - germancredit$creditability %>% as.numeric()
plot(
  germancredit[,c("age.in.years", "credit.amount","duration.in.month")], 
  col = alpha(g_bads + 1, 0.5 + g_bads * 0.5), 
  pch = g_bads + 1
)


train <- germancredit[,c("age.in.years", "credit.amount","duration.in.month")]


g_breaks <- make_breaks(train, m)
g_raw_xtr <- split_it(train, g_breaks) %>% mutate(bads = g_bads)
g_xtr <- make_risk_hypercube(g_raw_xtr, m)

g_xtr %>% 
  mutate(risk = bads2/cnt2) %>% 
  ggplot() +
  geom_raster(aes(x = age.in.years, y = credit.amount, fill = risk, alpha = 1/group^0.01)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_jitter(aes(x = age.in.years, y = credit.amount, color = as.factor(bads)), g_raw_xtr, alpha = 0.3) +
  geom_label(aes(x = age.in.years, y = credit.amount, label = round(risk, 2)), alpha = 0.5, color = NA) + 
  geom_text(aes(x = age.in.years, y = credit.amount, label = round(risk, 2))) +
  facet_grid(cols = vars(duration.in.month), labeller = label_both) +
  scale_color_manual(breaks = c(0, 1), values=c("black", "red")) +
  labs(color = 'bads', fill = "risk level") +
  scale_alpha(guide = 'none')


# График числа объединённых ячеек

g_xtr %>% 
  ggplot() +
  geom_raster(aes(x = age.in.years, y = credit.amount, alpha = 1/group^0.01)) +
  geom_text(aes(x = age.in.years, y = credit.amount, label = round(1/group, 2)), color = 'red') +
  facet_grid(cols = vars(duration.in.month), labeller = label_both) +
  scale_alpha(guide = 'none')


# Создание копулы ----

g_U <- pobs(as.matrix(train))

# Calculate kernel density estimate
# from MASS package
bivn_kde12 <- kde2d(g_U[,1], g_U[,2], n = 50)   
bivn_kde13 <- kde2d(g_U[,1], g_U[,3], n = 50)  
bivn_kde23 <- kde2d(g_U[,2], g_U[,3], n = 50)  


# Распределения
par(mfrow = c(1,3))

image(bivn_kde12, xlab = 'u1', ylab = 'u2')
contour(bivn_kde12, add = TRUE)

image(bivn_kde13, xlab = 'u1', ylab = 'u3')
contour(bivn_kde13, add = TRUE)

image(bivn_kde23, xlab = 'u2', ylab = 'u3')
contour(bivn_kde23, add = TRUE)

par(mfrow = c(1,1))

# Поиск подходящего набора копул
g_estimation <- CDVineCopSelect(g_U, type = 2)

fam <- g_estimation$family
par <- g_estimation$par
par2 <- g_estimation$par2

CDVineAIC(g_U, family = fam, par = par, par2 = par2, type = 2)
CDVineBIC(g_U, family = fam, par = par, par2 = par2, type = 2)

# Создание новой копулы
g_U2 <- CDVineSim(NROW(germancredit), family = fam, par = par, par2 = par2, type = 2)
M <- NCOL(train)

g_reverse_data <- 
  map(1:M, ~ approxfun(g_U[,.x], train[,.x])(g_U2[,.x])) %>% 
  as.data.frame()

colnames(g_reverse_data) <- colnames(train)

plot(g_reverse_data, col = alpha(1, 0.2))

g_merge_xtr <- g_xtr %>% mutate(risk = bads2/cnt2) %>% dplyr::select(colnames(train), risk, cnt)

g_raw_xtr2 <- split_it(g_reverse_data, g_breaks)

g_xtr2 <- 
  g_raw_xtr2 %>% 
  group_by(.dots = colnames(train)) %>%
  summarise(cnt3 = n()) %>% 
  ungroup() %>% 
  left_join(g_merge_xtr, by = colnames(train))

g_xtr2 %>% 
  summarise(
    risk_new = sum(risk*cnt3, na.rm = T)/sum(cnt3, na.rm = T), 
    risk_old = sum(risk*cnt, na.rm = T)/sum(cnt, na.rm = T)
  )
