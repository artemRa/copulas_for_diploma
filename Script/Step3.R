library(tidyverse)
library(CDVine)
library(VineCopula)
library(MASS)
library(scorecard)
library(readr)
library(lubridate)
library(ggpubr)


source("./Script/Functions/make_risk_hypercube.R")
source("./Script/Functions/split_it.R")

Sys.setlocale("LC_TIME", "C")
loan <- read_csv("./Data/lending-club-loan-data/loan.csv")

# Разница в месяцах
nmonth <- function(x, y) (year(x) * 12 + month(x)) - (year(y) * 12 + month(y))


# Общий вид данных
loan %>% 
  mutate_at(vars(last_pymnt_d, issue_d), ~ paste(.x, "1") %>% as.Date(format = "%b-%Y %d", tz = "GMT")) %>% 
  ggplot() + geom_bar(aes(x = issue_d, fill = loan_status)) + 
  labs(x = NULL, fill = "loan status", y = "Amt")

unique_loan_status <- loan$loan_status %>% unique()

# Рисование риск-индикаторов
group_by_data <- loan %>% 
  mutate_at(vars(last_pymnt_d, issue_d), ~ paste(.x, "1") %>% as.Date(format = "%b-%Y %d", tz = "GMT")) %>% 
  mutate(mob = nmonth(last_pymnt_d, issue_d)) %>% 
  mutate(charged_mob = ifelse(loan_status %in% unique_loan_status[c(3,5,7,9)], mob, NA)) %>%
  group_by(issue_d, charged_mob) %>% 
  summarise(cnt = n())

# Визуализация риск-индикаторов
target_mobs <- c(3, 6, 12, 24, 36)
for_indicators <- expand.grid(issue_d = group_by_data$issue_d %>% unique(), charged_mob = target_mobs)
last_date <- max(group_by_data$issue_d)

group_by_data_total <- 
  group_by_data %>% 
  group_by(issue_d) %>% 
  summarise(total_cnt = sum(cnt)) %>% 
  ungroup()

risk_data1 <-
  group_by_data %>% 
  filter(!is.na(charged_mob)) %>% 
  full_join(for_indicators, by = c("issue_d", "charged_mob")) %>% 
  left_join(group_by_data_total, by = "issue_d") %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~ ifelse(is.na(.x), 0, .x)) %>% 
  arrange(charged_mob, issue_d) %>% 
  group_by(issue_d) %>% 
  mutate(bads = cumsum(cnt)) %>% 
  ungroup() %>%
  mutate(risk = ifelse(total_cnt == 0, NA, bads/total_cnt)) %>% 
  mutate(risk = ifelse(nmonth(last_date, issue_d) <= charged_mob, NA, risk)) %>% 
  filter(charged_mob %in% target_mobs)

# Грфик риск-индикаторов
ggplot() +
  geom_bar(aes(x = issue_d, y = total_cnt / 2e5), stat = 'identity', alpha = 0.3, data = group_by_data_total) +
  geom_line(aes(x = issue_d, y = risk, color = as.factor(charged_mob)), size = 0.9, data = risk_data1) +
  scale_y_continuous(sec.axis = sec_axis(~.* 2e5, name = "Amt")) +
  labs(color = 'defolt mob', x = NULL, y = 'risk level')

ggsave("./Output/lending_club_risk_indicators.png", width = 12, height = 4)

# Подготовка данных для анализа
loan_clear <- loan %>%
  mutate_at(vars(last_pymnt_d, issue_d), ~ paste(.x, "1") %>% as.Date(format = "%b-%Y %d", tz = "GMT")) %>% 
  filter_at(vars(last_pymnt_d, issue_d), ~ !is.na(.x)) %>% 
  mutate(mob = nmonth(last_pymnt_d, issue_d)) %>% 
  mutate(charged_mob = ifelse(loan_status %in% unique_loan_status[c(3,5,7,9)], mob, NA)) %>% 
  mutate(
    bads3  = ifelse(nmonth(last_date, issue_d) <= 3, NA, ifelse(!is.na(charged_mob) & mob <= 3, 1, 0)),
    bads6  = ifelse(nmonth(last_date, issue_d) <= 6, NA, ifelse(!is.na(charged_mob) & mob <= 6, 1, 0)),
    bads12 = ifelse(nmonth(last_date, issue_d) <= 12, NA, ifelse(!is.na(charged_mob) & mob <= 12, 1, 0)),
    bads24 = ifelse(nmonth(last_date, issue_d) <= 24, NA, ifelse(!is.na(charged_mob) & mob <= 24, 1, 0)),
    bads36 = ifelse(nmonth(last_date, issue_d) <= 36, NA, ifelse(!is.na(charged_mob) & mob <= 36, 1, 0))
  ) %>% 
  dplyr::select(
    issue_d, last_pymnt_d, charged_mob, 
    loan_amnt, term, int_rate, installment, annual_inc, dti, emp_length, grade, purpose,
    bads3, bads6, bads12, bads24, bads36
  ) %>% 
  mutate_at(c("dti", "annual_inc"), ~ ifelse(is.na(.x), 0, .x))

# Риск-драйверы
risk_drivers <- c("int_rate", "dti", "annual_inc")
var_bads <- paste0("bads", target_mobs)

# График IV
bins <- woebin(loan_clear %>% filter(!is.na(bads24)), y = "bads24", x = risk_drivers, method = "tree")
woebin_plot(bins)
breaks <- map(risk_drivers, ~ c(-Inf, bins[[.x]]$breaks %>% as.numeric()))

# Grade
woebin(loan_clear %>% filter(!is.na(bads24)), y = "bads24", x = "grade", method = "tree") %>% woebin_plot()

# Ставка / grade
loan_clear %>% 
  head(1000) %>% 
  ggplot() +
  geom_jitter(aes(x = grade, y = int_rate), alpha = 0.7)

ggsave("./Output/lending_club_grade_split.png", width = 12, height = 4)


# Риск по grade
loan_clear %>% 
  filter(issue_d > '2008-01-01') %>% 
  mutate(grade = ifelse(grade %in% c("E", "F", "G"), "E+", grade) %>% as.factor()) %>% 
  group_by(issue_d, grade) %>% 
  summarise(cnt = n(), risk = sum(bads12)/n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = issue_d)) +
  geom_line(aes(y = risk, color = grade), size = 0.9) +
  scale_colour_brewer(palette = "Set1")

# Сырые данные по риск-драйверам
raw_xtr <- loan_clear %>%
  dplyr::select(issue_d, c("grade", risk_drivers, var_bads)) %>% 
  mutate(issue_d = ifelse(issue_d < '2013-01-01', as.Date('2013-01-01'), issue_d) %>% as.Date(origin="1970-01-01")) %>% 
  arrange(issue_d)
  
raw_xtr_list <- raw_xtr %>% group_split(issue_d, keep = F)
names(raw_xtr_list) <- raw_xtr %>% group_keys(issue_d) %>% pull()

raw_xtr_list_group <- raw_xtr_list %>% 
  map(
    ~ cbind(
        .x[, risk_drivers] %>% 
          split_it(breaks = breaks),
        .x[, "bads24"] %>% 
          rename(bads = bads24)
      )
  )

x_first <- raw_xtr_list_group[[1]]
hypercube <- make_risk_hypercube(x_first)

# График гиперкуба
hypercube %>% 
  mutate(risk = bads2/cnt2) %>% 
  ggplot() +
  geom_raster(aes(x = int_rate, y = dti, fill = risk, alpha = 1/group)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_jitter(aes(x = int_rate, y = dti, color = as.factor(bads)), x_first %>% head(1e4), alpha = 0.2) +
  geom_label(aes(x = int_rate, y = dti, label = round(risk, 2)), alpha = 0.5, color = NA) + 
  geom_text(aes(x = int_rate, y = dti, label = round(risk, 2))) +
  facet_grid(cols = vars(annual_inc), labeller = label_both) +
  scale_color_manual(breaks = c(0, 1), values=c("black", "red")) +
  labs(color = 'bads', fill = "risk level") +
  scale_alpha(guide = 'none')

ggsave("./Output/lending_club_hypercube.png", width = 16, height = 4)


X <- raw_xtr_list[[1]][, risk_drivers]
U <- pobs(as.matrix(raw_xtr_list[[1]][, risk_drivers]))


# Calculate kernel density estimate
# from MASS package
bivn_kde12 <- kde2d(U[,1], U[,2], n = 50)   
bivn_kde13 <- kde2d(U[,1], U[,3], n = 50)  
bivn_kde23 <- kde2d(U[,2], U[,3], n = 50)  

# Распределение
par(mfrow = c(1,3))

image(bivn_kde12, xlab = 'u1', ylab = 'u2')
contour(bivn_kde12, add = TRUE)

image(bivn_kde13, xlab = 'u1', ylab = 'u3')
contour(bivn_kde13, add = TRUE)

image(bivn_kde23, xlab = 'u2', ylab = 'u3')
contour(bivn_kde23, add = TRUE)

par(mfrow = c(1,1))

# Оценка
estimation <- CDVineCopSelect(U, type = 2)

fam <- estimation$family
par <- estimation$par
par2 <- estimation$par2

aic <- CDVineAIC(U, family = fam, par = par, par2 = par2, type = 2)
bic <- CDVineBIC(U, family = fam, par = par, par2 = par2, type = 2)

# Восстановление данных
U2 <- CDVineSim(NROW(U),family = fam, par = par, par2 = par2, type = 2)

reverse_data <- 
  map(1:NCOL(U), ~ approxfun(U[,.x], X[,.x] %>% pull())(U2[,.x])) %>% 
  as.data.frame()

colnames(reverse_data) <- risk_drivers
split_reverse_data <- split_it(reverse_data, breaks)

merge_data <- hypercube %>% 
  mutate(risk = bads2/cnt2) %>% 
  dplyr::select(risk_drivers, risk, cnt, bads)

sim_group_data <- 
  split_reverse_data %>% 
  group_by(.dots = risk_drivers) %>%
  summarise(sim_cnt = n()) %>% 
  ungroup() %>% 
  left_join(merge_data, by = risk_drivers)

sim_group_data %>% 
  summarise(
    risk_new = sum(risk*sim_cnt, na.rm = T) / sum(sim_cnt, na.rm = T), 
    risk_old = sum(risk*cnt, na.rm = T) / sum(cnt, na.rm = T), 
    fact_risk = sum(bads, na.rm = T) / sum(cnt, na.rm = T)
  )

bads24_mature <- raw_xtr_list %>% map_lgl(~ !(sum(.x$bads24) %>% is.na()))
num_mature <- which(bads24_mature)

flexy_family_params <- 
  tibble(id = 1, cop = 1:3, fam, par, par2, as_tibble(aic), as_tibble(bic))

reverse_data_list <- list()
reverse_data_list[[1]] <- reverse_data

hypercube_data_list <- list()
hypercube_data_list[[1]] <- hypercube

sim_group_data_list <- list() 
sim_group_data_list[[1]] <- sim_group_data

for (i in num_mature[-1]) {
  
  x_next <- raw_xtr_list_group[[i]]
  x_hypercube <- make_risk_hypercube(x_next)
  
  hypercube_data_list[[i]] <- x_hypercube
  
  X <- raw_xtr_list[[i]][, risk_drivers]
  U <- pobs(as.matrix(raw_xtr_list[[i]][, risk_drivers]))
  
  # Оценка
  estimation0 <- CDVineCopSelect(U, type = 2)
  
  # Без учета прошлой оценки
  x0_fam <- estimation0$family
  x0_par <- estimation0$par
  x0_par2 <- estimation0$par2
  
  aic0 <- CDVineAIC(U, family = x0_fam, par = x0_par, par2 = x0_par2, type = 2)
  bic0 <- CDVineBIC(U, family = x0_fam, par = x0_par, par2 = x0_par2, type = 2)
  
  flexy_family_params <- 
    rbind(
      flexy_family_params,
      tibble(id = i, cop = 1:3, fam = x0_fam, par = x0_par, par2 = x0_par2, as_tibble(aic0), as_tibble(bic0)) 
    )
  
  # Восстановление данных
  U2 <- CDVineSim(NROW(U),family = x0_fam, par = x0_par, par2 = x0_par2, type = 2)
  
  reverse_data <- 
    map(1:NCOL(U), ~ approxfun(U[,.x], X[,.x] %>% pull())(U2[,.x])) %>% 
    as.data.frame()
  
  colnames(reverse_data) <- risk_drivers
  
  reverse_data_list[[i]] <- reverse_data
  split_reverse_data <- split_it(reverse_data, breaks)
  
  merge_data <- x_hypercube %>% 
    mutate(risk = bads2/cnt2) %>% 
    dplyr::select(risk_drivers, risk, cnt, bads)
  
  sim_group_data <- 
    split_reverse_data %>% 
    group_by(.dots = risk_drivers) %>%
    summarise(sim_cnt = n()) %>% 
    ungroup() %>% 
    left_join(merge_data, by = risk_drivers)
  
  sim_group_data_list[[i]] <- sim_group_data
  
  # Очистка
  rm(U2)
  rm(reverse_data)
  rm(sim_group_data)
  rm(x_hypercube)
  rm(merge_data)
  rm(X)
  rm(U)
  rm(x_next)
  
}




cop_dict <- 
  tribble(
    ~ fam,  ~ copula,
    1, "Gaussian copula", 
    2, "Student t copula (t-copula)",
    3, "Clayton copula",
    5, "Frank copula",
    6, "Joe copula",
    10, "BB8 copula", 
    13, "rotated Clayton copula (180 degrees; “survival Clayton”)",
    14, "rotated Gumbel copula (180 degrees; “survival Gumbel”)",
    20, "rotated BB8 copula (180 degrees; “survival BB8”)",
    30, "rotated BB8 copula (90 degrees)",
    33, "rotated Clayton copula (270 degrees)" 
  ) %>% mutate_at("copula", as.factor)


flexy_family_params %>% pull(fam) %>% unique() %>% sort()

flexy_family_params %>% 
  left_join(cop_dict, by = "fam") %>% 
  filter(cop == 3) %>% 
  ggplot(aes(x = id, y = copula, color = as.factor(cop), fill =  as.factor(cop))) +
  geom_point(size = 3) +
  geom_text(aes(label = id), vjust = -1, size = 3)

# Грaфики c параметрами
for (i in 1:3) {
  
  flexy_family_params %>% 
    left_join(cop_dict, by = "fam") %>% 
    filter(cop == 1) %>%
    mutate(par2 = ifelse(par2 == 0, NA, par2)) %>% 
    ggplot(aes(x = id, y = par, color = copula, fill =  copula)) +
    geom_point(size = 4, aes(alpha = BIC, shape = 'Par1')) + 
    geom_line(linetype = 2) +
    geom_text(aes(label = id), vjust = -1, size = 3) + 
    geom_point(size = 4, aes(y = par2, alpha = BIC, shape = 'Par2')) + 
    geom_line(linetype = 2, aes(y = par2)) +
    geom_text(aes(label = id, y = par2), vjust = -1, size = 3) + 
    labs(x = NULL, shape = 'parameter', y = 'parameter')
  
  ggsave(paste0("./Output/lendin_club_params_cop", i, ".png"), width = 12, height = 5, dpi = 240)
}

par1_df <- par2_df <- fam_df <- list()

for (i in 1:3) {

  previous <- 
    flexy_family_params %>% 
    filter(id <= 24) %>% 
    filter(cop == i) %>%
    arrange(id) %>% 
    group_split(fam)
  
  possible_lines <- previous %>% map_lgl(~ NROW(.x) >= 3) %>% which()
  
  fam_df[[i]] <- 
    (flexy_family_params %>% 
       filter(id <= 24) %>% 
       filter(cop == i) %>%
       arrange(id) %>% 
       group_keys(fam) %>% 
       pull()
     )[possible_lines]
  
  par1_df[[i]] <- 
    map(
      possible_lines, 
      ~ lm(par ~ id, previous[[.x]]) %>% 
        predict(data.frame(id = num_mature[-(1:24)]))
    ) %>% 
    as.data.frame() %>% 
    as_tibble()
  
  par2_df[[i]] <- 
    map(
      possible_lines, 
      ~ lm(par2 ~ id, previous[[.x]]) %>% 
        predict(data.frame(id = num_mature[-(1:24)]))
    ) %>% 
    as.data.frame() %>% 
    as_tibble()
  
  colnames(par1_df[[i]]) <- paste0("fam", fam_df[[i]])
  colnames(par2_df[[i]]) <- paste0("fam", fam_df[[i]])
  
}

N <- 10000

fam_flow <- c(10,2,30)
par1_flow <- data.frame(par1_df[[1]]$fam10, par1_df[[2]]$fam2, par1_df[[3]]$fam30) %>% as.matrix()
par2_flow <- data.frame(par2_df[[1]]$fam10, par2_df[[2]]$fam2, par2_df[[3]]$fam30) %>% as.matrix()

steps <- num_mature[-(1:24)] %>% length() %>% seq()

risk <- NA

for (j in steps) {
  
  U2 <- CDVineSim(N, family = fam_flow, par = par1_flow[j,], par2 = par2_flow[j,], type = 2)
  
  reverse_data <- 
    map(1:NCOL(U), ~ approxfun(U[,.x], X[,.x] %>% pull())(U2[,.x])) %>% 
    as.data.frame()
  
  colnames(reverse_data) <- risk_drivers
  split_reverse_data <- split_it(reverse_data, breaks)
  
  merge_data <- hypercube %>% 
    mutate(risk = bads2/cnt2) %>% 
    dplyr::select(risk_drivers, risk, cnt, bads)
  
  risk[j] <-
    split_reverse_data %>% 
    group_by(.dots = risk_drivers) %>%
    summarise(sim_cnt = n()) %>% 
    ungroup() %>% 
    left_join(merge_data, by = risk_drivers) %>% 
    summarise(risk_new = sum(risk*sim_cnt, na.rm = T) / sum(sim_cnt, na.rm = T)) %>% pull()
  
}


fact_risk <- 
  risk_data1 %>% 
  filter(charged_mob == 24) %>% 
  filter(issue_d >= '2013-01-01') %>% 
  arrange(issue_d) %>% 
  mutate(n = row_number() + 1) %>% 
  filter(n > 25) %>% 
  filter(!is.na(risk)) %>% 
  pull(risk)
  

plot(steps, fact_risk, type = 'l', col = 'red', ylim = c(0.1, 0.15))



