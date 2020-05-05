library(tidyverse)
library(CDVine)
library(VineCopula)
library(MASS)
library(scorecard)
library(readr)
library(lubridate)
library(ggpubr)
library(conflicted)
library(gganimate)
library(yardstick)


source("./Script/Functions/make_risk_hypercube.R")
source("./Script/Functions/split_it.R")


# Конфликты функций
conflict_prefer("filter", "dplyr", "base")
conflict_prefer("filter", "dplyr", "stats")
conflict_prefer("select", "dplyr", "MASS")

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
    13, "rotated Clayton copula \n (180 degrees; “survival Clayton”)",
    14, "rotated Gumbel copula \n (180 degrees; “survival Gumbel”)",
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





hypercube_history_df <- 
  hypercube_data_list %>% 
  map(~ mutate(., risk = bads2 / cnt2) %>% select(c(risk_drivers, "risk", "group"))) %>% 
  reduce(left_join, by = risk_drivers, suffix = c("", "2"))

risk_var <-  tidyselect::vars_select(colnames(hypercube_history_df), starts_with("risk"))
group_var <- tidyselect::vars_select(colnames(hypercube_history_df), starts_with("group"))

risk_hisory_df <- hypercube_history_df %>% select(risk_drivers, risk_var)
group_hisory_df <- hypercube_history_df %>% select(risk_drivers, group_var)

colnames(risk_hisory_df)[-seq(length(risk_drivers))] <- names(num_mature)
colnames(group_hisory_df)[-seq(length(risk_drivers))] <- names(num_mature)

hypercube_history_df <-
  inner_join(
    risk_hisory_df %>% 
      pivot_longer(
        names_to = "date",
        values_to = "risk",
        -risk_drivers
      ),
    group_hisory_df %>% 
    pivot_longer(
      names_to = "date",
      values_to = "group",
      -risk_drivers
    ),
    by = c(risk_drivers, "date")
  ) %>% 
  mutate_if(is.character, as.Date)


# Анимация
test <- hypercube_history_df %>% 
  mutate_if(is.character, as.Date) %>% 
  ggplot() +
  geom_raster(aes(x = int_rate, y = dti, fill = risk)) +
  scale_fill_viridis_c(option = "plasma") +
  # geom_label(aes(x = int_rate, y = dti, label = round(risk, 2)), alpha = 0.5, color = NA) + 
  # geom_text(aes(x = int_rate, y = dti, label = round(risk, 2))) +
  facet_grid(cols = vars(annual_inc), labeller = label_both) +
  scale_color_manual(breaks = c(0, 1), values=c("black", "red")) +
  transition_time(date) +
  labs(title = "Date: {frame_time}")

animate(test, width = 1600, height = 400, fps = 3)
anim_save("./Output/lending_club_hypercube_anim.gif")



hypercube_history_df %>% 
  ggplot() +
  geom_line(aes(x = date, y = risk, color = as.factor(int_rate)), show.legend = F) +
  facet_grid(dti ~ annual_inc + int_rate, labeller = label_both) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()
  )

ggsave("./Output/lendin_club_risk_hyper_dyn.png", width = 24, height = 5, dpi = 240)

# save.image(file = "./Output/my_work_space.RData")



N2 <- 100
risk_dots <- list()

for (i in num_mature[-1]) {
  
  N <- (reverse_data_list %>% map_int(NROW))[i]
  par_flow <- flexy_family_params %>% filter(id == i-1)
  
  U2 <- CDVineSim(N * N2, family = par_flow$fam, par = par_flow$par, par2 = par_flow$par2, type = 2)
  reverse_data <- 
    map(1:NCOL(U), ~ approxfun(U[,.x], X[,.x] %>% pull())(U2[,.x])) %>% 
    as.data.frame()
  
  colnames(reverse_data) <- risk_drivers
  
  risk_dots[[i]] <- list()
  
  for (j in 1:i) {
    
    merge_df <- 
      hypercube_data_list[[j]] %>% 
      mutate(risk = bads2/cnt2, cl = 1/group) %>% 
      select(risk_drivers, risk, cl)
    
    risk_dots[[i]][[j]] <- 
      split_it(reverse_data, breaks) %>% 
      mutate(split_gr = rep(seq(N2), N)) %>% 
      group_by(.dots = c(risk_drivers, "split_gr")) %>%
      summarise(sim_cnt = n()) %>% 
      ungroup() %>% 
      left_join(
        merge_df,
        by = risk_drivers
      ) %>% 
      group_by(split_gr) %>% 
      summarise(
        risk = sum(sim_cnt * risk, na.rm = T) / sum(sim_cnt, na.rm = T),
        cl = sum(sim_cnt * cl, na.rm = T) / sum(sim_cnt, na.rm = T)
      )
    
  }
  
  print(i)
}


fact_risk <- 
  risk_data1 %>% 
  filter(charged_mob == 24) %>% 
  filter(issue_d > '2013-01-01') %>% 
  arrange(issue_d) %>% 
  mutate(id = row_number()) %>% 
  filter(!is.na(risk)) %>% 
  select(id, issue_d, risk)

median_predict <- 
  map_df(
    num_mature,
    function(k) 
      risk_dots %>% map_dbl( ~ ifelse(is.null(.x) | NROW(.x) < k, NA, median(.x[[k]]$risk)))
  ) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(names_to = "risk_base", values_to = "median", -id) %>% 
  mutate_if(is.character, as.Date)

cl_predict <- 
  map_df(
    num_mature,
    function(k) 
      risk_dots %>% map_dbl( ~ ifelse(is.null(.x) | NROW(.x) < k, NA, median(.x[[k]]$cl)))
  ) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(names_to = "risk_base", values_to = "cl", -id) %>% 
  mutate_if(is.character, as.Date)

full_predict <- 
  map_df(
    num_mature,
    function(k) 
      risk_dots %>% map_dbl( ~ ifelse(is.null(.x) | NROW(.x) < k, NA, sd(.x[[k]]$risk)))
  ) %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(names_to = "risk_base", values_to = "sd", -id) %>% 
  mutate_if(is.character, as.Date) %>% 
  left_join(median_predict, by = c("risk_base", "id")) %>% 
  left_join(cl_predict, by = c("risk_base", "id"))


loan_clear_grade <- 
  loan_clear %>% 
  mutate(issue_d = ifelse(issue_d < '2013-01-01', as.Date('2013-01-01'), issue_d) %>% as.Date(origin="1970-01-01")) %>% 
  mutate(grade = ifelse(grade %in% c("E", "F", "G"), "E+", grade) %>% as.factor()) %>% 
  group_by(issue_d, grade) %>% 
  summarise(cnt = n(), risk = sum(bads24)/n()) %>% 
  ungroup()

grade_split_data <- loan_clear_grade %>% 
  left_join(loan_clear_grade %>% select(issue_d, grade, risk), by = "grade", suffix = c("", "_base")) %>% 
  mutate(bads_sint = risk_base * cnt, bads = risk * cnt) %>% 
  group_by(issue_d, issue_d_base) %>% 
  summarise(risk_pred = sum(bads_sint, na.rm = T) / sum(cnt, na.rm = T), 
            risk_fact = sum(bads, na.rm = T) / sum(cnt, na.rm = T)) %>% 
  filter(issue_d > issue_d_base) %>% 
  arrange(issue_d, desc(issue_d_base)) %>% 
  mutate(lagged = row_number()) %>% 
  # filter(lagged == 1) %>% 
  filter(lagged %in% c(6, 12, 24)) %>%
  filter(issue_d_base >  '2013-01-01') %>% 
  left_join(fact_risk %>% select(issue_d, id), by = "issue_d")


loan_clear_risk_drivers <- map2(raw_xtr_list_group,
                                as.Date(names(raw_xtr_list_group)),
                                ~ mutate(.x, issue_d = .y)) %>% 
  map_df(cbind) %>% 
  group_by(.dots = c(risk_drivers, "issue_d")) %>% 
  summarise(risk = sum(bads, na.rm = T) / n(), cnt = n())

group_by_risk_drivers <- loan_clear_risk_drivers %>% 
  left_join(loan_clear_risk_drivers, 
            by = risk_drivers, 
            suffix = c("", "_base")) %>% 
  filter(issue_d > issue_d_base) %>% 
  group_by(issue_d, issue_d_base) %>% 
  summarise(risk_pred = sum(risk_base * cnt, na.rm = T) / sum(cnt, na.rm = T), 
            risk_fact = sum(risk * cnt, na.rm = T) / sum(cnt, na.rm = T)) %>% 
  arrange(issue_d, desc(issue_d_base)) %>% 
  mutate(lagged = row_number()) %>% 
  filter(lagged %in% c(1, 6, 12, 24)) %>% 
  left_join(fact_risk %>% select(issue_d, id), by = "issue_d")
  




full_predict %>% 
  mutate(id = id - 1) %>%
  filter(!is.na(median)) %>% 
  arrange(id, desc(risk_base)) %>% 
  group_by(id) %>% 
  mutate(lagged = row_number()-2) %>% 
  ungroup() %>% 
  # filter(risk_base >  '2013-01-01') %>% 
  # filter(lagged == 1) %>% 
  filter(lagged %in% c(6, 12, 24)) %>% 
  # arrange(lagged, id) %>% 
  # print(n =  100)
  ggplot(aes(x = id)) +
  geom_ribbon(aes(ymin = median - sd, 
                  ymax = median + sd, 
                  fill = as.factor(lagged)), 
              alpha = 0.7) +
  geom_ribbon(aes(ymin = median - 3*sd, 
                  ymax = median + 3*sd, 
                  fill = as.factor(lagged)), 
              alpha = 0.3) +
  geom_line(aes(y = median, color = as.factor(lagged), linetype = "D-vine prediction")) +
  geom_line(data = fact_risk, aes(x = id, y = risk)) + 
  geom_line(data = grade_split_data, 
            aes(x = id, y = risk_pred, color = as.factor(lagged), linetype = "grade split prediction"), 
            size = 1.2) +
  # geom_line(data = group_by_risk_drivers, aes(x = id, y = risk_pred, color = as.factor(lagged)), linetype = 2) +  
  labs(x = NULL, y = 'risk level', color = 'lag', fill = 'lag') +
  ylim(0.08, 0.15)


ggsave("./Output/lending_club_median_predict.png", width = 12, height = 9)


mod6 <- 
  fact_risk %>% 
  inner_join(grade_split_data %>% 
              ungroup() %>% 
              # filter(lagged == 6) %>% 
              select(id, lagged, risk_pred),
            by = "id") %>% 
  inner_join(full_predict %>% 
              mutate(id = id - 1) %>%
              filter(!is.na(median)) %>% 
              arrange(id, desc(risk_base)) %>% 
              group_by(id) %>% 
              mutate(lagged = row_number()-2) %>% 
              ungroup() %>% 
              select(id, lagged, median),
            by = c("id", "lagged")) %>% 
  pivot_longer(cols = c("risk_pred", "median"),
               names_to = "prediction", 
               values_to = "risk_level") %>% 
  group_by(lagged, prediction)

# Сравнение
mod6 %>% yardstick::rmse(risk, risk_level, na.rm = T)
mod6 %>% yardstick::mae(risk, risk_level, na.rm = T)
mod6 %>% yardstick::mase(risk, risk_level, na.rm = T)


conflict_prefer("lag", "dplyr", "stats")

x <- runif(5)
cbind(ahead = lead(x), x, behind = lag(x))

cop_params <-
  flexy_family_params %>%
  select(id, cop, fam) %>% 
  arrange(cop, id) %>% 
  mutate(change = ifelse(fam != lag(fam), 1, 0) %>% as.factor())


cop_dict2 <- 
  tribble(
    ~ fam,  ~ copula,
    1, "Gaussian copula", 
    2, "Student t copula\n(t-copula)",
    3, "Clayton copula",
    5, "Frank copula",
    6, "Joe copula",
    10, "BB8 copula", 
    13, "rotated Clayton copula\n(180 degrees; “survival Clayton”)",
    14, "rotated Gumbel copula\n(180 degrees; “survival Gumbel”)",
    20, "rotated BB8 copula\n(180 degrees; “survival BB8”)",
    30, "rotated BB8 copula\n(90 degrees)",
    33, "rotated Clayton copula\n(270 degrees)" 
  ) %>% mutate_at("copula", as.factor)


for (i in 1:3) {
  
  hypercube_history_df %>% 
    left_join(fact_risk %>% select(id, issue_d), by = c('date' = 'issue_d')) %>% 
    mutate_at("id", ~ coalesce(., 0L)) %>% 
    group_by(.dots = risk_drivers) %>% 
    arrange(id) %>%
    mutate(risk_diff = risk / lead(risk) - 1) %>% 
    # filter_at(risk_drivers, ~ . == 1L)
    ungroup() %>% 
    left_join(cop_params, by = "id") %>%
    left_join(cop_dict2, by = "fam") %>% 
    # filter(int_rate == 5L) %>%
    mutate(int_rate_gr = ifelse(int_rate == 5L, 'worst', 'other')) %>% 
    filter_at(c("risk_diff", "change"), ~ !is.na(.)) %>%
    filter(cop == i) %>%
    ggplot(aes(x = as.factor(change), y = risk_diff, fill = as.factor(change))) +
    geom_jitter(alpha = 0.1, size = 2) +
    geom_violin(draw_quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9), size = 0.75, alpha = 0.7) +
    facet_grid(cols = vars(copula), rows = vars(int_rate_gr)) +
    theme(strip.text.x = element_text(size = 9, angle = 90)) +
    labs(fill = 'change', y = 'risk level change', x = NULL) +
    scale_y_continuous(labels = scales::percent, limits = c(-1, 1))
  
  ggsave(paste0("./Output/lendin_club_cop_risk_distrib", i, ".png"), width = 12, height = 9, dpi = 500)
  
}

  
fact_risk %>% 
  arrange(id) %>% 
  mutate(risk_diff = risk / lead(risk) - 1) %>% 
  left_join(cop_params, by = "id") %>%
  filter(cop == 1) %>% 
  ggplot() +
  geom_col(aes(x = id, y = risk_diff, fill = as.factor(fam)))
 
hypercube_history_df %>% 
  left_join(fact_risk %>% select(id, issue_d), by = c('date' = 'issue_d')) %>% 
  arrange(id) %>% 
  mutate(risk_diff = risk / lead(risk) - 1) %>% 
  left_join(cop_params, by = "id") %>%
  filter(cop == 1) %>%
  ggplot() +
  geom_violin(aes(x = as.factor(id), y = risk_diff, fill = as.factor(fam)), draw_quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)) +
  ylim(-1,1)


# Симуляция роста риска

X_good <- raw_xtr_list[[1]] %>% filter(bads24 == 0) %>% select(risk_drivers)
X_bad <- raw_xtr_list[[1]] %>% filter(bads24 == 1) %>% select(risk_drivers)

U_good <- pobs(as.matrix(X_good))
U_bad <- pobs(as.matrix(X_bad))

# Оценка
estimation <- map(list(U_good, U_bad), CDVineCopSelect, type = 2)
  
# Восстановление данных
Xscenario <- round(NROW(X_bad) * seq(0.5, 3, 0.1))
Ubad <- map(Xscenario, 
    ~ CDVineSim(., 
                family = estimation[[2]]$family, 
                par = estimation[[2]]$par, 
                par2 = estimation[[2]]$par2, 
                type = 2))

Ugood <- CDVineSim(NROW(X_good), 
                   family = estimation[[1]]$family, 
                   par = estimation[[1]]$par, 
                   par2 = estimation[[1]]$par2, 
                   type = 2)

reverse_data_good <- 
  map(1:NCOL(U_good), 
      ~ approxfun(U_good[,.x], 
                  X_good[,.x] %>% 
                    pull())(Ugood[,.x])) %>% 
  as.data.frame() %>% 
  rename_all(~ risk_drivers)

reverse_data_bad <- 
  map(Ubad, 
      function(num) {
        map(1:NCOL(U_bad),
            ~ approxfun(U_bad[,.x], X_bad[,.x] %>% pull())(num[,.x])) %>% 
          as.data.frame() %>% 
          rename_all(~ risk_drivers)
      })

samples <- map2(reverse_data_bad, 
                list(reverse_data_good), 
                ~ rbind(mutate(.x, bad = 1L), 
                        mutate(.y, bad = 0L)))


split_sampl <- map(samples, 
                   ~ split_it(.x[, risk_drivers], breaks) %>% 
                     mutate(bads = .x$bad) %>% 
                     group_by(.dots = risk_drivers) %>% 
                     summarise(cnt = n(), bads = sum(bads)) %>% 
                     mutate(risk = bads / cnt))

final_simulation <- map2_df(seq(0.5, 3, 0.1), split_sampl, ~ rbind(mutate(.y, Xscenario = .x))) %>% ungroup()





raw_xtr_ready <- raw_xtr %>% filter(!is.na(bads24))

fact_data <- 
  cbind(issue_d = raw_xtr_ready$issue_d, 
        raw_xtr_ready[,risk_drivers] %>% split_it(breaks), 
        bads = raw_xtr_ready$bads24) %>% 
  group_by(.dots = c("issue_d", risk_drivers)) %>% 
  summarise(bads = sum(bads), cnt = n())

fact_data_total <- 
  fact_data %>% 
  group_by(issue_d) %>% 
  summarise(risk = sum(bads) / sum(cnt),
            cnt = sum(cnt)) %>% 
  ungroup()

fact_data_total <- 
  fact_data_total %>% 
  mutate(risk_base = 
           fact_data_total %>% 
           filter(issue_d == '2013-01-01') %>% 
           pull(risk)) %>% 
  mutate(Xscenario = risk / risk_base)



sim_data_for_ggplot <- 
  final_simulation %>% 
  filter_at(risk_drivers, ~ !is.na(.)) %>% 
  left_join(filter(final_simulation, Xscenario == 1), 
            by = risk_drivers, 
            suffix = c("", "_base")) %>% 
  group_by(Xscenario) %>% 
  mutate(cnt_part = cnt / sum(cnt),
         cnt_part_base = cnt_base / sum(cnt_base)) %>% 
  ungroup() %>% 
  group_by(.dots = c("Xscenario", risk_drivers)) %>% 
  summarise(risk = sum(bads) / sum(cnt), 
            cnt = sum(cnt),
            cnt_part = sum(cnt_part, na.rm = T),
            risk_base = sum(bads_base, na.rm = T) / sum(cnt_base, na.rm = T),
            cnt_base = sum(cnt_base, na.rm = T),
            cnt_part_base = sum(cnt_part_base, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(change_risk = risk / risk_base,
         change_cnt = cnt_part / cnt_part_base)

fact_data_x <-
  fact_data %>% 
  left_join(fact_data_total %>% 
              select(issue_d, Xscenario), 
            by = "issue_d") %>%
  left_join(fact_data %>% 
              filter(issue_d == '2013-01-01'), 
            by = risk_drivers, 
            suffix = c("", "_base")) %>% 
  
  mutate(Xscenario = cut(Xscenario,
                          breaks = c(-Inf, 0.9999, 1, 1.2, 1.3, +Inf),
                          labels = c(0.9, 1, 1.2, 1.3, 1.5)) %>%
           as.character() %>%
           as.numeric()) %>%
  
  group_by(Xscenario) %>% 
  mutate(cnt_part = cnt / sum(cnt),
         cnt_part_base = cnt_base / sum(cnt_base)) %>% 
  ungroup() %>% 
  
  group_by(.dots = c("Xscenario", risk_drivers)) %>% 
  summarise(risk = sum(bads) / sum(cnt), 
            cnt = sum(cnt),
            cnt_part = sum(cnt_part),
            risk_base = sum(bads_base, na.rm = T) / sum(cnt_base, na.rm = T),
            cnt_base = sum(cnt_base, na.rm = T),
            cnt_part_base = sum(cnt_part_base)) %>% 
  ungroup() %>% 
  mutate(change_risk = risk / risk_base,
         change_cnt = cnt_part / cnt_part_base)


  

# Фактические данные
  
fact_data_x %>%  
  mutate(risk_label = 
           ifelse(round(change_risk, 2) == 1 | is.infinite(change_risk) | is.nan(change_risk),
                  NA,
                  paste0("X", round(change_risk, 1)))) %>% 
  
  ggplot(aes(x = int_rate, y = dti)) +
  geom_raster(aes(fill = risk, alpha = cnt_part^0.01)) +
  scale_fill_viridis_c(option = "plasma") +
  geom_text(aes(label = risk_label)) + 
  facet_grid(Xscenario + . ~ annual_inc, labeller = label_both) +
  scale_alpha(guide = 'none') +
  labs(fill = "risk level")

ggsave("./Output/lendin_club_fact_Xscenario.png", width = 16, height = 12, dpi = 500)


# Симуляция

sim_data_for_ggplot %>% 
  filter(Xscenario %in% c(0.9, 1, 1.2, 1.3, 1.5)) %>% 
  mutate(risk_label = 
           ifelse(round(change_risk, 2) == 1 | is.infinite(change_risk) | is.nan(change_risk),
                  NA,
                  paste0("X", round(change_risk, 1)))) %>% 
  
  inner_join(fact_data_x, by = c(risk_drivers, "Xscenario"), suffix = c("", "_fact")) %>% 
  mutate(risk_diff = change_risk / change_risk_fact - 1) %>% 
  mutate(risk_diff2 = ifelse(is.na(risk_label), NA, pmin(2,risk_diff))) %>%
  
  ggplot(aes(x = int_rate, y = dti)) +
  geom_raster(aes(fill = risk, alpha = cnt_part^0.01)) +
  scale_fill_viridis_c(option = "plasma") +
  
  geom_point(aes(color = risk_diff2), size = 9.5) +
  scale_colour_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         na.value = NA) +
  
  geom_text(aes(x = int_rate, y = dti, label = risk_label)) + 
  
  facet_grid(Xscenario + . ~ annual_inc, labeller = label_both) +
  scale_alpha(guide = 'none') +
  labs(fill = "risk level", color = "error level")  

ggsave("./Output/lendin_club_sim_Xscenario.png", width = 16, height = 12, dpi = 500)

for (i in 1:3) {
  for (j in 1:2) {
    
    predictor <- risk_drivers[i]
    type_plot <- c("change_part", "change_risk")[j]
    
    sim_data_for_ggplot %>% 
      group_by(.dots = c('Xscenario', predictor)) %>% 
      summarise(risk = sum(risk * cnt) / sum(cnt), 
                cnt = sum(cnt),
                cnt_part = sum(cnt_part),
                risk_base = sum(risk_base * cnt_base, na.rm = T) / sum(cnt_base, na.rm = T),
                cnt_base = sum(cnt_base, na.rm = T),
                cnt_part_base = sum(cnt_part_base)) %>% 
      ungroup() %>% 
      mutate(change_risk = risk / risk_base,
             change_part = cnt_part / cnt_part_base) %>%
      mutate_at(predictor, as.factor) %>% 
      ggplot() +
      geom_line(aes_string(x = "Xscenario", y = type_plot, color = predictor), alpha = 0.7,  linetype = 2) +
      geom_line(data = 
                  fact_data_x %>% 
                  filter(Xscenario >= 1) %>% 
                  group_by(.dots = c('Xscenario', predictor)) %>% 
                  summarise(risk = sum(risk * cnt) / sum(cnt), 
                            cnt = sum(cnt),
                            cnt_part = sum(cnt_part),
                            risk_base = sum(risk_base * cnt_base, na.rm = T) / sum(cnt_base, na.rm = T),
                            cnt_base = sum(cnt_base, na.rm = T),
                            cnt_part_base = sum(cnt_part_base)) %>% 
                  ungroup() %>% 
                  mutate_at(predictor, as.factor) %>% 
                  mutate(change_risk = risk / risk_base,
                         change_part = cnt_part / cnt_part_base),
                aes_string(x = "Xscenario", y = type_plot, color = predictor)) +
      labs(color = predictor)
    
    ggsave(paste0("./Output/lendin_club_sim_vs_fact_", predictor,"_", type_plot ,".png"), width = 5, height = 4)
    
  } 
}

