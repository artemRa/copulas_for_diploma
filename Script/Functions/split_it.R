# Разбивка таблицы на бакеты
# X - входой data.frame признаков
split_it <- 
  function(X, breaks, m = 5) {
    map2(X, breaks, ~ cut(.x, .y, labels = seq(m))) %>% 
      as_tibble() %>% 
      mutate_if(is.factor, as.integer)
  }
