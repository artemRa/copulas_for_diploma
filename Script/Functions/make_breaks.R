# Создание бакетов для группировки
# X - входой data.frame признаков
make_breaks <- 
  function(X, m) {
    map(X, ~ {
      pre <- quantile(.x, probs = seq(0, 1, 1/m))
      pre[1] <- (-Inf)
      pre[m + 1] <- Inf
      pre
    })
  }