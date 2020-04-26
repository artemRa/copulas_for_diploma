make_risk_hypercube <-
  function(raw_xtr, m = 5, significant_size = 100) {

    # Вводные данные
    var_dim <- NCOL(raw_xtr) - 1
    var_names <- colnames(raw_xtr)[1:var_dim]
    
    # Группировка    
    xtr <- 
      raw_xtr %>% 
      group_by(.dots = var_names) %>%
      summarise(cnt = n(), bads = sum(bads)) %>% 
      ungroup() %>% 
      arrange(desc(cnt))
    
    # Заполнение оставшихся участков
    splits <- rep(list(1:m), var_dim)
    names(splits) <- var_names
    
    xtr <- 
      expand.grid(splits) %>% 
      left_join(xtr, by = var_names) %>% 
      mutate_all(~ ifelse(is.na(.x), 0, .x))
    
    rm(splits)
    
    # Новый размер
    N2 <- NROW(xtr)
    
    # Ближайшие кубики
    exp_grid <- expand.grid(1:N2, 1:N2)
    exp_grid <- exp_grid[which(exp_grid$Var1 - exp_grid$Var2 != 0),]
    dist <- exp_grid %>% apply(1, function(x) dist(rbind(xtr[x[1],1:3], xtr[x[2],1:3])))
    
    xtr$group <- as.integer(NA)
    xtr$cnt2 <- as.integer(NA)
    xtr$bads2 <- as.integer(NA)
    
    for (i in seq(N2)) {
      
      k <- 1
      
      xtr$cnt2[i] <- xtr$cnt[i]
      xtr$bads2[i] <- xtr$bads[i]
      xtr$group[i] <- 1L
      
      if (xtr$cnt[i] < significant_size) {
        
        neigbors <- which(exp_grid$Var1 == i)
        nearest <- neigbors[order(dist[neigbors])]
        
        while (xtr$cnt2[i] < significant_size) {
          
          j <- exp_grid[nearest,]$Var2[k]
          
          xtr$cnt2[i] <- xtr$cnt2[i] + xtr$cnt[j]
          xtr$bads2[i] <- xtr$bads2[i] + xtr$bads[j]
          xtr$group[i] <- xtr$group[i] + 1L
          
          k<- k+1
          
        }
        
        rm(j)
      }
      
      rm(k)
      
    }
    
    rm(dist)
    rm(exp_grid)
    
    
    xtr
  }