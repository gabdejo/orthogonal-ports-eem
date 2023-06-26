table1_portfolio_result_function <- function(sigma, mu, gamma, type){
  unit_vector <- rep(1,length(sigma[,1]))
  if (type == "S") {
    weight <- (1/gamma) * (solve(sigma) %*% mu)
    sqr_shrp <- t(mu) %*% solve(sigma) %*% mu
    utility <-  (sqr_shrp^2)/(2*gamma)
  }
  if (type == "G") {
    weight <- (solve(sigma) %*% unit_vector) / as.numeric(t(unit_vector)%*%solve(sigma)%*%unit_vector)
    sqr_shrp <- ((t(mu)%*%solve(sigma)%*%unit_vector)^2)/((t(unit_vector)%*%solve(sigma)%*%unit_vector))
    mu_G <- mu %*% weight
    sqr_sigma_G <- t(weight)%*%sigma%*%weight
    utility <-  mu_G - (gamma/2)*sqr_sigma_G
  }
  if (type == "G~") {
    weight_G <- (solve(sigma) %*% unit_vector) / as.numeric(t(unit_vector)%*%solve(sigma)%*%unit_vector)
    sqr_shrp_G <- ((t(mu)%*%solve(sigma)%*%unit_vector)^2)/((t(unit_vector)%*%solve(sigma)%*%unit_vector))
    mu_G <- mu %*% weight_G  
    sqr_sigma_G <- t(weight_G)%*%sigma%*%weight_G
    weight <- solve(sigma) %*% unit_vector
    sqr_shrp <- sqr_shrp_G
    utility <-  mu_G/sqr_sigma_G - (gamma/2)*(1/sqr_sigma_G)
  }
  if (type == "H") {
    R <- solve(sigma) - ((solve(sigma)%*%unit_vector%*%t(unit_vector)%*%solve(sigma))/as.numeric(t(unit_vector)%*%solve(sigma)%*%unit_vector))
    weight <- (1/gamma)*(R%*%mu)
    sqr_shrp <- t(mu)%*%R%*%mu
    utility <-  sqr_shrp/(2*gamma)
  }
  result <- list(weight, sqr_shrp, utility)
  return(result)
}

out_of_sample_performance_implementable_function <- function(n, t, sigma, mu, gamma, implemented_type){
  set.seed(420)
  table1_result_G <- table1_portfolio_result_function(sigma, mu, gamma, "G")
  weight_G <- table1_result_G[[1]]
  mu_G <- mu %*% weight_G
  sqr_sigma_G <- t(weight_G)%*%sigma%*%weight_G
  sqr_shrp_G <- table1_result_G[[2]]

  table1_result_H <- table1_portfolio_result_function(sigma, mu, gamma, "H")
  weight_H <- table1_result_H[[1]]
  sqr_shrp_H <- table1_result_H[[2]]
  
  vector_Y1 = ((n+1)/(t-n-1))*rf(n*t,n+1,t-n-1,t*sqr_shrp_H)
  vector_Y2 = ((n-1)/(t-n-1))*rf(n*t,n-1,t-n-1,t*sqr_shrp_H)
  if (implemented_type == "KZ") {
    f1 = vector_Y1/(vector_Y1 + (n/t))
    f2 = vector_Y2/(vector_Y2 + (n/t))
    k1 = (t - n - 1)*(t - n - 4)/(2*gamma*(t - 2)*(t - n - 2))
    k2 = (t- n- 4)*(t- n- 5)/(2*gamma*(t- 2)*(t- n- 2)*(t- n- 3))
    k3 = -(t-4)*(t-n-1)*(t-n-4)/(2*gamma*t*(t-2)*(t-n-2)*(t-n-3))
    k4 = (t - n - 4)/(gamma*(t - 2))
    k5 = -(t - n - 1)*((t - n - 4)^2)/(2*gamma*t*(t - 2)*(t - n))
    k6 = -(t - n - 1)*((t - n - 4)^2)/(gamma*t*(t - 2)*(t - n)*(t - n - 2))
  }
  if (implemented_type == "Q") {
    f1 = vector_Y1/(vector_Y1 + (n-1)/t)
    f2 = vector_Y2/(vector_Y2 + (n-1)/t)
    k1 = t*(t - n - 1)*(t - 2*n - 8)/(2*gamma*(t - 2)*(t - n - 2)*(t - n - 4))
    k2 = t*((t^2) - 3*n*t + 2*n*(n + 7) - 13*t + 24)/(2*gamma*(t - 2)*(t - n - 2)*(t - n - 3)*(t - n - 4))
    k3 = - t*(t - 4)*(t - n - 1)/(2*gamma*(t - 2)*(t - n - 2)*(t - n - 3)*(t - n - 4))
    k4 = (t - n)*(t - n - 3)/(gamma*(t - 2)*(t - n - 1))
    k5 = - (t - n)*((t - n - 3)^2)/(2*gamma*t*(t - 2)*(t - n - 1))
    k6 = - (t - n - 3)/(gamma*(t - 2)*(t - n - 2))
  }
  if (implemented_type == "M") {
    f1 = vector_Y1/(vector_Y1 + (n-1)/t)
    f2 = vector_Y2/(vector_Y2 + (n-1)/t)
    k1 = (t - n - 1)*(t - n - 4)/(2*gamma*(t - 2)*(t - n - 2))
    k2 = (t - n - 4)*(t - n - 5)/(2*gamma*(t - 2)*(t - n - 2)*(t - n - 3))
    k3 = - (t - 4)*(t - n - 1)*(t - n - 4)/(2*gamma*t*(t - 2)*(t - n - 2)*(t - n - 3))
    k4 = (t - n)*(t - n - 3)/(gamma*(t - 2)*(t - n - 1))
    k5 = - (t - n)*((t - n - 3)^2)/(2*gamma*t*(t - 2)*(t - n - 1))
    k6 = - (t - n - 3)*(t - n - 4)/(gamma*t*(t - 2)*(t - n - 2))
  }
  if (implemented_type == "QS") {
    f1 = vector_Y1/(vector_Y1 + (n-1)/t)
    f2 = vector_Y2/(vector_Y2 + (n-1)/t)
    k1 = (t - n - 1)*(t - n - 4)/(2*gamma*(t - 2)*(t - n - 2))
    k2 = (t - n - 4)*(t - n - 5)/(2*gamma*(t - 2)*(t - n - 2)*(t - n - 3))
    k3 = - (t - 4)*(t - n - 1)*(t - n - 4)/(2*gamma*t*(t - 2)*(t - n - 2)*(t - n - 3))
    k4 = (t - n)*(t - n - 5)*(t - n - 7)/(gamma*t*(t - 2)*(t - n - 1))
    k5 = - (t - n)*((t - n - 5)^2)*((t - n - 7)^2)/(2*gamma*(t^3)*(t - 2)*(t - n - 1))
    k6 = - (t - n - 4)*(t - n - 5)*(t - n - 7)/(gamma*(t^2)*(t - 2)*(t - n - 2))
  }
  if (implemented_type == "QSa") {
    f1 = vector_Y1/(vector_Y1 + (n/t))
    f2 = vector_Y2/(vector_Y2 + (n/t))
    k1 = (t - n - 1)*(t - n - 4)/(2*gamma*(t - 2)*(t - n - 2))
    k2 = (t - n - 4)*(t - n - 5)/(2*gamma*(t - 2)*(t - n - 2)*(t - n - 3))
    k3 = - (t - 4)*(t - n - 1)*(t - n - 4)/(2*gamma*t*(t - 2)*(t - n - 2)*(t - n - 3))
    k4 = (t - n)*(t - n - 5)*(t - n - 7)/(gamma*t*(t - 2)*(t - n - 1))
    k5 = - (t - n)*((t - n - 5)^2)*((t - n - 7)^2)/(2*gamma*(t^3)*(t - 2)*(t - n - 1))
    k6 = - (t - n - 4)*(t - n - 5)*(t - n - 7)/(gamma*(t^2)*(t - 2)*(t - n - 2))
  }
  VG = k1 * sqr_shrp_G + k2 * sqr_shrp_H + k3
  VH = k4 * sqr_shrp_H * mean(f1) + k5 * mean((f2^2)*vector_Y2)
  VGH = k6 * mean(f2*vector_Y2)
  Total = VG + VH + VGH
  table_results <- cbind(Total, VG, VH, VGH)
  colnames(table_results) <- c("Total", "Fund1", "Fund2", "Interaction")
  return(t(table_results))
}

out_of_sample_performance_theoretical <- function(n, t, sigma, mu, gamma, implemented_type){
  table1_result_G <- table1_portfolio_result_function(sigma, mu, gamma, "G")
  weight_G <- table1_result_G[[1]]
  mu_G <- mu %*% weight_G
  sqr_sigma_G <- t(weight_G)%*%sigma%*%weight_G
  sqr_shrp_G <- table1_result_G[[2]]
  
  table1_result_H <- table1_portfolio_result_function(sigma, mu, gamma, "H")
  weight_H <- table1_result_H[[1]]
  sqr_shrp_H <- table1_result_H[[2]]

  table1_result_S <- table1_portfolio_result_function(sigma, mu, gamma, "S")
  weight_S <- table1_result_S[[1]]
  sqr_shrp_S <- table1_result_S[[2]]
  
  if (implemented_type == "KZ") {
    factor <- (1-((n/t)/(sqr_shrp_S+(n/t)*(sqr_shrp_S/sqr_shrp_H))))*(sqr_shrp_S/(2*gamma))
    utility <- ((t - n - 1)*(t - n - 4)/((t - 2)*(t - n - 2)))*factor
  }
  if (implemented_type == "Y") {
    term1 <- ((t - n - 1)/(t - 2))*(sqr_shrp_G/(2*gamma))
    term2 <- ((t - n - 1)*(t - n - 4)/((t - 2)*(t - n - 2)))*(sqr_shrp_H/(sqr_shrp_H+(n/t)+(2*sqr_shrp_H/(t-n-2))))*(sqr_shrp_H/(2*gamma))
    utility <- term1 + term2
  }
  if (implemented_type == "Q") {
    term1 <- ((t - n - 1)/(t - 2))*(sqr_shrp_G/(2*gamma)) 
    term2 <- ((t - n)*(t - n - 3)/((t - 2)*(t - n - 1)))*(sqr_shrp_H/(sqr_shrp_H+((n-1)/t)))*(sqr_shrp_H/(2*gamma))
    utility <- term1 + term2
  }
  if (implemented_type == "M") {
    term1 <- ((t - n - 1)*(t - n - 4)/((t - 2)*(t - n - 2)))*(sqr_shrp_G/(2*gamma)) 
    term2 <- ((t - n)*(t - n - 3)/((t - 2)*(t - n - 1)))*(sqr_shrp_H/(sqr_shrp_H+((n-1)/t)))*(sqr_shrp_H/(2*gamma))
    utility <- term1 + term2
  }
  return(utility)
}

performance_table_function <- function(df, type_table, n_obs, gamma){
  df <- df %>% na.omit() %>% tail(n_obs) # use equal number of observations 
  row.names(df) <- NULL  # reset index
  n <- length(df)
  t <- length(df[,1])
  sigma <- cov(df)
  mu <- df %>% colMeans() %>% as.vector()
  gamma <- gamma
  if (type_table == 'implementable') {
    results_implementable_KZ <- out_of_sample_performance_implementable_function(n, t, sigma, mu, gamma, "KZ")*100
    results_implementable_Q <- out_of_sample_performance_implementable_function(n, t, sigma, mu, gamma, "Q")*100
    results_implementable_M <- out_of_sample_performance_implementable_function(n, t, sigma, mu, gamma, "M")*100
    results_implementable_QS <- out_of_sample_performance_implementable_function(n, t, sigma, mu, gamma, "QS")*100
    results_implementable_QSa <- out_of_sample_performance_implementable_function(n, t, sigma, mu, gamma, "QSa")*100 
    ## joint results ---------------------------------------------------------------
    results_implementable <- cbind(results_implementable_KZ, 
                                        results_implementable_Q, 
                                        results_implementable_M, 
                                        results_implementable_QS,
                                        results_implementable_QSa)
    colnames(results_implementable) <- c("KZ","Q","M","QS","QSa")
    return(results_implementable)
  }
  if (type_table == 'theoretical') {
    results_theoretical_KZ <- out_of_sample_performance_theoretical(n, t, sigma, mu, gamma, "KZ")*100
    results_theoretical_Y <- out_of_sample_performance_theoretical(n, t, sigma, mu, gamma, "Y")*100
    results_theoretical_Q <- out_of_sample_performance_theoretical(n, t, sigma, mu, gamma, "Q")*100
    results_theoretical_M <- out_of_sample_performance_theoretical(n, t, sigma, mu, gamma, "M")*100
    ## joint results ---------------------------------------------------------------
    table_results_theoretical <- cbind(results_theoretical_KZ,
                                      results_theoretical_Y,
                                      results_theoretical_Q, 
                                      results_theoretical_M)
    colnames(table_results_theoretical) <- c("KZ","Y","Q","M")
    rownames(table_results_theoretical) <- c("Total")
    return(table_results_theoretical)
  }
}

copulas_test_function <- function(df,n_obs ,n_sim, n_rep){
  df_copulas <- df %>% select(-date) 
  df_copulas_pseudo <- pobs(df_copulas)
  ncol <- ncol(df_copulas_pseudo)
  fitcopula_1 <- fitCopula(tCopula(dim=ncol,dispstr='un'), df_copulas_pseudo,
                              method = 'ml')
  summary(fitcopula_1)
  
  # Simulations
  set.seed(420)
  list_sim <- rep(n_sim,n_rep)
  list_sim <- lapply(list_sim, rCopula, fitcopula_1@copula) %>%
    lapply(as.data.frame)
  list_result <- lapply(list_sim, performance_table_function, 'implementable', n_obs, gamma = 1)
  
  KZ = c()
  Q = c()
  M = c()
  QS = c()
  QSa = c()
  
  for (i in 1:100) {
    KZ[i] =  list_result[[i]][1,1]
    Q[i] = list_result[[i]][1,2]
    M[i] = list_result[[i]][1,3]
    QS[i] = list_result[[i]][1,4]
    QSa[i] = list_result[[i]][1,5]
  }
  df_simulation <- cbind(KZ,Q,M,QS,QSa) %>% as.data.frame()
  df_simulation_colmeans <- df_simulation %>% colMeans()
  ttest <- t.test(KZ, QS, data = df_simulation, paired = TRUE, alternative = 'less')
  wilcoxtest <- wilcox.test(KZ, QS, data = df_simulation, paired = TRUE, alternative = 'less')
  list_result_final <- list(df_simulation,df_simulation_colmeans,ttest,wilcoxtest)
  return(list_result_final)
}

sensitivity_function <- function(df, vector_sensitivity){
  df_sensitivity <- df_1 %>% select(-date) 
  list_result <- lapply(list_sim, performance_table_function, df = df_sensitivity, 
                        type_table = 'implementable', n_obs = 100)
  
  KZ = c()
  Q = c()
  M = c()
  QS = c()
  QSa = c()
  
  for (i in 1:length(vector_sensitivity)) {
    KZ[i] =  list_result[[i]][1,1]
    Q[i] = list_result[[i]][1,2]
    M[i] = list_result[[i]][1,3]
    QS[i] = list_result[[i]][1,4]
    QSa[i] = list_result[[i]][1,5]
  }
  df_sensitivity_final <- cbind(KZ,Q,M,QS,QSa) %>% as.data.frame()
  return(df_sensitivity_final)
}

portfolio_returns_function <- function(asset_list, returns_tibble, initial_date){
  
  port_returns_tibble <- returns_tibble %>% 
    select(date,
           all_of(asset_list)) %>% 
    filter(between(date,initial_date + days(1),initial_date + years(1))) %>% 
    mutate(port_ret = rowMeans(across(!date),
                               na.rm = T)) %>% 
    select(date, port_ret)
  
  return(port_returns_tibble)
  
}

hist_plot_function <- function(stock, tibble_data, i_date = NULL, title = NULL,
                               bin = 20, normal = T, axis_display = T){
  
  if (axis_display){
    
    gg_axis <- labs(x = 'Return', y = 'Density')
    
  } else {
    
    gg_axis <- labs(x = NULL, y = NULL)
    
  }
  
  if (is.null(i_date)){
    
    i_date <- min(tibble_data$date)
    
  }
  
  if (is.null(title)){
    
    title_str <- stock
    
  } else {
    
    title_str <- title
    
  }
  
  histg_data <- tibble_data %>% 
    filter(date >= i_date)
  
  stck_mean <- histg_data %>% 
    select(!!as.name(stock)) %>% 
    unlist() %>% 
    mean()
  
  stck_sd <- histg_data %>% 
    select(!!as.name(stock)) %>% 
    unlist() %>% 
    sd()
  
  if(normal == T){
    
    normal_curve_fit <- stat_function(fun = function(x) 
      dnorm(x, mean = stck_mean, sd = stck_sd)/100,
      alpha = 0.8 , size = 1, color = "#004c6d")
    
  } else {
    
    normal_curve_fit <- NULL
    
  }
  
  
  hist_graph <- histg_data %>% #SPX Histogram
    ggplot(aes(!!as.name(stock))) + 
    geom_histogram(aes(y = (..density..)/100), 
                   fill = "#3e7d9c", color = "#004c6d",
                   bins = bin,
                   alpha = 0.8) +
    geom_density(aes(y = (..density..)/100), 
                 alpha=.6, fill="#a4eaff", color = "#70b2cd") + 
    normal_curve_fit +
    scale_y_continuous(labels = percent) +
    scale_x_continuous(labels = percent) +
    gg_axis +
    #coord_flip() +
    theme_minimal() +
    #theme(text = element_text(family = 'Segoe UI')) +
    labs(title = title_str) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),)
  
  return(hist_graph)
  print(hist_graph)
  
}
