# Packs, funcs and WD ####

# Packages
library(tidyverse)
library(tidyquant)
library(scales)
library(readxl)
library(grid)
library(gridExtra)
library(timetk)
library(ggridges)
library(copula)
library(tseries)
library(Rblpapi)


# Functions 
performance_metrics <- function(tibble_data){
  
  r <- tibble_data %>% 
    tk_xts(select = !date,
           date_var = date)
  
  anual_periods <- 12
  
  mean_ret <- colMeans(r, na.rm = T) * anual_periods
  volat <- sqrt(diag(cov(r, use = 'complete.obs')) * anual_periods)
  gam <- skewness(r)
  kap <- kurtosis(r)
  sharpe <- mean_ret/volat
  omeg <- Omega(r)
  maxdd <- maxDrawdown(r)
  
  anual_metrics <- rbind(mean_ret, volat, gam, kap, sharpe, omeg, maxdd)
  rownames(anual_metrics) <- c('Mean','Volatility','Skewness','Kurtosis',
                               'Sharpe','Omega','Max_Drawdown')
  
  return(anual_metrics)
  
}
portfolio_returns <- function(asset_list, returns_tibble, initial_date){
  
  port_returns_tibble <- returns_tibble %>% 
    select(date,
           all_of(asset_list)) %>% 
    filter(between(date,initial_date + days(1),initial_date + years(1))) %>% 
    mutate(port_ret = rowMeans(across(!date),
                               na.rm = T)) %>% 
    select(date, port_ret)
  
  return(port_returns_tibble)
  
}


# Working directory
setwd("C:/Users/CRISTINA/OneDrive - Universidad del Pacífico/Universidad/Tesis IE2/Data")

# Data import and wrangling ####
tickers <- read_xlsx('Tickers.xlsx')

#10 Year Data
price_to_book <- read_xlsx('PB Ratio 10Y.xlsx')

market_cap <- read_xlsx('Market Cap 10Y.xlsx')


#Wrangling and processing
book_to_market <- price_to_book %>%
  select(Name, matches('06/../..') | matches('12/../..')) %>% # Jun and Dic
  rename_with(~ str_extract(.x,'../../..'), !Name) %>% 
  slice(-1) %>% # Remove EEM total row
  mutate(n_na = rowSums(is.na(across()))) %>% 
  filter(n_na < 20) %>% # Filter equities with all cols NA
  group_by(Name) %>% 
  #filter(n() > 1) %>% #To look which where dup
  arrange(n_na, .by_group = T) %>% # From least to most empty
  fill(!Name,.direction = 'up') %>% # Fill upwards
  slice(1) %>% # Keep the first row from each dup equity
  ungroup() %>% 
  select(-n_na) %>% # Drop variable, served its purpose
  mutate(across(.cols = !Name, .fns = ~ 1/.x)) %>% # From P/B to BtM
  pivot_longer(!Name, names_to = 'q_date', values_to = 'btm') %>% # Long format
  mutate(q_date = as.Date(q_date, '%m/%d/%y'))
  
market_cap <- market_cap %>% # Same pipeline as BtM
  select(Name, matches('06/../..') | matches('12/../..')) %>%
  rename_with(~ str_extract(.x,'../../..'), !Name) %>% 
  slice(-1) %>%
  mutate(n_na = rowSums(is.na(across()))) %>% 
  filter(n_na < 20) %>%  
  group_by(Name) %>% 
  #filter(n() > 1) %>% 
  arrange(n_na, .by_group = T) %>% 
  fill(!Name,.direction = 'up') %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-n_na) %>% 
  pivot_longer(!Name, names_to = 'q_date', values_to = 'mc') %>% 
  mutate(q_date = as.Date(q_date, '%m/%d/%y'))


tickers <- tickers %>% 
  slice(-1) %>% # Drop EEM row total ETF
  rename(Country = `Country (P)`) %>% 
  mutate(Ticker = paste(Ticker,'Equity')) %>% 
  select(Name, Ticker, Country)

tickers %>% # Country composition
  group_by(Country) %>% 
  summarise(count = n()) %>% 
  arrange(count %>% desc())

# Consolidate financial ratios
financials <- inner_join(book_to_market,
                         market_cap,
                         by = c('Name','q_date')) %>% 
  inner_join(tickers, by = c('Name')) %>% 
  select(Ticker, Country, q_date, btm, mc) %>% 
  group_by(Ticker) %>% 
  arrange(q_date,.by_group = T) %>% 
  ungroup()

financials %>% # 1220 firms survived the join
  select(Ticker) %>% 
  distinct()

# Bucket process 1 (ORIGINAL)
factor_ports <- financials %>% 
  #filter(!is.na(btm) & !is.na(mc)) %>% 
  group_by(Ticker) %>% 
  arrange(q_date, .by_group = T) %>% 
  mutate(lag_btm = lag(btm)) %>% # lag dic Book-to-Market to next year's june
  ungroup() %>% 
  group_by(q_date) %>% 
  filter(!is.na(mc) & !is.na(lag_btm) & (month(q_date) == 6)) %>% # Use if june and has info
  mutate(btm_rank = ntile(lag_btm,10), # Grouping with quantiles
         mc_rank = ntile(mc,10),
         btm_group = case_when(
           btm_rank <= 3 ~ 'growth',
           btm_rank > 7 ~ 'value',
           T ~ 'neutral'
         ),
         mc_group = case_when(
           mc_rank == 1 ~ 'small',
           mc_rank == 10 ~ 'big',
           T ~ 'other'
         )) %>% 
  filter(mc_group != 'other') %>% # Use only big and small, no medium cap
  select(Ticker,q_date,btm_group,mc_group, btm, mc) %>% 
  group_by(q_date,btm_group,mc_group) %>% 
  summarise(group_tickers = list(Ticker),
            mc_mean = mean(mc, na.rm = T),
            btm_mean = mean(btm, na.rm = T),
            .groups = 'keep') %>%
  ungroup() %>% 
  mutate(risk_factor = paste(mc_group,
                             btm_group,
                             sep = '_'))


# Buckets process 2 (REVISED)
financials_cuts <- financials %>% 
  group_by(Ticker) %>% 
  arrange(q_date, .by_group = T) %>% 
  mutate(lag_btm = lag(btm)) %>%
  ungroup() %>% 
  group_by(q_date) %>% 
  filter(!is.na(mc) & !is.na(lag_btm) & (month(q_date) == 6)) %>% 
  mutate(mc_lb = quantile(mc, 0.1),
         mc_ub = quantile(mc, 0.9)) %>% 
  filter(mc >= mc_ub) %>% 
  mutate(btm_lb = quantile(lag_btm, 0.3),
         btm_ub = quantile(lag_btm, 0.7)) %>% 
  select(q_date, mc_lb:btm_ub) %>%
  distinct() %>% 
  ungroup() %>% 
  arrange(q_date)

factor_ports <- financials %>% 
  group_by(Ticker) %>% 
  arrange(q_date, .by_group = T) %>% 
  mutate(lag_btm = lag(btm)) %>% # lag dic Book-to-Market to next year's june
  ungroup() %>% 
  group_by(q_date) %>% 
  filter(!is.na(mc) & !is.na(lag_btm) & (month(q_date) == 6)) %>% # Use if june and has info
  ungroup() %>% 
  inner_join(financials_cuts, by = 'q_date') %>% 
  group_by(q_date) %>% 
  mutate(btm_group = case_when(
           lag_btm <= btm_lb ~ 'growth',
           lag_btm >= btm_ub ~ 'value',
           T ~ 'neutral'
         ),
         mc_group = case_when(
           mc <= mc_lb ~ 'small',
           mc >= mc_ub ~ 'big',
           T ~ 'other'
         )) %>% 
  filter(mc_group != 'other') %>% # Use only big and small, no medium cap
  select(Ticker,q_date,btm_group,mc_group, btm, mc) %>% 
  ungroup() %>% 
  group_by(q_date,btm_group,mc_group) %>% 
  summarise(group_tickers = list(Ticker),
            mc_mean = mean(mc, na.rm = T),
            btm_mean = mean(btm, na.rm = T),
            .groups = 'keep') %>%
  ungroup() %>% 
  mutate(risk_factor = paste(mc_group,
                             btm_group,
                             sep = '_'))
  
# Factor ports analysis
factor_ports %>% # Market Cap ports mean evolution
  ggplot(aes(q_date, mc_mean, col = btm_group)) +
  facet_wrap(~ mc_group, scales = 'free_y') +
  geom_line() +
  theme_bw()

factor_ports %>% # BtM ports mean evolution
  ggplot(aes(q_date, btm_mean, col = mc_group)) +
  facet_wrap(~ btm_group, scales = 'free_y') +
  geom_line() +
  theme_bw()

factor_ports %>% # Market Cap mean by Date and Factor Port
  select(q_date,
         mc_mean,
         risk_factor) %>% 
  pivot_wider(names_from = risk_factor,
              values_from = mc_mean)

financials %>% # MC Histogram
  mutate(mc_p = ntile(mc,100),
         btm_p = ntile(btm,100)) %>% 
  filter(mc_p <= 95) %>% # Windsorizing
  ggplot(aes(x = mc)) +
  geom_histogram(aes(y = ..density..),
                 bins = 100, 
                 fill = 'sky blue',
                 col = 'white') +
  theme_light()

financials %>% #BtM Histogram
  mutate(mc_p = ntile(mc,100),
         btm_p = ntile(btm,100)) %>% 
  filter(btm_p <= 95) %>% # Windsor
  ggplot(aes(x = btm)) +
  geom_histogram(aes(y = ..density..),
                 bins = 100, 
                 fill = 'sky blue',
                 col = 'white') +
  theme_light()

# Next step: go to BBLG Port, Performance Tab, to download the prices/returns of
# each ticker

common_tickers <- inner_join(book_to_market,market_cap,by = c('Name','q_date')) %>% 
  select(Name) %>% 
  distinct() %>% 
  inner_join(tickers) %>%
  #mutate(Ticker = paste(Ticker,'Equity')) %>% 
  distinct(Ticker) %>% 
  select(Ticker)

#MC and BTM cuts along time (V1 ORIGINAL)
bucket_cuts <- financials %>% 
  #filter(!is.na(btm) & !is.na(mc)) %>% 
  group_by(Ticker) %>% 
  arrange(q_date, .by_group = T) %>% 
  mutate(lag_btm = lag(btm)) %>% 
  ungroup() %>% 
  group_by(q_date) %>% 
  filter(!is.na(mc) & !is.na(lag_btm) & (month(q_date) == 6)) %>% 
  mutate(btm_rank = ntile(lag_btm,10),
         mc_rank = ntile(mc,10),
         btm_growth = if_else(btm_rank == 3, lag_btm, NaN) %>% max(na.rm = T),
         btm_value = if_else(btm_rank == 8, lag_btm, NaN) %>% min(na.rm = T),
         mc_small = if_else(mc_rank == 1, mc, NaN) %>% max(na.rm = T),
         mc_big = if_else(mc_rank == 10, mc, NaN) %>% min(na.rm = T)) %>% 
  ungroup() %>% 
  arrange(q_date) %>% 
  select(q_date, btm_growth, btm_value, mc_small, mc_big) %>% 
  distinct() %>% 
  mutate(q_date = as.yearmon(q_date),
         q_date_btm = q_date - 0.5)

bucket_cuts_comparacion <- financials %>% 
  #filter(!is.na(btm) & !is.na(mc)) %>% 
  group_by(Ticker) %>% 
  arrange(q_date, .by_group = T) %>% 
  mutate(lag_btm = lag(btm)) %>% 
  ungroup() %>% 
  group_by(q_date) %>% 
  filter(!is.na(mc) & !is.na(lag_btm) & (month(q_date) == 6)) %>% 
  mutate(btm_rank = ntile(lag_btm,10),
         mc_rank = ntile(mc,10),
         btm_group = case_when(
           btm_rank <= 3 ~ 'growth',
           btm_rank > 7 ~ 'value',
           T ~ 'neutral'
         ),
         mc_group = case_when(
           mc_rank == 1 ~ 'small',
           mc_rank == 10 ~ 'big',
           T ~ 'other'
         )) %>% 
  filter(mc_group != 'other') %>%
  ungroup() %>% 
  group_by(q_date, mc_group, btm_group) %>% 
  summarise(mean_mc = mean(mc)) %>% 
  ungroup() %>% 
  arrange(q_date) %>% 
  mutate(bucket = paste0(mc_group,'_',btm_group)) %>% 
  select(q_date,bucket,mean_mc) %>% 
  #group_by(q_date) %>% 
  #summarise(tot = n())
  pivot_wider(names_from = bucket,
              values_from = mean_mc) %>% 
  select()

  

# Graphs for presentation ####
financials %>% # BTM Hist
  filter(month(q_date) == 12) %>% 
  group_by(q_date) %>% 
  mutate(mc_p = ntile(mc,100),
         btm_p = ntile(btm,100)) %>% 
  filter(btm_p < 99) %>% 
  mutate(q_date = q_date %>% 
           as.yearmon() %>% 
           as.factor()) %>% 
  ggplot(aes(btm, fill = q_date)) + 
  #geom_histogram(aes(y = (..density..)/100, color = as.factor(q_date)), 
                 #fill = "#3e7d9c", color = "#004c6d",
                 #bins = 100) +
  geom_density(aes(y = ..density..), 
               alpha=.2) + 
  scale_y_continuous(#labels = percent,
                     name = 'Density') +
  theme_minimal()

financials %>% # MC Hist
  filter(month(q_date) == 6) %>% 
  group_by(q_date) %>% 
  mutate(mc_p = ntile(mc,100),
         btm_p = ntile(btm,100)) %>% 
  filter(mc_p < 95) %>% 
  mutate(q_date = q_date %>% 
           as.yearmon() %>% 
           as.factor()) %>% 
  ggplot(aes(mc, fill = q_date)) + 
  #geom_histogram(aes(y = (..density..)/100, color = as.factor(q_date)), 
  #fill = "#3e7d9c", color = "#004c6d",
  #bins = 100) +
  geom_density(aes(y = ..density..), 
               alpha=.2) + 
  scale_y_continuous(labels = percent,
    name = 'Density') +
  theme_minimal()

financials %>% # MC 3D Hist
  filter(month(q_date) == 6) %>% 
  group_by(q_date) %>% 
  mutate(mc_p = ntile(mc,100),
         btm_p = ntile(btm,100)) %>% 
  filter(mc_p < 95) %>% 
  mutate(q_date = q_date %>% 
           as.yearmon() %>% 
           as.factor()) %>% 
  ggplot(aes(x = mc, y = q_date)) +
  stat_density_ridges(scale = 3,
                      rel_min_height = 0.001,
                      quantile_lines = TRUE, 
                      quantiles = c(0.1, 0.9), 
                      alpha = 0.6,
                      fill = "#BFDEEB") +
  #geom_point(aes(x = mc_small, y = q_date %>% as.factor(), group = 1), 
  #           data = bucket_cuts) +
  #geom_point(aes(x = mc_big, y = q_date %>% as.factor(), group = 1), 
  #           data = bucket_cuts) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color='dark grey')
  ) +
  labs(x = 'Market Cap',
       y = NULL,
       title = 'Evolución del Market Cap de acciones del EEM',
       subtitle = 'En miles de USD')

ggsave('evolucion MC.png',
       width = 650,
       height = 600,
       units = 'px',
       dpi = 110)

financials %>% # BTM 3D Hist
  filter(month(q_date) == 12) %>% 
  group_by(q_date) %>% 
  mutate(mc_p = ntile(mc,100),
         btm_p = ntile(btm,100)) %>% 
  filter(btm_p < 99) %>% 
  mutate(q_date = q_date %>% 
           as.yearmon() %>% 
           as.factor()) %>% 
  ggplot(aes(x = btm, y = q_date)) +
  #geom_vline(xintercept = 1) +
  #geom_density_ridges_gradient(scale = 3, 
  #                             rel_min_height = 0.001, 
  #                             gradient_lwd = 1.,
  #                             fill = "#BFDEEB",
                               #color = "#004c6d",
  #) +
  stat_density_ridges(scale = 3,
                      rel_min_height = 0.001,
                      quantile_lines = TRUE, 
                      quantiles = c(0.3, 0.7), 
                      alpha = 0.6,
                      fill = "#BFDEEB") +
  #geom_point(aes(x = btm_value, y = q_date_btm %>% as.factor(), group = 1), 
  #           data = bucket_cuts) +
  #geom_point(aes(x = btm_growth, y = q_date_btm %>% as.factor(), group = 1), 
  #           data = bucket_cuts) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size=.1, color='dark grey')
        ) +
  labs(x = 'Book-to-Market',
       y = NULL,
       title = 'Evolución del Book-to-Market de acciones del EEM',
       subtitle = '')

ggsave('evolucion BtM.png',
       width = 650,
       height = 600,
       units = 'px',
       dpi = 110)

financials %>% 
  #filter(!is.na(btm) & !is.na(mc)) %>% 
  group_by(Ticker) %>% 
  arrange(q_date, .by_group = T) %>% 
  mutate(lag_btm = lag(btm)) %>% 
  ungroup() %>% 
  group_by(q_date) %>% 
  filter(!is.na(mc) & !is.na(lag_btm) & (month(q_date) == 6)) %>% 
  mutate(btm_rank = ntile(lag_btm,10),
         mc_rank = ntile(mc,10),
         btm_group = case_when(
           btm_rank <= 3 ~ 'growth',
           btm_rank > 7 ~ 'value',
           T ~ 'neutral'
         ),
         mc_group = case_when(
           mc_rank == 1 ~ 'small',
           mc_rank == 10 ~ 'big',
           T ~ 'other'
         )) %>% 
  filter(q_date == '2014-06-30') %>% 
  mutate(lag_btm_p = ntile(lag_btm,100)) %>% 
  filter(lag_btm_p < 99) %>% 
  ggplot(aes(lag_btm)) +
  geom_histogram(aes(y = (..density..)), 
                 fill = "#3e7d9c", color = "#004c6d",
                 bins = 30) +
  geom_density(aes(y = (..density..)), 
               alpha=.6, fill="#a4eaff", color = "#70b2cd") + 
  geom_vline(xintercept = c(0.343,0.769), 
             col = 'red', 
             linetype = 'dashed',
             size = 0.9) +
  theme_light() +
  labs(x = 'Book-to-Market',
       y = 'Densidad')

ggsave('ejemplo cuantiles.png',
       width = 650,
       height = 600,
       units = 'px',
       dpi = 110)

# Prices Extracción BBLG ####
blpConnect()

lista_tickers <- common_tickers %>% pull(Ticker) #Debio ser con los tickers de Financials

initial_date <- '2013-01-01'
final_date <- '2022-12-31'

precios <- bdh(lista_tickers, 
               c("PX_LAST"), 
               start.date = initial_date %>% as.Date(),
               end.date = final_date %>% as.Date(),
               options = c("currency"="USD"))

precios_tabla <- bind_rows(precios, .id = 'ticker')

write.csv(x = precios_tabla,
          file = 'precios diarios 10Y.csv',
          row.names = F)

precios_tabla %>% dim()

# Importacion CSV, Returns y Graphs ####
prices_long <- read.csv('precios diarios 10Y.csv')

prices <- prices_long %>% 
  as_tibble() %>% 
  #filter(ticker %in% financials$Ticker) %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_wider(names_from = ticker, values_from = PX_LAST) %>% 
  arrange(date) %>% 
  fill(!date)

log_returns <- prices %>% 
  mutate(across(!date, ~ log(.x) - lag(log(.x))))

# Finish factor portfolios
factor_ports <- factor_ports %>% 
  rowwise() %>% 
  mutate(port_returns = portfolio_returns(group_tickers,
                                          log_returns,
                                          q_date) %>% list()) %>% 
  select(risk_factor, port_returns, group_tickers,mc_group,btm_group)

# Factor portfolios returns
factor_port_returns <- factor_ports %>% 
  unnest(port_returns) %>% 
  select(-group_tickers)

factor_port_returns %>% # Cumulative Returns value: big vs small
  filter(str_detect(risk_factor,'value')) %>% 
  group_by(risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(cum_ret = exp(cumsum(port_ret)) - 1) %>% 
  ungroup() %>% 
  ggplot(aes(date,cum_ret, col = risk_factor)) +
  geom_line(alpha = 0.8) +
  guides(colour = guide_legend(override.aes = list(size = 1.6))) +
  theme_light()

factor_port_returns %>% 
  group_by(risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(cum_ret = exp(cumsum(port_ret)) - 1) %>% 
  ungroup() %>% 
  ggplot(aes(date,cum_ret, col = mc_group)) +
  facet_wrap(~ btm_group %>% as.factor()) +
  geom_line(alpha = 0.8) +
  guides(colour = guide_legend(override.aes = list(size = 1.6))) +
  theme_light()

factor_port_returns %>% 
  group_by(risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(cum_ret = exp(cumsum(port_ret)) - 1) %>% 
  ungroup() %>% 
  ggplot(aes(date,cum_ret, col = btm_group)) +
  facet_wrap(~ mc_group %>% as.factor()) +
  geom_line(alpha = 0.8) +
  guides(colour = guide_legend(override.aes = list(size = 1.6))) +
  theme_light()

factor_port_returns_wide <- factor_port_returns %>% 
  select(-c(mc_group,btm_group)) %>% 
  pivot_wider(names_from = risk_factor, values_from = port_ret)

factor_port_returns_wide %>% # ***WARNING*** : SAVE IN CSV
  write.csv(file = 'factor portfolios.csv',
            row.names = F)

factor_returns <- factor_port_returns_wide

# Comparison with Kenneth DB ####
factor_returns <- read.csv('factor portfolios.csv') %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

initial_date <- min(factor_returns$date)
final_date <- max(factor_returns$date)

tbill13w <- tq_get('^IRX',
                   from = initial_date,
                   to = final_date) %>% 
  select(date, 
         rf = adjusted) %>% 
  mutate(periodo = as.yearmon(date)) %>% 
  group_by(periodo) %>% 
  arrange(date, .by_group = T) %>% 
  fill(rf) %>% 
  slice_tail(n = 1) %>% 
  mutate(date = as.Date(periodo, frac = 1),
         rf = rf/100) %>% 
  ungroup() %>% 
  select(-periodo)

kenneth_returns <- read.csv('kenneth em factor portfolios.csv') %>% 
  as_tibble() %>% 
  setNames(c('date','small_growth','small_neutral','small_value',
             'big_growth','big_neutral','big_value')) %>% 
  mutate(date = as.character(date),
         date = as.yearmon(date, '%Y%m'),
         date = as.Date(date, frac = 1),
         across(!date,~ (.x)/100)) %>% 
  filter(between(date,min(factor_returns$date),max(factor_returns$date)))


factor_month_returns <- factor_returns %>% 
  pivot_longer(cols=!date,
               names_to = 'risk_factor',
               values_to = 'logret') %>% 
  mutate(year_month = as.yearmon(date)) %>% 
  group_by(risk_factor, year_month) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(ret = cumsum(logret) %>% exp() - 1) %>% 
  slice_tail(n = 1) %>% 
  ungroup() %>% 
  mutate(date = as.Date(year_month, frac = 1)) %>% 
  select(-c(year_month,logret))

factor_month_excess_returns <- factor_month_returns %>% 
  left_join(tbill13w,
            by = 'date') %>% 
  mutate(ret = ret - rf/12) %>% 
  select(-rf)

factor_month_excess_returns %>% 
  pivot_wider(names_from = risk_factor,
              values_from = ret) %>% 
  performance_metrics()

kenneth_excess_returns <- kenneth_returns %>% 
  left_join(tbill13w, by = 'date') %>% 
  mutate(across(!c(date,rf)) - rf/12) %>% 
  select(-rf)
  
kenneth_excess_returns %>% 
  performance_metrics()

factor_month_excess_returns <- factor_month_excess_returns %>% 
  pivot_wider(names_from = risk_factor,
              values_from = ret)

#Export month excess returns ***WARNING***
factor_month_excess_returns %>% 
  write.csv('factor portfolios excess mensual.csv',
            row.names = F)

kenneth_excess_returns %>% 
  write.csv('kenneth em factor excess mensual.csv',
            row.names = F)

#Import excess returns
factor_month_excess_returns <- read.csv('factor portfolios excess mensual.csv') %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

kenneth_excess_returns <- read.csv('kenneth em factor excess mensual.csv') %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

#Graphs to compare
graph_propio <- factor_month_excess_returns %>% 
  pivot_longer(!date, names_to = 'risk_factor',values_to = 'ret') %>%
  rowwise() %>% 
  mutate(mc_group = str_split(risk_factor,'_')[[1]][1],
         btm_group = str_split(risk_factor,'_')[[1]][2]) %>% 
  ungroup() %>% 
  group_by(risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(cum_ret = cumprod(1+ret) - 1) %>% 
  ungroup() %>% 
  ggplot(aes(date,cum_ret, col = btm_group)) +
  facet_wrap(~ mc_group %>% as.factor()) +
  geom_line(alpha = 0.8) +
  guides(colour = guide_legend(override.aes = list(size = 1.6),
                               title = 'Book-to-Market')) +
  labs(y = 'Retorno acumulado',
       #title = 'Base propia',
       x = NULL) +
  scale_y_continuous(labels = percent) +
  theme_bw()

graph_kenneth <- kenneth_excess_returns %>% 
  pivot_longer(!date, names_to = 'risk_factor',values_to = 'ret') %>% 
  rowwise() %>% 
  mutate(mc_group = str_split(risk_factor,'_')[[1]][1],
         btm_group = str_split(risk_factor,'_')[[1]][2]) %>% 
  ungroup() %>% 
  group_by(risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(cum_ret = cumprod(1+ret) - 1) %>% 
  ungroup() %>% 
  ggplot(aes(date,cum_ret, col = btm_group)) +
  facet_wrap(~ mc_group %>% as.factor()) +
  geom_line(alpha = 0.8) +
  guides(colour = guide_legend(override.aes = list(size = 1.6),
                               title = 'Book-to-Market')) +
  labs(y = 'Retorno acumulado',
       #title = 'Base Kenneth',
       x = NULL) +
  scale_y_continuous(labels = percent) +
  theme_bw()

grid.arrange(graph_propio, graph_kenneth)

propia_kenneth <- rbind(
  kenneth_excess_returns %>% 
    pivot_longer(!date, names_to = 'risk_factor',values_to = 'ret') %>% 
    mutate(base = 'Kenneth'),
  factor_month_excess_returns %>% 
    pivot_longer(!date, names_to = 'risk_factor',values_to = 'ret') %>% 
    mutate(base = 'Propia')
  )

propia_kenneth %>% # PRINCIPAL GRAPH ###
  rowwise() %>% 
  mutate(mc_group = str_split(risk_factor,'_')[[1]][1],
         btm_group = str_split(risk_factor,'_')[[1]][2]) %>% 
  ungroup() %>% 
  group_by(base, risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  mutate(cum_ret = cumprod(1+ret) - 1) %>% 
  ungroup() %>% 
  mutate(base = factor(base, levels = c('Propia','Kenneth')),
         mc_group = factor(mc_group, levels = c('small','big'))) %>% 
  #filter(mc_group != 'small') %>% 
  ggplot(aes(date,cum_ret, col = btm_group)) +
  facet_grid(vars(base),vars(mc_group), scales = 'free') +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.4) +
  geom_line(alpha = 0.8) +
  guides(colour = guide_legend(override.aes = list(size = 1.6),
                               title = 'Book-to-Market')) +
  labs(y = 'Retorno acumulado',
       #title = 'Base Kenneth',
       x = NULL) +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('comparacion bases.png',
       width = 1500,
       height = 900,
       units = 'px',
       dpi = 190)
  
kenneth_excess_returns %>% 
  performance_metrics()

factor_month_excess_returns %>%
  select(c('date','small_growth','small_neutral','small_value',
           'big_growth','big_neutral','big_value')) %>% 
  performance_metrics()

# Copulas ####

hist_plot <- function(stock, tibble_data, i_date = NULL, title = NULL,
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


# Marginal distribution analysis

factor_month_excess_returns %>% 
  pivot_longer(!date, 
               values_to = 'log_return', 
               names_to = 'symbol') %>% 
  mutate(symbol = factor(symbol)) %>% 
  ggplot(aes(x = log_return, y = symbol)) +
  stat_density_ridges(scale = 3,
                      rel_min_height = 0.0001,
                      quantile_lines = TRUE, 
                      quantiles = c(0.1, 0.9), 
                      alpha = 0.4,
                      fill = "#BFDEEB") +
  labs(y = NULL) +
  theme_light()

kenneth_excess_returns %>% 
  pivot_longer(!date, 
               values_to = 'log_return', 
               names_to = 'symbol') %>% 
  mutate(symbol = factor(symbol)) %>% 
  ggplot(aes(x = log_return, y = symbol)) +
  stat_density_ridges(scale = 3,
                      rel_min_height = 0.0001,
                      quantile_lines = TRUE, 
                      quantiles = c(0.1, 0.9), 
                      alpha = 0.4,
                      fill = "#BFDEEB") +
  labs(y = NULL) +
  theme_light()

factor_month_excess_returns %>% 
  hist_plot('small_value',.)

factor_port_names <- factor_month_excess_returns %>% 
  select(!date) %>% 
  colnames()

propia_kenneth

# Marginales
factor_month_excess_returns %>% 
  lapply(factor_port_names, hist_plot,
         tibble_data = .,
         axis_display = F) %>% 
  arrangeGrob(grobs = ., layout_matrix = matrix(1:6, nrow = 3)) %>% 
  plot()

kenneth_excess_returns %>% 
  lapply(factor_port_names, hist_plot, 
         tibble_data = .,
         axis_display = F) %>%
  arrangeGrob(grobs = ., layout_matrix = matrix(1:6, nrow = 3)) %>% 
  plot()

# Normality tests
factor_month_excess_returns %>% 
  lapply(factor_port_names, pull, .data = .) %>% 
  setNames(factor_port_names) %>% 
  lapply(., shapiro.test)

kenneth_excess_returns %>% 
  lapply(factor_port_names, pull, .data = .) %>% 
  setNames(factor_port_names) %>% 
  lapply(., shapiro.test)


# Tidier
propia_kenneth_nest <- propia_kenneth %>% 
  group_by(base, risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  nest() %>% 
  ungroup()

propia_kenneth_nest <- propia_kenneth_nest %>% 
  rowwise() %>% 
  mutate(shapiro = shapiro.test(data$ret)$p.value,
         jb = jarque.bera.test(data$ret)$p.value) %>% 
  ungroup()

propia_kenneth_nest %>% 
  select(-data) %>% 
  select(base, risk_factor, shapiro) %>%
  mutate(risk_factor = str_replace(risk_factor,'_',' ')) %>% 
  setNames(c('Base', 'Portafolio', 'p-value')) %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type = 'html',
            out = 'shapiro.html',
            summary = F, digits = 4,
            rownames = F)


for (base_name in unique(propia_kenneth$base)){
  
  propia_kenneth_nest %>% 
    filter(base == base_name) %>% 
    rowwise() %>% 
    mutate(graph = hist_plot('ret',
                             data,
                             title = str_replace(risk_factor,'_',' '), 
                             axis_display = F) %>% list()) %>% 
    pull(graph) %>% 
    arrangeGrob(grobs=.,
                layout_matrix = matrix(1:6, nrow = 3),
                top = textGrob(base_name)) %>% 
    #plot()
    ggsave(plot = .,
           file = paste0('marginales ',base_name,'.png'),
           width = 1500,
           height = 900,
           units = 'px',
           dpi = 190)
  
}

# Simulations to Understand
gaus_cop <- matrix(rep(NA, 5000 * 2), ncol = 2)

mu <- c(0,0)
sigma <- c(1.2,1.8)
ro <- -0.8
ro_mat <- matrix(c(1,ro,ro,1), nrow = 2)

varcov <- matrix(t(t(sigma)) %*% (t(sigma)) * ro_mat, nrow = 2)

for (i in 1:5000){
  
  #gaus_cop[i,] <- mu + rnorm(2) %*% chol(varcov)
  #gaus_cop[i, 1] <- mu[1] + rnorm(1) * sigma[1]
  #gaus_cop[i, 2] <- mu[2] + rt(1,4) * sigma[2]
  gaus_cop[i,] <- mu + rt(2,2) %*% chol(varcov)
  
}

plot(gaus_cop)

pobs(gaus_cop) %>% fitCopula(do.call('normalCopula',list(dim = 2)),.)
normalCopula()
rCopula(1000,tCopula(0.8)) %>%
  qt(df = 4) %>% 
  plot()

rCopula(1000,normalCopula(0.8)) %>% 
  qnorm() %>% 
  plot()

copula_riskport <- propia_kenneth_nest %>% 
  rowwise() %>% 
  mutate(u = pobs(data$ret) %>% list()) %>% 
  ungroup() %>% 
  group_by(base) %>% 
  summarise(port_u = bind_cols(u) %>% as.matrix() %>%  list()) %>% 
  crossing(copula_spec = c('normalCopula','tCopula',
                           'gumbelCopula','claytonCopula')) %>% 
  rowwise() %>% 
  mutate(fit_copula = fitCopula(do.call(copula_spec,list(dim = 6)),port_u) %>% list(),
         aic = AIC(fit_copula),
         logl = logLik(fit_copula) %>% as.double())

copula_riskport %>% 
  filter(copula_spec == 'tCopula') %>% 
  pull(fit_copula) %>% 
  lapply(summary)

copula_riskport %>% 
  filter((copula_spec == 'tCopula') | (copula_spec == 'normalCopula')) %>%
  mutate(copula_spec = recode(copula_spec, 
                              normalCopula = 'Gauss',
                              tCopula = 'T-Student'),
         coef_est = coef(fit_copula) %>% t() %>% as_tibble() %>% list()) %>%
  unnest(coef_est) %>% 
  select(base, copula_spec, df, aic) %>% 
  setNames(c('Base','Copula','GL','AIC')) %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type = 'html',
                       out = 'copulas.html',
                       summary = F, 
                       digits = 2,
                       rownames = F)

(copula_riskport %>% 
  pull(fit_copula))[[4]] %>% coef() %>% t() %>% as_tibble()

propia_kenneth_nest %>% 
  rowwise() %>% 
  mutate(data = data %>% tk_xts(silent = T) %>% list()) %>% 
  ungroup() %>% 
  group_by(base) %>% 
  summarise(data = bind_cols(data) %>% list()) %>% 
  rowwise() %>% 
  mutate(corr_mat = cor(data) %>% list()) %>% 
  pull(corr_mat)

# Graph Gamma Sensitivity
sens_gabo <- read.csv('df sensitivity gabo.csv') %>% 
  as_tibble() %>% 
  mutate(crra = c(seq(0.1,0.9,0.1),seq(1, 10, 1)),
         #qs_kz = QS - KZ
         )

sens_kenneth <- read.csv('df sensitivity kenneth.csv') %>% 
  as_tibble() %>% 
  mutate(crra = c(seq(0.1,0.9,0.1),seq(1, 10, 1)),
         #qs_kz = QS - KZ
         )

sens_gabo %>%
  mutate(base = 'MSCI EEM') %>% 
  rbind(sens_kenneth %>% mutate(base = 'Kenneth')) %>% 
  #select(-c(Q,M)) %>% 
  filter(between(crra,1,10)) %>% 
  pivot_longer(!c(crra,base), 
               names_to = 'Portafolio',
               values_to = 'utility') %>% 
  ggplot(aes(x = crra, y = utility, col = Portafolio)) +
  facet_wrap(~ base %>% factor(levels = c('MSCI EEM','Kenneth')), scales = 'free_y') +
  geom_line(size = 0.7, alpha = 0.8) +
  labs(y='Utilidad Esperada', x='Coeficiente de Aversión al Riesgo') +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 1.6)))

ggsave(file = paste0('sensibildad_crra.png'),
       width = 1800,
       height = 800,
       units = 'px',
       dpi = 190)


sens_gabo %>%
  relocate(crra) %>% 
  mutate(crra = as.integer(crra)) %>% 
  filter(between(crra,1,10)) %>%
  as.data.frame() %>% 
  stargazer(type = 'html',
            out = 'sens_gabo.html',
            summary = F, 
            digits = 3,
            rownames = F)
  
sens_kenneth %>%
  relocate(crra) %>% 
  mutate(crra = as.integer(crra)) %>% 
  filter(between(crra,1,10)) %>%
  as.data.frame() %>% 
  stargazer(type = 'html',
            out = 'sens_kenneth.html',
            summary = F, 
            digits = 3,
            rownames = F)
  