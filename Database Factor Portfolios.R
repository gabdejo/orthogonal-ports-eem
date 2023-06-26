# Packages and Functions####

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
source('Functions.R')


# Data import and wrangling ####
tickers_df <- read_xlsx('Tickers.xlsx')

#Import 10 Year financials_df Data
price_to_book_df <- read_xlsx('PB Ratio 10Y.xlsx')
market_cap_df <- read_xlsx('Market Cap 10Y.xlsx')


#Wrangling and processing
book_to_market_df <- price_to_book_df %>%
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
  mutate(across(.cols = !Name, .fns = ~ 1/.x)) %>% # From P/B to B/M
  pivot_longer(!Name, names_to = 'q_date', values_to = 'btm') %>% # Long format
  mutate(q_date = as.Date(q_date, '%m/%d/%y'))
  
market_cap_df <- market_cap_df %>% # Same pipeline as BtM
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


tickers_df <- tickers_df %>% 
  slice(-1) %>% # Drop EEM row total ETF
  rename(Country = `Country (P)`) %>% 
  mutate(Ticker = paste(Ticker,'Equity')) %>% 
  select(Name, Ticker, Country)


# Consolidate financial ratios
financials_df <- inner_join(book_to_market_df,
                         market_cap_df,
                         by = c('Name','q_date')) %>% 
  inner_join(tickers_df, by = c('Name')) %>% 
  select(Ticker, Country, q_date, btm, mc) %>% 
  group_by(Ticker) %>% 
  arrange(q_date,.by_group = T) %>% 
  ungroup()

# Bucket process
factor_ports_df <- financials_df %>% 
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

common_tickers_df <- inner_join(book_to_market_df,market_cap_df,by = c('Name','q_date')) %>% 
  select(Name) %>% 
  distinct() %>% 
  inner_join(tickers_df) %>%
  #mutate(Ticker = paste(Ticker,'Equity')) %>% 
  distinct(Ticker) %>% 
  select(Ticker)


# Prices Extraction BBLG ####
blpConnect()

lista_tickers_list <- common_tickers_df %>% pull(Ticker)

initial_date <- '2013-01-01'
final_date <- '2022-12-31'

precios_list <- bdh(lista_tickers_list, 
               c('PX_LAST'), 
               start.date = initial_date %>% as.Date(),
               end.date = final_date %>% as.Date(),
               options = c('currency'='USD'))

precios_df <- bind_rows(precios, .id = 'ticker')

write.csv(x = precios_df,
          file = 'precios diarios 10Y.csv',
          row.names = F)

# Import Prices CSV ####
prices_long_df <- read.csv('precios diarios 10Y.csv')

prices_df <- prices_long_df %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_wider(names_from = ticker, values_from = PX_LAST) %>% 
  arrange(date) %>% 
  fill(!date)

log_returns_df <- prices_df %>% 
  mutate(across(!date, ~ log(.x) - lag(log(.x))))

# Finish factor portfolios
factor_ports_df <- factor_ports_df %>% 
  rowwise() %>% 
  mutate(port_returns = portfolio_returns_function(group_tickers,
                                                   log_returns_df,
                                                   q_date) %>% list()) %>% 
  select(risk_factor, port_returns, group_tickers,mc_group,btm_group)

# Factor portfolios returns
factor_port_returns_df <- factor_ports_df %>% 
  unnest(port_returns) %>% 
  select(-group_tickers)

factor_port_returns_wide_df <- factor_port_returns_df %>% 
  select(-c(mc_group,btm_group)) %>% 
  pivot_wider(names_from = risk_factor, values_from = port_ret)

factor_port_returns_wide_df %>% # ***WARNING*** : SAVE IN CSV
  write.csv(file = 'factor portfolios.csv',
            row.names = F)

factor_returns_df <- factor_port_returns_wide_df

# Comparison with Kenneth DB ####
factor_returns_df <- read.csv('factor portfolios.csv') %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

initial_date <- min(factor_returns_df$date)
final_date <- max(factor_returns_df$date)

tbill13w_df <- tq_get('^IRX',
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

kenneth_returns_df <- read.csv('kenneth em factor portfolios.csv') %>% 
  as_tibble() %>% 
  setNames(c('date','small_growth','small_neutral','small_value',
             'big_growth','big_neutral','big_value')) %>% 
  mutate(date = as.character(date),
         date = as.yearmon(date, '%Y%m'),
         date = as.Date(date, frac = 1),
         across(!date,~ (.x)/100)) %>% 
  filter(between(date,min(factor_returns_df$date),max(factor_returns_df$date)))


factor_month_returns_df <- factor_returns_df %>% 
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

factor_month_excess_returns_df <- factor_month_returns_df %>% 
  left_join(tbill13w_df,
            by = 'date') %>% 
  mutate(ret = ret - rf/12) %>% 
  select(-rf)

kenneth_excess_returns_df <- kenneth_returns_df %>% 
  left_join(tbill13w_df,
            by = 'date') %>% 
  mutate(across(!c(date,rf)) - rf/12) %>% 
  select(-rf)

factor_month_excess_returns_df <- factor_month_excess_returns_df %>% 
  pivot_wider(names_from = risk_factor,
              values_from = ret)

#Export month excess returns ***WARNING***
factor_month_excess_returns_df %>% 
  write.csv('factor portfolios excess mensual.csv',
            row.names = F)

kenneth_excess_returns_df %>% 
  write.csv('kenneth em factor excess mensual.csv',
            row.names = F)

#Import excess returns
factor_month_excess_returns_df <- read.csv('factor portfolios excess mensual.csv') %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

kenneth_excess_returns_df <- read.csv('kenneth em factor excess mensual.csv') %>% 
  as_tibble() %>% 
  mutate(date = as.Date(date))

# Copulas ####
factor_returns_consolidate_df <- rbind(
  kenneth_excess_returns_df %>% 
    pivot_longer(!date, names_to = 'risk_factor',values_to = 'ret') %>% 
    mutate(base = 'Kenneth'),
  factor_month_excess_returns_df %>% 
    pivot_longer(!date, names_to = 'risk_factor',values_to = 'ret') %>% 
    mutate(base = 'Propia')
)

factor_returns_consolidate_nest_df <- factor_returns_consolidate_df %>% 
  group_by(base, risk_factor) %>% 
  arrange(date, .by_group = T) %>% 
  nest() %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(shapiro = shapiro.test(data$ret)$p.value,
         jb = jarque.bera.test(data$ret)$p.value) %>% 
  ungroup()

factor_returns_consolidate_nest_df %>% 
  select(-data) %>% 
  select(base, risk_factor, shapiro) %>%
  mutate(risk_factor = str_replace(risk_factor,'_',' ')) %>% 
  setNames(c('Base', 'Portafolio', 'p-value')) %>% 
  as.data.frame() %>% 
  stargazer::stargazer(type = 'html',
            out = 'shapiro.html',
            summary = F, digits = 4,
            rownames = F)


copula_fit_df <- factor_returns_consolidate_nest_df %>% 
  rowwise() %>% 
  mutate(u = pobs(data$ret) %>% list()) %>% 
  ungroup() %>% 
  group_by(base) %>% 
  summarise(port_u = bind_cols(u) %>% as.matrix() %>%  list()) %>% 
  crossing(copula_spec = c('normalCopula','tCopula')) %>% 
  rowwise() %>% 
  mutate(fit_copula = fitCopula(do.call(copula_spec,list(dim = 6)),port_u) %>% list(),
         aic = AIC(fit_copula),
         logl = logLik(fit_copula) %>% as.double())

copula_fit_df %>% 
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
