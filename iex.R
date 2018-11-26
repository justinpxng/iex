library(tidyverse)
library(plm)
library(meboot)

tickers <- c("emb", "vug")
jboot <- 99

url_endpoint <- function(endpoint, ticker, time_window){
  url <- paste0("https://api.iextrading.com/1.0/stock/", ticker, "/", endpoint, "/", time_window)
}


df_iex_hist <- function(endpoint, ticker, time_window = "5y"){
  on.exit(Sys.sleep(0.2))
  resp <- httr::GET(url_endpoint(endpoint, ticker, time_window))
  resp_parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
  return(data.frame(resp_parsed))
}

returns_divd_adjd <- function(iex_chart, iex_divd){
  iex_divd %>%
    rename(date = exDate) %>%
    right_join(iex_chart, by = "date") %>%
    transmute(amount = replace_na(amount,0), date, close)  %>%
    mutate(returns = (close + amount)/dplyr::lag(close) - 1) %>%
    select(date, returns)
}

df_returns_all_tickers <- function(tickers){
  lapply(tickers, function(ticker){
    returns_divd_adjd(iex_chart = df_iex_hist("chart", ticker),
                      iex_divd = df_iex_hist("dividends", ticker)) %>%
      mutate(ticker = ticker)
  }) %>%
  do.call(rbind, .) %>%
  filter(complete.cases(.))
}

df_returns_complete_panel <- function(df_panel){
  df_panel %>%
    spread(ticker, returns) %>%
    filter(complete.cases(.)) %>%
    gather(ticker, returns, -date)
}

returns_flat_matrix <- function(df){
  df %>%
  spread(ticker, returns) %>%
  select(-date) %>%
  as.matrix(.)
}

calc_mean_covariance_matrix <- function(df_panel){
  returns_matrix <- df_panel %>% returns_flat_matrix(.) 
  mean_returns <- returns_matrix %>% apply(., 2, mean)
  cov_returns <- ((nrow(returns_matrix)-1)^(-1)) * t(returns_matrix - mean_returns) %*% (returns_matrix - mean_returns)
  return(list(mean_returns = mean_returns, cov_returns = cov_returns))
}

df_long <- df_returns_all_tickers(tickers)
df_returns_panel <- pdata.frame(df_long, index = c("ticker", "date"))

calc_mean_covariance_matrix(df_returns_panel %>%
  df_returns_complete_panel())



#returns.ens <- meboot(x = df_panel, reps = jboot, colsubj = 3, coldata = 2)

#str(returns.ens)



