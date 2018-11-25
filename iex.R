library(tidyverse)

tickers <- c("emb", "vug")
endpoints <- c("chart", "dividends")

url_endpoint <- function(endpoint, ticker, time_window){
  url <- paste0("https://api.iextrading.com/1.0/stock/", ticker, "/", endpoint, "/", time_window)
}


df_iex_hist <- function(endpoint, ticker, time_window = "5y"){
  on.exit(Sys.sleep(0.2))
  resp <- httr::GET(url_endpoint(endpoint, ticker, time_window))
  resp_parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
  return(data.frame(resp_parsed))
}

returns_divd_adjd <- function(df_chart, df_divd){
  df_divd %>%
    rename(date = exDate) %>%
    right_join(df_chart, by = "date") %>%
    transmute(amount = replace_na(amount,0), date, close) %>%
    mutate(returns = (close + amount)/lag(close) - 1) %>%
    select(date, returns)
}

df_returns_all <- function(tickers){
  lapply(tickers, function(ticker){
    returns_divd_adjd(df_iex_hist(endpoints[1], ticker), df_iex_hist(endpoints[2], ticker)) %>%
      mutate(ticker = ticker)
  }) %>%
  do.call(rbind, .)
}

df_returns_all(tickers)



