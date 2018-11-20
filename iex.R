library(tidyverse)

tickers <- c("aapl")
endpoints <- c("chart", "dividends")

url_endpoint <- function(endpoint, ticker, time_window){
  url <- paste0("https://api.iextrading.com/1.0/stock/", ticker, "/", endpoint, "/", time_window)
}


df_iex_hist <- function(endpoint, ticker, time_window = "5y"){
  resp <- httr::GET(url_endpoint(endpoint, ticker, time_window))
  resp_parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
  return(data.frame(resp_parsed))
}

returns_divd_adjd <- function(df_chart, df_divd){
  df_divd %>%
    rename(date = exDate) %>%
    right_join(df_chart, by = "date") %>%
    select(date, close, amount)
}

returns_divd_adjd(df_iex_hist(endpoints[1], tickers[1]), df_iex_hist(endpoints[2], tickers[1]))

str(df_iex_hist(endpoints[1], tickers[1]))

str(df_iex_hist(endpoints[2], tickers[1]))


