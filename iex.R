library(devtools)

chart <- httr::GET("https://api.iextrading.com/1.0/stock/aapl/chart/5y")
chart_parsed <- jsonlite::fromJSON(httr::content(chart, "text"))

chart_parsed

div <- httr::GET("https://api.iextrading.com/1.0/stock/aapl/dividends/5y")
div_parsed <- jsonlite::fromJSON(httr::content(div, "text"))

div_parsed

