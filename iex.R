library(tidyverse)
library(quadprog)
library(plm)
library(meboot)

tickers <- c("emb", "agg", "vxus", "vti")
target.return <- 0.14
jboot <- 99

ConstructUrlEndpoint <- function(endpoint, ticker, time.window){
  url <- paste0("https://api.iextrading.com/1.0/stock/", ticker, "/", endpoint, "/", time.window)
}


GetIexHist <- function(endpoint, ticker, time.window = "5y"){
  on.exit(Sys.sleep(0.2))
  resp <- httr::GET(ConstructUrlEndpoint(endpoint, ticker, time.window))
  resp.parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
  return(data.frame(resp.parsed))
}

CalcDividentAdjustedReturns <- function(iex.chart, iex.divd){
  iex.divd %>%
    rename(date = exDate) %>%
    group_by(date) %>%
    summarise(amount = min(amount)) %>%
    ungroup() %>%
    right_join(iex.chart, by = "date") %>%
    transmute(amount = replace_na(amount, 0), date, close)  %>%
    mutate(returns = (close + amount)/dplyr::lag(close) - 1) %>%
    select(date, returns)
}

GetMergeAllTickerReturns <- function(tickers){
  lapply(tickers, function(ticker){
    CalcDividentAdjustedReturns(iex.chart = GetIexHist("chart", ticker),
        iex.divd = GetIexHist("dividends", ticker)) %>%
      mutate(ticker = ticker)
  }) %>%
  do.call(rbind, .) %>%
  filter(complete.cases(.))
}

CompleteCasesPanel <- function(df.panel){
  df.panel %>%
    spread(ticker, returns) %>%
    filter(complete.cases(.)) %>%
    gather(ticker, returns, -date)
}

FlattenReturnsDfMatrix <- function(df){
  df %>%
  spread(ticker, returns) %>%
  select(-date) %>%
  as.matrix(.)
}

CalcMeanCovarianceMatrix <- function(df.panel){
  returns.matrix <- df.panel %>% FlattenReturnsDfMatrix(.) 
  mean.returns <- returns.matrix %>% apply(., 2, mean)
  cov.returns <- ((nrow(returns.matrix)-1)^(-1)) * t(returns.matrix - mean.returns) %*% (returns.matrix - mean.returns)
  return(list(mean.returns = mean.returns, cov.returns = cov.returns))
}

ReformatResampledReturns <- function(returns.ens, i){
  returns.ens %>%
    select(i) %>%
    bind_cols(df.returns.panel %>% select(-returns)) %>%
    rename(returns = 1) 
}  

SignifFloor <- function(x, n){
  pow <- floor( log10( abs(x) ) ) + 1 - n
  y <- floor(x / 10 ^ pow) * 10^pow
  y[x==0] <- 0
  y
}

EfficientPortfolio <- function(er, covmat, target.return = NULL){
  N <- length(er)
  if (max(er) >= 0){
    if (is.null(target.return)){
      target.return.bounded <- min(er)
    } else {
      target.return.bounded <- min(max(min(er), target.return), max(er)) %>%
        SignifFloor(2)

    }
  } else if (max(er) < 0){
    target.return.bounded <- max(er) %>%
      SignifFloor(2)

  }
  Dmat <- covmat
  dvec <- rep.int(0, N)
  Amat <- cbind(rep(1,N), er, diag(1,N))
  bvec <- c(1, target.return.bounded, rep(0,N))
  result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
  w <- round(result$solution, 3)
  names(w) <- names(er)
  return(w)
}

ConvertAnnualToDailyTargetReturn <- function(annual.ret){
  daily.ret <- (1+annual.ret)^(1/252) - 1
  return(daily.ret)
}

df.long <- GetMergeAllTickerReturns(tickers)
df.returns.panel <- pdata.frame(df.long, index = c("ticker", "date"))
returns.ens <- meboot(x = df.returns.panel, reps = jboot, colsubj = 3, coldata = 2)

w <- lapply(seq(returns.ens), function(i){
  mean.covariance <- returns.ens %>%
    ReformatResampledReturns(., i) %>%
    CompleteCasesPanel(.) %>%
    CalcMeanCovarianceMatrix(.)
  er <- mean.covariance[[1]]
  covmat <- mean.covariance[[2]]
  w <- EfficientPortfolio(er, covmat, ConvertAnnualToDailyTargetReturn(target.return))
  return(w)
}) %>%
  Reduce('+', .)/jboot 


mean_covariance <- df.returns.panel %>%
  CompleteCasesPanel() %>%
  CalcMeanCovarianceMatrix()
er <- mean_covariance[[1]]
covmat <- mean_covariance[[2]]
w
crossprod(er, w)
w %*% covmat %*% w %>% sqrt()
covmat
w %*% covmat %*% w

q()

mean_covariance <- df_returns_panel %>%
  df_returns_complete_panel() %>%
  calc_mean_covariance_matrix()

er <- mean_covariance[[1]]




w <- efficient_portfolio(er, covmat, mean(er))

w
crossprod(er, w)
w %*% covmat %*% w

er
covmat

#returns.ens <- meboot(x = df_panel, reps = jboot, colsubj = 3, coldata = 2)

#str(returns.ens)



