#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
options(scipen=99)

library(tidyverse)
library(quadprog)
library(plm)
library(meboot)

if (length(args) == 0){
  tickers <- c("emb", "agg", "vxus", "vti")
} else {
  tickers <- args
}
target.return <- 0.10
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

ReformatResampledReturns <- function(returns.ens, df.returns.panel, i){
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

Main <- function(tickers, jboot, target.return){

  df.returns.panel <- GetMergeAllTickerReturns(tickers) %>%
      pdata.frame(., index = c("ticker", "date"))
  returns.ens <- meboot(x = df.returns.panel, reps = jboot, colsubj = 3, coldata = 2)

  w <- lapply(seq(returns.ens), function(i){
    cat("Proccessing sample: ", i, "\n")
    mean.covariance <- returns.ens %>%
      ReformatResampledReturns(., df.returns.panel, i) %>%
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

  # Print output
  
  "Efficient portfolio weights:\n" %>% cat()
  w %>% print()
  cat("\n\n")
  
  crossprod(er, w) %>% sprintf("Efficient portfolio expected daily return: \n%f\n\n", .) %>% cat()
  
  (1 + crossprod(er, w))^(252) %>% sprintf("Efficient portfolio expected annual return: \n%f\n\n", .) %>% cat()
  
  w %*% covmat %*% w %>% sqrt() %>% sprintf("Efficient portfolio return standard deviation: \n%f\n\n", .) %>% cat()

  "Asset covariance matrix:\n" %>% cat()
  covmat %>% print()
  cat("\n\n")

  
  w %*% covmat %*% w %>% sprintf("Efficient portfolio return variance: \n%f\n\n", .) %>% cat()
}

Main(tickers, jboot, target.return)

