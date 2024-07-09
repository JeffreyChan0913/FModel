setwd(dirname(rstudioapi::getSourceEditorContext()$path))

required.pkgs <- c("quantmod", "reticulate", "dplyr")
pacman::p_load(required.pkgs, character.only=T)

download.data <- function(vector.of.tickers){
  folder <- file.path(Sys.getenv("STATS417"), "project")
  use_virtualenv("~/ml", required = T)
  crawl.tickers <-paste0(paste(vector.of.tickers, collapse=" "))
  command <- sprintf("~/ml/bin/python3 %s/crawler.py %s", folder, crawl.tickers)
  system(command) 
}

# output weight, return, variance, sharpe ratio
monte.carlo.portfolio <- function(
    mu.ret, # n by 1
    portfolio.cov, # n by n 
    n,
    iteration = 1000000
) {
  risk.free.rate  <- 0.05375
  result          <- matrix(NA, nrow=iteration, ncol=n+3)
  for(i in 1:iteration){
    w           <- runif(n,0,1000)
    w           <- w / sum(w)
    ret         <- w %*% mu.ret
    vol         <- t(w) %*% portfolio.cov %*% w
    result[i, ] <- c(w, ret, sqrt(vol), (ret - risk.free.rate) / vol)
  }
  colnames(result) <- c(paste0("w",1:n), "ret", "sigma", "sharperatio")
  return (result)
}

stock.data <- function(
    tickers,
    update=F,
    folder = "data/"
) {
  if(!dir.exists(folder) || update){
    dir.create(folder)
    download.data(tickers)
    return("Done")
  }
  get.tickers <- c()
  for(ticker in tickers){
    file <- paste(ticker,"csv", sep='.')
    if(!file.exists(file.path(folder, file))){
      get.tickers <- c(get.tickers, ticker)
    }
  }
  if(length(get.tickers) > 0){
    download.data(get.tickers)
  }
}

process.data <- function(
    tickers,
    folder = "data"
) {
  n         <- length(tickers)
  mu        <- c()
  var       <- c()
  cov.data  <- c()
  for(ticker in tickers){
    ticker.data       <- read.csv(file.path(folder, paste(ticker, "csv", sep=".")))
    ticker.data$Date  <- as.Date(ticker.data$Date)
    ticker.data       <- subset(ticker.data, Date >= as.Date("2015-01-01"), Date < as.Date("2024-01-01"))
    cov.data          <- cbind(cov.data, ticker.data$ret)
    mu                <- c(mu, mean(ticker.data$ret, na.rm=T))
    var               <- c(var, var(ticker.data$ret, na.rm=T))
  }
  return.package <- list(data.frame("mu"=mu,"var"=var, row.names=tickers), as.matrix(cov.data))
  return(return.package)
}

# tickers <- c("NVDA", "AMD", "TSM", "ASML", "EIX","DUK","AAPL", "TSLA", "CVS","SCI", "UNH","CSV")
tickers <- c("NVDA", "AMD", "TSM", "ASML", "EIX","DUK","AAPL", "TSLA", "CVS","SCI")
stock.data(tickers, update=T)
data.package            <- process.data(tickers)
stock.stats             <- data.package[[1]]
stock.cov               <- cov(data.package[[2]])
row.names(stock.stats)  <- tickers
row.names(stock.cov)    <- tickers
colnames(stock.cov)     <- tickers
result.mc               <- monte.carlo.portfolio(stock.stats$mu, stock.cov, length(tickers))
plot(result.mc[,"sigma"], result.mc[, "ret"])
