rm( list = ls(all.names=T) )

options(warn=-1)

try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

required.pkgs <- c( "quantmod", "reticulate", "dplyr", "tidyverse",
                   
                    "foreach", "doParallel", "lubridate", "matrixStats",
                   
                    "scattermore", "ggplot2"
                  )

pacman::p_load(required.pkgs, character.only=T)

source( "portfolio_mc.R" )

source( "data_processor.R" )

source( "plotting.R" )

tickers  <- c( "NVDA", "AMD", "TSM", "ASML", "MU", "AMAT", 
              
               "GOOG", "AAPL", "META", "TSLA", "DUK",
               
               "GLD", "X", "PG", "TLT","CVS", "VGIT", "GOVT"
              
              )


baseline.tickers              <- c("SPY", "^GSPC")

startDate                     <- as.Date("2015-01-01")

endDate                       <- as.Date("2020-01-01")

stock.data( c( tickers, baseline.tickers ), update=F )

data.package                  <- process.data(tickers)

stock.stats.annaual.ret       <- data.package[[1]]

stock.annual.cov              <- data.package[[2]]

rownames( stock.annual.cov )  <- tickers

colnames( stock.annual.cov )  <- tickers

system.time(
  
  result.mc     <- parallel.monte.carlo.portfolio( stock.stats.annaual.ret$mu, 
                                                   
                                                   stock.annual.cov, 
                                                   
                                                   length( tickers ),
                                                   
                                                   iteration = 1000,
                                                   
                                                   n.cores = 10
                                                   
  )
  
)

frontier.data                   <- process.frontier.data( result.mc )

market.ret.D                    <- get.current.return( baseline.tickers, startDate, endDate )

spy.ret <- mean(market.ret.D[ , 1] ) * 252

spy.sigma <- var(market.ret.D[ , 1] ) * sqrt( 252 )

sp500.ret <- mean( market.ret.D[ , 2] ) * 252

sp500.sigma <- var( market.ret.D[ , 2] ) * sqrt( 252 )


plot.frontier( frontier.data, spy.ret, spy.sigma, sp500.ret, sp500.sigma )

portfolio.weights               <- result.mc[ which.max( result.mc$sharpe.ratio ), ]

portfolio.ret                   <- result.mc[ which.max( result.mc$sharpe.ratio ), "ret"]

portfolio.weights               <- portfolio.weights[ 1:length(tickers) ]


colnames(portfolio.weights)     <- tickers

portfolio.weights               <- as.matrix( portfolio.weights )

future.package                  <- future_return( tickers, endDate,
                                                 
                                                  portfolio.w = portfolio.weights,
                                                 
                                                  baseline.tickers = baseline.tickers
                                                 
)

plot.cum.ret( future.package, tickers )

plot.w( t(portfolio.weights) , tickers )

options(scipen = 6)

portfolio.performance <- get.performance.metric( tickers, startDate, endDate,

                                                 portfolio.weights, baseline.tickers

                                               )

portfolio.result                              <- portfolio.performance

rownames( portfolio.weights )                 <- NULL

portfolio.result[['Weights']]                 <- portfolio.weights

portfolio.result[['Port. Expected Return']]   <- result.mc[ which.max( result.mc$sharpe.ratio ), "ret"]

portfolio.result[["Port. Volatility"]]        <- result.mc[ which.max( result.mc$sharpe.ratio ), "sigma"]

portfolio.result[["Port. Sharpe Ratio"]]      <- result.mc[ which.max( result.mc$sharpe.ratio ), "sharpe.ratio"]

daily.ret                                     <- get.current.return( tickers, startDate, endDate )

daily.ret                                     <- get.performance.metric.helpper( daily.ret, 
                                                                                 
                                                                                 portfolio.weights 
                                                                                 
                                                                                )

VaR.cutoff.95                                 <- portfolio.result[["VaR"]][1,1]

VaR.cutoff.99                                 <- portfolio.result[["VaR"]][2,1]

plot.ret.VaR( as.data.frame( daily.ret ), VaR.cutoff.95, VaR.cutoff.99 )

plot.ret.CVaR( as.data.frame( daily.ret ), VaR.cutoff.95, 0.05 )

plot.ret.CVaR( as.data.frame( daily.ret ), VaR.cutoff.99, 0.01 )

lapply( portfolio.result, round, 6 )
