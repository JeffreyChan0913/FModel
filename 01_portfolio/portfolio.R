rm( list = ls(all.names=T) )

options(warn=-1)

try( dev.off(dev.list()["RStudioGD"]), silent=TRUE )

setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

required.pkgs <- c( "quantmod", "reticulate", "dplyr", "tidyverse",
                   
                    "foreach", "doParallel", "lubridate", "matrixStats",
                   
                    "scattermore", "ggplot2", "MASS", "gridExtra"
                  )

pacman::p_load( required.pkgs, character.only=T )

source( "portfolio_mc.R" )

source( "data_processor.R" )

source( "plotting.R" )

tickers  <- c( "NVDA", "AMD", "TSM", "ASML", "MU", "AMAT", 
              
               "GOOG", "AAPL", "META", "TSLA", "DUK",
               
               "GLD", "X", "PG", "TLT","CVS", "VGIT", "GOVT"
              
              )

t.tickers <- c( "NVDA", "AMD", "TSM", "ASML", "MU", "AMAT", "GOOG", "AAPL", "META", "TSLA", "DUK" )

hedge <- c( "GLD", "X", "PG", "TLT","CVS", "VGIT", "GOVT" )


baseline.tickers              <- c( "SPY", "^GSPC" )

startDate                     <- as.Date( "2015-01-01" )

endDate                       <- as.Date( "2020-01-01" )

stock.data( c( tickers, baseline.tickers ), update=F )

data.package                  <- process.data( tickers )

stock.stats.annaual.ret       <- data.package[[1]]

stock.annual.cov              <- data.package[[2]]

rownames( stock.annual.cov )  <- tickers

colnames( stock.annual.cov )  <- tickers

system.time(
  
  result.mc     <- parallel.monte.carlo.portfolio( stock.stats.annaual.ret$mu, 
                                                   
                                                   stock.annual.cov, 
                                                   
                                                   length( tickers ),
                                                   
                                                   iteration = 10000000,
                                                   
                                                   n.cores = 10
                                                   
                                                )
  
)

frontier.data                   <- process.frontier.data( result.mc )

market.ret.D                    <- get.current.return( baseline.tickers, startDate, endDate )

spy.ret <- mean(market.ret.D[ , 1] ) * 252

spy.sigma <- var(market.ret.D[ , 1] ) * sqrt( 252 )

sp500.ret <- mean( market.ret.D[ , 2] ) * 252

sp500.sigma <- var( market.ret.D[ , 2] ) * sqrt( 252 )

plot.frontier( frontier.data )

ggsave( "frontier.png", width=8, height=5, dpi=320, bg="white" )

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

ggsave( "ret.png", width=8, height=5, dpi=320, bg="white" )

plot.w( portfolio.weights , tickers )

ggsave( "w.png", width=8, height=5, dpi=320, bg="white" )

options( scipen = 6 )

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

HHI                                           <- sum( portfolio.weights*portfolio.weights )

portfolio.result[["HHI"]]                     <- HHI

plot.ret.VaR( as.data.frame( daily.ret ), VaR.cutoff.95, VaR.cutoff.99 )

ggsave("var.png", width=8, height=5, dpi=320, bg="white" )

plot.ret.CVaR( as.data.frame( daily.ret ), VaR.cutoff.95, portfolio.result$CVaR[1,1], 0.05 )

ggsave( "cvar95.png", width=8, height=5, dpi=320, bg="white" )

plot.ret.CVaR( as.data.frame( daily.ret ), VaR.cutoff.99, portfolio.result$CVaR[2,1], 0.01 )

ggsave( "cvar99.png", width=8, height=5, dpi=320, bg="white" )

lapply( portfolio.result, round, 6 )


################################
######## stock plotting ########
################################

df <- list()

for( ticker in tickers ){
  
  k <- read.csv(paste0("data/", ticker, ".csv"))
  
  k$Date <- as.Date(k$Date)
  
  k <- subset( k, Date >= startDate & Date < endDate )
  
  df[[ticker]] <- k$Adj.Close
  
}

df          <- as.data.frame( do.call( cbind, df ) )

df.h        <- df[ , hedge]

df.t        <- df[ , t.tickers]

df$Date     <- k$Date

df.t$Date   <- k$Date

df.h$Date   <- k$Date

df.transform.h           <- pivot_longer( df.h, 
                                        
                                        col=-Date,
                                                 
                                        names_to="Hedging Ticker",
                                                 
                                        values_to="Price"
                                                 
                                      )

df.transform.t           <- pivot_longer( df.t, 
                                          
                                          col=-Date,
                                          
                                          names_to="Ticker",
                                          
                                          values_to="Price"
                                          
                                        )

fontsize <- 12

sth <-   theme_minimal( ) +
  
  theme( axis.title.x=element_text( size=fontsize ), 
         
         axis.title.y=element_text( size=fontsize ),
         
         plot.title=element_text( size=fontsize ),
         
         axis.text=element_text( size=fontsize ), 
         
         legend.title=element_text( size=12 ),
         
         legend.text=element_text( size=12 ),
         
         legend.key.size=unit( 0.8,"cm" ), 
         
         legend.position="right"
         
  )


ggplot( df.transform.t, aes( x=Date, y=Price, color=Ticker ) ) +
  
  ggtitle( "Tech Stock Price" ) +
  
  geom_line( size=0.8 ) +
  
  sth

ggsave( "Techstockprice.png", width=8, height=5, dpi=320, bg="white" )

ggplot( df.transform.h, aes( x=Date, y=Price, color=`Hedging Ticker` ) ) +
  
  ggtitle( "Stock Price for Hedging" ) +
  
  geom_line( size= 0.8 ) +
  
  sth

ggsave( "Techstockprice2.png", width=8, height=5, dpi=320, bg="white" )
