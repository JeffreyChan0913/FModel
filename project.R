setwd(dirname(rstudioapi::getSourceEditorContext()$path))

required.pkgs <- c("quantmod", "reticulate", "dplyr", "tidyverse")

pacman::p_load(required.pkgs, character.only=T)

download.data <- function( vector.of.tickers ){
  
  use_virtualenv("~/ml", required = T)
  
  prefix        <- Sys.getenv("STATS417")
  
  folder        <- file.path( prefix, "project" )
  
  crawl.tickers <- paste0( paste( vector.of.tickers, collapse=" " ) )
  
  command       <- sprintf("~/ml/bin/python3 %s/crawler.py %s", folder, crawl.tickers)
  
  system(command) 
  
}

monte.carlo.portfolio <- function(
    
    mu.ret,
    
    portfolio.cov,
    
    n,
    
    iteration = 1000000
    
) {
  
  risk.free.rate  <- 0.0428
  
  result          <- matrix( NA, nrow=iteration, ncol=n+3 )
  
  for(i in 1:iteration){
    
    w           <- runif( n, 0, 1000 )
    
    w           <- w / sum( w )
    
    ret         <- w %*% mu.ret
    
    vol         <- t( w ) %*% portfolio.cov %*% w
    
    result[i, ] <- c( w, ret, sqrt( vol ), ( ret - risk.free.rate ) / vol )
    
  }
  
  colnames( result ) <- c( paste0( "w",1:n ), "ret", "sigma", "sharperatio" )
  
  result             <- as.data.frame( result )
  return ( result )
  
}

stock.data <- function(
    
    tickers,
    
    update=F,
    
    folder = "data/"
    
) {
  
  if( !dir.exists( folder ) || update ){
    
    dir.create( folder )
    
    download.data( tickers )
    
    return( "Done" )
    
  }
  
  get.tickers <- c()
  
  for( ticker in tickers ) {
    
    file      <- paste( ticker,"csv", sep='.' )
    
    ticker.file.path <- file.path( folder, file )
    
    if(!file.exists( ticker.file.path ) ) {
      
      get.tickers <- c( get.tickers, ticker )
      
    }
    
  }
  
  if( length(get.tickers) > 0 ){
    
    download.data( get.tickers )
    
  }
  
}

process.data <- function(
    
    tickers,
    
    folder    = "data",
    
    startDate = as.Date("2015-01-01"),
    
    endDate   = as.Date("2024-01-01")
    
) {
  
  n         <- length( tickers )
  
  mu        <- numeric(n)
  
  var       <- numeric(n)
  
  cov.data  <- list()
  
  for( i in 1:n ){
    
    ticker            <- tickers[i]
    
    ticker.file.path  <- file.path( folder, paste( ticker, "csv", sep="." ) )
    
    ticker.data       <- read.csv( ticker.file.path )
    
    ticker.data$Date  <- as.Date( ticker.data$Date)
    
    ticker.data       <- subset( ticker.data,
                                 Date >= startDate,
                                 Date < endDate )
    
    cov.data[[i]]     <- ticker.data$ret
    
    ticker.mu         <- mean( ticker.data$ret, na.rm=T ) * 252
    
    ticker.var        <- var( ticker.data$ret, na.rm=T ) * 252
    
    mu[i]             <- ticker.mu
    
    var[i]            <- ticker.var
    
  }
  
  cov.data        <- do.call( cbind, cov.data )
  
  df              <- data.frame( "mu"=mu,"var"=var, row.names=tickers )
  
  cov.data        <- as.matrix( cov.data )
  
  return.package  <- list( df, cov.data )
  
  return( return.package )
  
}

# tickers <- c("NVDA", "AMD", "TSM", "ASML", "EIX","DUK","AAPL", "TSLA", "CVS","SCI", "UNH","CSV")
tickers <- c("NVDA", "AMD", "TSM", "ASML", "EIX", "DUK","AAPL", "TSLA", "CVS", "MU")

stock.data( tickers, update=T )

data.package              <- process.data(tickers)

stock.stats               <- data.package[[1]]

stock.cov                 <- cov(data.package[[2]])

row.names( stock.stats )  <- tickers

row.names( stock.cov )    <- tickers

colnames( stock.cov )     <- tickers

result.mc                 <- monte.carlo.portfolio( stock.stats$mu, 
                                                    stock.cov, 
                                                    length( tickers )
                                                   )

frontier.data             <- result.mc[ c("sigma", "ret") ]

frontier.data$ret         <- frontier.data$ret * 100

frontier.data             <- frontier.data[ order( frontier.data$sigma ), ]

frontier.data             <- frontier.data[ !duplicated( frontier.data$sigma, 
                                                         fromLast = T), ]

ggplot( frontier.data, aes( x=sigma, y=ret ) ) +
  
  geom_point( color="steelblue", alpha=0.3) + 
  
  labs( x="Portfolio Standard Deviation", y="Portfolio Return") +
  
  theme_minimal( )
