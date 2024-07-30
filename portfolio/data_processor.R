download.data <- function( 
    
  vector.of.tickers
  
){
  
  use_virtualenv("~/ml", required = T)
  
  prefix        <- Sys.getenv("STATS417")
  
  folder        <- file.path( prefix, "project" )
  
  crawl.tickers <- paste0( paste( vector.of.tickers, collapse=" " ) )
  
  command       <- sprintf("~/ml/bin/python3 %s/crawlerv1.py %s", folder, crawl.tickers)
  
  system(command) 
  
}


stock.data <- function(
    
  tickers,
  
  update = F,
  
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
  
  endDate   = as.Date("2020-01-01")
  
) {
  
  source("plotting.R")
  
  n         <- length( tickers )
  
  mu        <- numeric(n)
  
  stock.var <- numeric(n)
  
  cov.data  <- list()
  
  for( i in 1:n ){
    
    ticker            <- tickers[i]
    
    ticker.file.path  <- file.path( folder, paste( ticker, "csv", sep="." ) )
    
    ticker.data       <- read.csv( ticker.file.path )
    
    ticker.data$Date  <- as.Date( ticker.data$Date )
    
    ticker.data       <- subset( ticker.data,
                                 
                                 Date >= startDate & Date < endDate 
                                 
    )
    
    n                 <- nrow(ticker.data)
    
    ticker.data$ret   <- c( NA, 
                            
                            ticker.data[ 2:n, "Adj.Close"] / ticker.data[ 1:( n - 1 ), "Adj.Close" ] - 1
    )
    
    cov.data[[i]]     <- ticker.data$ret
    
    ticker.mu         <- mean( ticker.data$ret, na.rm=T ) * 252
    
    ticker.var        <- var( ticker.data$ret, na.rm=T ) * 252
    
    mu[i]             <- ticker.mu
    
    stock.var[i]      <- ticker.var
    
  }
  
  cov.data            <- do.call( cbind, cov.data )
  
  df                  <- data.frame( "mu"=mu,"var"=stock.var )
  
  rownames(df)        <- tickers #Y return
  
  cov.data            <- as.matrix( cov.data, row.names=tickers )
  
  cov.data            <- cov( cov.data[-1, ] ) * 252 # D cov
  
  return.package      <- list( df, cov.data )
  
  return( return.package )
  
}

process.frontier.data <- function(
    
  x
  
) {
  
  frontier.data             <- x[ c( "sigma", "ret", "sharpe.ratio" ) ]
  
  frontier.data$ret         <- frontier.data$ret
  
  frontier.data$sigma       <- frontier.data$sigma
  
  frontier.data             <- frontier.data[ order( frontier.data$sigma ), ]
  
  frontier.data             <- frontier.data[ !duplicated( frontier.data$sigma, 
                                                           fromLast = T), ]
  
  return ( frontier.data )
  
}

future_return <- function(
    
  tickers,
  
  start.Date = "2020-01-01",
  
  portfolio.w,
  
  folder           = "data",
  
  baseline.tickers = c("SPY", "^GSPC")
  
) {
  
  start.Date  <- as.Date( start.Date )
  
  all.tickers   <- c( tickers, baseline.tickers )
  
  n             <- length( all.tickers )
  
  daily.ret     <- list( ) 
  
  date.col      <- c()
  
  for( i in 1:n ) {
    
    ticker              <- all.tickers[i]
    
    ticker.file.path    <- file.path( folder, paste( ticker, "csv", sep="." ) )
    
    ticker.data         <- read.csv( ticker.file.path )
    
    ticker.data$Date    <- as.Date( ticker.data$Date )
    
    ticker.data         <- subset( ticker.data, Date >= start.Date )
    
    n                   <- nrow(ticker.data)
    
    ticker.data$d.ret   <- c( NA,
                              
                              ticker.data[ 2:n, "Adj.Close"] / ticker.data[ 1:( n - 1 ), "Adj.Close" ] - 1
                              
    )
    
    daily.ret[[ticker]] <- ticker.data[ -1 , c( "d.ret" ) ]
    
    date.col            <- ticker.data$Date
    
  }
  
  all.ret                   <- as.data.frame( do.call( cbind, daily.ret ) )
  
  colnames( all.ret )       <- all.tickers
  
  return.package            <- as.matrix( all.ret[ , tickers ] )
  
  return.package            <- return.package %*% t(portfolio.w)
  
  date.col                  <- as.Date( date.col[-1] )
  
  return.package            <- data.frame( Date = date.col,
                                           
                                           portfolio.return = return.package,
                                           
                                           all.ret[ , baseline.tickers]
                                           
  )
  
  colnames(return.package)  <- c("Date", "portfolio.return", "SPY", "S&P500")
  
  return( return.package )
  
}

cum.ret <- function(
    
  x
  
) {
  
  return ( cumprod( 1 + x ) - 1 )
  
}

get.current.return <- function(
    
  tickers,
  
  start.date,
  
  end.date,
  
  outside.root = F,
  
  folder = "data"
  
) {
  
  if( outside.root ) {
    
    folder <- "../portfolio/data"
    
  }
  
  performance.result <- list( )
  
  for ( ticker in tickers ) {
    
    tikcer.data                   <- file.path( folder, paste0( ticker, ".csv" ) )
    
    data                          <- read.csv( tikcer.data )
    
    data$Date                     <- as.Date( data$Date )
    
    data                          <- subset( data, Date >= start.date & Date < end.date )
    
    n                             <- nrow( data )
    
    performance.result[[ticker]]  <- data[ 2:n, "Adj.Close"] / data[ 1:( n - 1 ), "Adj.Close" ] - 1
    
  }
  
  performance.result              <- as.matrix( do.call( cbind, performance.result ) )

  return( performance.result )
  
}

get.performance.metric.helpper <- function(
    
  x,
  
  w
  
) {
  
  x              <- sweep( x, MARGIN=2, w, "*" )  
  
  x              <- x + 1
  
  result         <- apply( x, 1, prod ) - 1
  
  return( result )
  
}

get.performance.metric <- function(

  tickers,
  
  start.date,
  
  end.date,
  
  w,
  
  tickers.baseline,
  
  folder    = "data",
  
  risk.free = 0.0159
  
) {
  
  daily.return                    <- get.current.return( tickers, start.date, end.date )
  
  baseline.return.D               <- get.current.return( tickers.baseline, start.date, end.date )
  
  daily.return                    <- get.performance.metric.helpper( daily.return, w )
  
  daily.return.mu                 <- mean( daily.return, na.rm=T )
  
  daily.return.sigma              <- var( daily.return, na.rm=T )
  
  annual.ret                      <- daily.return.mu * 252
  
  annual.sigma                    <- daily.return.sigma * sqrt( 252 )
  
  ret                             <- cbind( daily.return , baseline.return.D )
  
  cov.portfolio.baseline.D        <- cov( ret )
  
  cov.portfolio.baseline.Y        <- cov( ret ) * 252
  
  baseline.var.D                  <- diag( cov.portfolio.baseline.D )[ 2 : 3 ]

  baseline.var.Y                  <- diag( cov.portfolio.baseline.Y )[ 2 : 3 ]

  SPY.beta.D                      <- cov.portfolio.baseline.D[ 1 , 2 ] / baseline.var.D[1]

  SP500.beta.D                    <- cov.portfolio.baseline.D[ 1 , 3 ] / baseline.var.D[2]

  SPY.beta.Y                      <- cov.portfolio.baseline.Y[ 1 , 2 ] / baseline.var.Y[1]

  SP500.beta.Y                    <- cov.portfolio.baseline.Y[ 1 , 3 ] / baseline.var.Y[2]

  spy.ret.D                       <- baseline.return.D[ , 1]

  sp500.ret.D                     <- baseline.return.D[ , 2]

  spy.ret.Y                       <- mean( baseline.return.D[ , 1], na.rm=T ) * 252

  SP500.ret.Y                     <- mean( baseline.return.D[ , 2], na.rm=T ) * 252

  treynor.spy.Y                   <- ( annual.ret - risk.free ) / SPY.beta.Y
  
  treynor.spy.base.Y              <- ( spy.ret.Y - risk.free )
  
  treynor.sp500.Y                 <- ( annual.ret - risk.free ) / SP500.beta.Y
  
  treynor.sp500.base.Y            <- ( SP500.ret.Y - risk.free )

  Jensen.alpha.spy.Y              <- ( annual.ret - ( risk.free + ( SPY.beta.Y*( spy.ret.Y - risk.free ) ) ) )

  jensen.alpha.sp500.Y            <- ( annual.ret - ( risk.free + ( SP500.beta.Y*( SP500.ret.Y - risk.free ) ) ) )

  Beta.SP500.Y                    <- cbind( SP500.beta.Y, treynor.sp500.Y, treynor.sp500.base.Y, jensen.alpha.sp500.Y )

  Beta.SPY.Y                      <- cbind( SPY.beta.Y, treynor.spy.Y, treynor.spy.base.Y, Jensen.alpha.spy.Y )

  Beta.Y                          <- rbind( Beta.SPY.Y, Beta.SP500.Y )

  rownames( Beta.Y )              <- c( "SPY", "SP500" )

  colnames( Beta.Y )              <- c( "Market", "Treynor", "Treyno (M)", "Jensen Alpha" )

  alpha                           <- c( 0.95, 0.99 )

  daily.return                    <- sort( as.vector( daily.return ) )

  `VaR Daily`                     <- quantile( daily.return, 1 - alpha )

  `VaR Yearly`                    <- `VaR Daily` * sqrt( 252 )

  `CVaR Daily`                    <- c( mean( daily.return[ daily.return <= `VaR Daily`[1] ] ),

                                        mean( daily.return[ daily.return <= `VaR Daily`[2] ] )

                                      )

  `CVaR Yearly`                   <- `CVaR Daily` * sqrt( 252 )
  
  alpha                           <- c( 0.05, 0.01 )
  
  z                               <- qnorm( alpha )

  phi                             <- dnorm( z )
  
  parametric.VaR.D                <- daily.return.mu + ( z * daily.return.sigma )

  parametric.VaR.Y                <- annual.ret + ( z * annual.sigma )

  parametric.CVaR.D               <- daily.return.mu - ( phi / ( 1 - alpha ) ) * daily.return.sigma

  parametric.CVaR.Y               <- annual.ret - ( phi / ( 1 - alpha ) * annual.sigma ) 

  VaR.table                       <- t( rbind( `VaR Daily`, `VaR Yearly`, -parametric.VaR.D, -parametric.VaR.Y ) )

  colnames( VaR.table )           <- c( "Historical Daily VaR", "Historical Annual VaR",


                                        "Parametric Daily VaR", "Parametric Annual VaR"

                                       )

  rownames( VaR.table )           <- c( "95%", "99%" )

  CVaR.table                      <- t( rbind( `CVaR Daily`, `CVaR Yearly`,

                                               -parametric.CVaR.D, -parametric.CVaR.Y

                                              )
                                      )

  colnames( CVaR.table )          <- c( "Historical Daily CVaR", "Historical Annually CVaR",

                                        "Parametric Daily CVaR", "Parametric Annually CVaR"

                                      )

  rownames(CVaR.table)            <- c("95%", "99%")

  performance.metric              <- list( )

  performance.metric[["VaR"]]     <- VaR.table

  performance.metric[["CVaR"]]    <- CVaR.table

  performance.metric[["Annual Betas"]]   <- Beta.Y

  return( performance.metric )
  
}
