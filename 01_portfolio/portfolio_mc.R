monte.carlo.portfolio <- function(
    
  mu.ret,
  
  portfolio.cov,
  
  n,
  
  iteration      = 100000,
  
  risk.free.rate = 0.0159
  
) {

  result          <- matrix( NA, nrow=iteration, ncol=n+3 )
  
  for(i in 1:iteration){
    
    tech.w        <- runif( 11, 0, 1000 )
    
    tech.w        <- ( tech.w / sum( tech.w ) ) * 0.80
    
    hedge.w       <- runif( (n- 11 ), 0, 1000 )
    
    hedge.w       <- ( hedge.w / sum( hedge.w ) ) * 0.2
    
    w             <- c( tech.w, hedge.w )
    
    ret           <- w %*% mu.ret
    
    vol           <- t( w ) %*% portfolio.cov %*% w
    
    std.dev       <- sqrt( vol )
    
    sharpe.ratio  <- ( ret - risk.free.rate ) / std.dev
    
    result[i, ]   <- c( w, ret, sqrt( std.dev ), sharpe.ratio )
    
  }
  
  colnames( result ) <- c( paste0( "w",1:n ), "ret", "sigma", "sharpe.ratio" )
  
  result             <- as.data.frame( result )
  
  return ( result )
  
}

parallel.monte.carlo.portfolio <- function(
    
  mu.ret,
  
  portfolio.cov,
  
  n.tickers,
  
  iteration      = 1000,   
  
  risk.free.rate = 0.0159,
  
  n.cores        = 12
  
) {

  labor.market <- makeCluster( n.cores )
  
  clusterSetRNGStream( labor.market, iseed= 133 )
  
  registerDoParallel( labor.market )
  
  clusterExport( labor.market, "monte.carlo.portfolio" )
  
  results <- foreach( core = 1:n.cores, .combine=rbind ) %dopar% 
    
    {
      
      monte.carlo.portfolio( mu.ret, 
                             
                             portfolio.cov, 
                             
                             n.tickers, 
                             
                             iteration, 
                             
                             risk.free.rate 
                             
      )
      
    }
  
  stopCluster( labor.market )
  
  return ( results )
  
}