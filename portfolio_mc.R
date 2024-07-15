monte.carlo.portfolio <- function(
    
  mu.ret,
  
  portfolio.cov,
  
  n,
  
  iteration      = 100000,
  
  risk.free.rate = 0.0159
  
) {

  result          <- matrix( NA, nrow=iteration, ncol=n+3 )
  
  for(i in 1:iteration){
    
    w             <- runif( n, 0, 1000 )
    
    w             <- w / sum( w )
    
    ret           <- w %*% mu.ret
    
    vol           <- t( w ) %*% portfolio.cov %*% w
    
    std.dev       <- sqrt( vol )
    
    sharpe.ratio  <- ( ret - risk.free.rate ) / std.dev
    
    result[i, ] <- c( w, ret, sqrt( std.dev ), sharpe.ratio )
    
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