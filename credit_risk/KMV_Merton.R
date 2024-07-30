kmv.iterative <- function(
    
  E,
  
  D,
  
  V, 
  
  S, 
  
  t, 
  
  r, 
  
  iterations
  
) {
  
  for ( i in 1:iterations ) {

    d1 <- ( log( V / D ) + ( r + 0.5 * S^2 ) * t ) / ( S * sqrt( t ) )
    
    d2 <- d1 - S * sqrt( t )
    
    Nd1 <- pnorm( d1 )
    
    Nd2 <- pnorm( d2 )
    
    v.new <- ( E + D * exp( -r * t ) * Nd2 ) / Nd1
    
    s.new <- ( E * S ) / ( v.new * Nd1 )
    
    if ( abs( V - v.new ) < 1e-6 && abs( S - s.new ) < 1e-6 ) {
      
      break
      
    }
    
    V <- v.new
    
    S <- s.new
    
  }
  
  DD <- ( log( V / D ) + ( r - 0.5 * S^2 ) * T ) / ( S * sqrt( T ) )

  PD <- pnorm( DD )
  
  return( list( V = V, S = S, DD = DD, PD = PD ) )
  
}

run <- function( 
    
  kmv.data,
    
  company

) {

  E <- kmv.data[company, "equity"]
  
  sigma.E <- kmv.data[company, "equity.volatility"]
  
  D <- kmv.data[company, "debt"]
  
  V <- kmv.data[company, "asset"]
  
  r <- kmv.data[company, "drift"]
  
  t <- 1
  
  iterations <- 100000

  V0 <- E + D

  sigma.V <- sigma.E

  kmv.result <- kmv.iterative( E, D, V0, sigma.V, t, r, iterations )
  
  V.final <- kmv.result$V
  
  sigma.V.final <- kmv.result$S
  
  DD <- kmv.result$DD
  
  PD <- kmv.result$PD

  cat( "Probability of Default (PD):", PD, "\n" )
  
}


