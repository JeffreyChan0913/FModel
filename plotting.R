plot.cum.ret <- function(
    
  x,
  
  tickers
  
) {
  
  transform.future.data           <- pivot_longer( x, 
                                                   
                                                   cols=-Date,
                                                   
                                                   names_to="Class",
                                                   
                                                   values_to="ret"
                                                   
  )
  
  future.package$cum.port.ret     <- cum.ret( future.package$portfolio.return )
  
  future.package$cum.spy.ret      <- cum.ret( future.package$SPY )
  
  future.package$cum.sp.ret       <- cum.ret( future.package$`S&P500` )
  
  cum.future.package              <- pivot_longer( future.package,
                                                   
                                                   cols = starts_with("cum"),
                                                   
                                                   names_to = "Class",
                                                   
                                                   values_to = "cum.ret"
                                                   
  )
  
  cum.future.package$Class        <- recode( cum.future.package$Class,
                                             
                                             cum.port.ret = "Portfolio",
                                             
                                             cum.spy.ret  = "SPY",
                                             
                                             cum.sp.ret   = "SP500"
                                             
  )
  
  ggplot( cum.future.package, aes( x=Date, y=cum.ret, color = Class ) ) +
    
    geom_line( ) +
    
    scale_y_continuous( breaks=seq( -1, 3 , 0.5 ), limits=c( 0, 3 ) + c( -0.5, 0.5 ) ) +
    
    labs( title="Daily Return Performance", x="Date", y="Expected Return" ) +
    
    theme_minimal()
  
}



plot.w <- function(
    
  x,
  
  tickers
  
) {
  
  rownames( x ) <- tickers
  
  colnames( x ) <-"w"
  
  ordered.portfolio.weights     <- as.data.frame( x )
  
  ordered.portfolio.weights     <- ordered.portfolio.weights %>%
    
    arrange( desc( w ) )
  
  ggplot( ordered.portfolio.weights, aes( x=w, y=reorder( tickers, w ), fill = tickers ) ) +
    
    geom_bar( stat="identity") +
    
    scale_x_continuous( breaks=seq( 0, 0.20, by=0.01 ), limits=c( 0, 0.20 ) )  + 
    
    labs( title="Portfolio Weights Distribution", x="Weight", y="Ticker" ) +
    
    theme_minimal()
  
}

plot.frontier <- function(
    
  frontier.data,
  
  annualized.spy.sigma,
  
  annualized.spy.ret,
  
  annualized.sp500.sigma,
  
  annualized.sp500.ret,
  
  risk.free.rate = 0.0159
  
) {
  
  max.sharpe.ratio.index <- which.max(frontier.data$sharpe.ratio)
  
  sharpe.ratio.          <- frontier.data[ max.sharpe.ratio.index, 
                                           
                                           "sharpe.ratio"
                                         ]

  portfolio.sigma        <- frontier.data[ max.sharpe.ratio.index, 
                                           
                                           "sigma"
                                         ]
  
  portfolio.ret         <- frontier.data[ max.sharpe.ratio.index, 
                                          
                                          "ret"
  ]
  
  spy.slope <- ( annualized.spy.ret - risk.free.rate ) / annualized.spy.sigma
  
  sp500.slope <- ( annualized.sp500.ret - risk.free.rate ) / annualized.sp500.sigma
  
  ggplot( frontier.data, aes( x=sigma, y=ret, color=sharpe.ratio) ) +
    
    geom_scattermore( alpha=0.3 ) + 
    
    scale_x_continuous( breaks=seq( 0.2, 0.6, 0.1 ), limits=c( 0.2, 0.6 ) )  + 

    scale_y_continuous( breaks=seq( 0, 0.4 , 0.1 ), limits=c( 0, 0.4 ) )  + 
    
    labs( x="Portfolio Standard Deviation", y="Portfolio Expected Return" ) +
    
    ggtitle( "Efficient Frontier" ) +
    
    #expand_limits( x=c( 0.05 , 0.15 ) ) +
    
    theme_minimal( ) +
    

    geom_abline( intercept=risk.free.rate, slope=spy.slope,

                 color="darkorange", linetype="dashed", linewidth=0.5

    ) +

    geom_abline( intercept=risk.free.rate, slope=sp500.slope,

                 color="#CC7A8B", linetype="dashed", linewidth=0.5

    ) +
    
    scale_color_gradient(low = "#DFD3E3", high = "steelblue3", name = "Sharpe Ratio") +
    
    theme( legend.position="right")

}

plot.ret.VaR <- function( 
  
  x,
  
  q95,
  
  q99
  
) {
  
  quantile.labels <- c( "q95" = "Alpha = 0.05", "q99" = "Alpha = 0.01" )
  
  quantile.colors <- c( "q95" = "darkorange", "q99" = "#98AFC7" )
  
  ggplot( x, aes(x=daily.ret)) +
    
    geom_histogram( bins=200, fill="steelblue3" ) +
    
    ggtitle( "Expected Portfolio Daily Return with VaR" ) +
    
    scale_x_continuous(breaks=c( -0.1,0.1 ), limits=c( -0.1,0.1 ) ) +
    
    labs( x="Expected Return" ) +
    
    theme_minimal( ) +
    
    geom_vline( aes( xintercept=q95, color="q95" ), linetype="dashed", size=1.5 ) +
    
    geom_vline( aes( xintercept=q99, color="q99" ), linetype="dashed", size=1.5 ) +
    
    scale_color_manual( name="Quantiles", 
      
                        values=quantile.colors,
      
                        labels=quantile.labels
      
    ) +
    
    theme( legend.position="right", 
           
           legend.title=element_text(size=14),
           
           legend.text=element_text(size=12), 
           
           legend.key.size=unit(1.2,"cm"),

           )
    
}

plot.ret.CVaR <- function( 
    
  x,
  
  cutoff,
  
  alpha 
  
) {

  quantile.label <- paste0("Alpha = ", alpha)
  
  cutoff.color <- "#98AFC7"
  
  ggplot( x, aes( x=daily.ret ) ) +
    
    geom_histogram( bins=200, fill="steelblue3") +
    
    geom_histogram(data= subset(x, daily.ret <= cutoff), 
                   
                   bins=200, fill="#997070") +
    
    ggtitle( "Expected Portfolio Daily Return with CVaR" ) +
    
    scale_x_continuous(breaks=c( -0.1,0.1 ), limits=c( -0.1,0.1 ) ) +
    
    labs( x="Expected Return" ) +
    
    theme_minimal( ) +
    
    geom_vline( aes( xintercept=cutoff, color=cutoff.color ), linetype="dashed", size=1.5 ) +

    scale_color_manual( name="Quantiles", 
                        
                        values=cutoff.color,
                        
                        labels=quantile.label
                        
    ) +
    
    theme( legend.position="right", 
           
           legend.title=element_text(size=14),
           
           legend.text=element_text(size=12), 
           
           legend.key.size=unit(1.2,"cm"),
           
    )
  
}
