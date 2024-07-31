fontsize <- 14
th <-   theme_minimal( ) +
  
        theme( axis.title.x=element_text( size=fontsize ), 
               
               axis.title.y=element_text( size=fontsize ),
               
               plot.title=element_text( size=fontsize ),
               
               axis.text=element_text( size=fontsize ),  

               legend.title=element_text(size=fontsize),

               legend.text=element_text(size=fontsize),

               legend.key.size=unit(0.5,"cm"), 
               
               legend.position="bottom"

             )

w.fonsize <- 12

w.th <- theme_minimal( ) +
  
        theme( axis.title.x=element_text( size=w.fonsize ), 
               
               axis.title.y=element_text( size=w.fonsize ),
               
               plot.title=element_text( size=w.fonsize ),
               
               axis.text=element_text( size=w.fonsize ),

        )

f.fonsize <- 16

f.th <- theme_minimal( ) +
  
  theme( axis.title.x=element_text( size=f.fonsize ), 
         
         axis.title.y=element_text( size=f.fonsize ),
         
         plot.title=element_text( size=f.fonsize ),
         
         axis.text=element_text( size=f.fonsize ), 
         
         legend.title=element_text(size=12),
         
         legend.text=element_text(size=12),
         
         legend.key.size=unit(1,"cm"), 
         
         legend.position="right"
         
  )

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
  
  cum.future.package$Class        <- dplyr::recode( cum.future.package$Class,
                                             
                                                    "cum.port.ret" = "Portfolio",
                                             
                                                    "cum.spy.ret"  = "SPY",
                                             
                                                    "cum.sp.ret"   = "SP500"
                                             
                                                  )
  
  ggplot( cum.future.package, aes( x=Date, y=cum.ret, color = Class ) ) +
    
    geom_line( size=0.8 ) +
    
    scale_y_continuous( breaks=seq( -1, 3 , 0.5 ), limits=c( 0, 3 ) + c( -0.5, 0.5 ) ) +
    
    labs( title="Cumulative Daily Return Performance", x="Date", y="Expected Return" ) +
    
    th
  
}



plot.w <- function(
    
  x,
  
  ticker,
  
  hedge.t
  
) {
 
  data              <- data.frame (as.numeric( x ) , colnames( x ) )
  
  colnames( data )  <- c( "w", "ticker" )
  
  data              <- data[ order( data$w, decreasing=T ), ]
  
  data$group        <- ifelse( data$ticker %in% t.tickers, "Tech", "Hedge" )
  
  data$group        <- factor( data$group, levels=c("Tech", "Hedge") )
  
  group.colors      <- c( "Tech" = "steelblue", "Hedge" = "darkorange" )

  ggplot( data, aes( x=w, y=reorder( ticker, w ), fill = group ) ) +
    
    geom_bar( stat="identity") +
    
    scale_x_continuous( breaks=seq( 0, max(x)+0.02, by=0.02 ), limits=c( 0, max(x)+0.02 ) )  + 
    
    labs( title="Portfolio Weights Distribution", x="Weight", y="Ticker" ) +
    
    w.th +
    
    scale_fill_manual( values = group.colors ) +
    
    theme( legend.position="bottom" )
    
}

plot.frontier <- function(
    
  frontier.data,

  risk.free.rate = 0.0159
  
) {
  
  max.sharpe.ratio.index <- which.max(frontier.data$sharpe.ratio)
  
  sharpe.ratio           <- frontier.data[ max.sharpe.ratio.index, 
                                           
                                           "sharpe.ratio"
                                         ]
  
  portfolio.sigma        <- frontier.data[ max.sharpe.ratio.index, "sigma" ]
  
  portfolio.ret          <- frontier.data[ max.sharpe.ratio.index, "ret" ]

  ggplot( frontier.data, aes( x=sigma, y=ret, color=sharpe.ratio) ) +
    
    geom_scattermore( alpha=0.3 ) + 
    
    geom_scattermore( x=portfolio.sigma, y=portfolio.ret, color="#800000", pointsize=6 ) +

    scale_x_continuous( breaks=seq( 0.3, 0.6, 0.1 ), limits=c( 0.3, 0.6 ) )  + 
    
    labs( x="Portfolio Standard Deviation", y="Portfolio Expected Return" ) +
    
    ggtitle( "Efficient Frontier" ) +
    
    expand_limits( x=c( 0.05 , 0.15 ) ) +
    
    scale_color_gradient(low = "#DFD3E3", high = "steelblue3", name = "Sharpe Ratio") +

    w.th

}

plot.ret.VaR <- function( 
  
  x,
  
  q95,
  
  q99
  
) {
  
  quantile.labels <- c( "q95" = paste("VaR(95) =", round( q95,5 ) ) , "q99" = paste("VaR(0.01) = ", round( q99,5 ) ) )
  
  quantile.colors <- c( "q95" = "darkorange", "q99" = "#98AFC7" )
  
  ggplot( x, aes(x=daily.ret)) +
    
    geom_histogram( bins=200, fill="steelblue3") +
    
    ggtitle( "Value at Risk (VaR)" ) +
    
    labs(x="Daily Return", y="Count") +
    
    scale_x_continuous( breaks=seq( -0.1,0.1, 0.05 ), limits=c( -0.1,0.1 ) ) +

    theme_minimal( ) +
    
    geom_vline( aes( xintercept=q95, color="q95" ), linetype="dashed", size=1 ) +
    
    geom_vline( aes( xintercept=q99, color="q99" ), linetype="dashed", size=1 ) +
    
    scale_color_manual( name="Quantiles", 
      
                        values=quantile.colors,
      
                        labels=quantile.labels
      
    ) +
    
    th

}

plot.ret.CVaR <- function( 
    
  x,
  
  cutoff,
  
  CVar,
  
  alpha 
  
) {

  quantile.label  <- paste0( "CVaR(", alpha, ") = ", round( CVar, 5 ) )
  
  cutoff.color    <- "#98AFC7"

  ggplot( x, aes( x=daily.ret ) ) +
    
    geom_histogram( bins=200, fill="steelblue3") +
    
    geom_histogram(data= subset(x, daily.ret <= cutoff), 
                   
                   bins=200, fill="#FFB8BF") +
    
    ggtitle( "Conditional Value at Risk (CVaR)" ) +
    
    scale_x_continuous(breaks=seq( -0.1, 0.1, 0.05 ), limits=c( -0.1,0.1 ) ) +
    
    labs( x="Expected Return" , y="Count") +
    
    theme_minimal( ) +
    
    geom_vline( aes( xintercept=cutoff, color=cutoff.color ), linetype="dashed", size=1 ) +

    scale_color_manual( name="Quantile",
                        
                        values=cutoff.color,
                        
                        labels=quantile.label
                        
                      ) +
    
    th
  
}