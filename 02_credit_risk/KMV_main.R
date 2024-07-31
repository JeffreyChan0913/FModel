rm( list = ls( all.names=T ) )

options( warn=-1)

try( dev.off( dev.list()["RStudioGD"] ), silent=TRUE )

setwd( dirname( rstudioapi::getSourceEditorContext()$path) )

required.pkgs <- c( "quantmod", "reticulate", "dplyr", "tidyverse",
                    
                    "foreach", "doParallel", "lubridate", "matrixStats",
                    
                    "scattermore", "ggplot2", "nleqslv" )

pacman::p_load( required.pkgs, character.only=T )

source( "../01_portfolio/data_processor.R" )

source( "KMV_Merton.R" )

risk.free <- 0.05375

daily.risk.free <- risk.free / 252

period <- 1

kmv.data            <- read.csv( "kmv23data.csv" )

tickers             <- read.csv( "../01_portfolio/tech_tickers.csv" )

tickers             <- tickers[c(1:11), ]

rownames(kmv.data)  <- tickers

kmv.data <- kmv.data[, -1]

ret                 <- get.current.return( tickers, "2023-01-01", "2024-01-01", T ) * 252

last.trading.price.date23 <- as.Date( "2023-12-29" )

i = 1

marketcap23 <- numeric( length( tickers ) )

for( ticker in tickers ){
  
  temp           <- c()
  
  file           <- file.path( "../01_portfolio/data" , paste0( ticker, ".csv" ) )
  
  data           <- read.csv( file )
  
  data$Date      <- as.Date( data$Date )

  marketcap23[i] <- data[ data$Date == last.trading.price.date23, "Adj.Close"] * kmv.data$Share.Issued.2023[i]
  
  i <- i + 1
    
}

kmv.data$marketcap23 <- marketcap23

kmv.data$equity23    <- kmv.data$Total.Assets.2023 - kmv.data$Current.Liabilities.2023

kmv.data <- kmv.data %>%
            
              mutate( drift = colMeans( ret ),
                
                      equity = marketcap23, 
                      
                      debt = ( Current.Liabilities.2023 + ( 0.5 * Long.Term.Debt.2023 ) ), 
                      
                      asset = Total.Assets.2023,
                      
                      equity.volatility = apply( ret, 2, sd ) * sqrt( 252 )
                      
                    )

for( ticker in tickers ){
  
  cat( ticker, ": " )
  
  run( kmv.data, ticker )  
  
}





