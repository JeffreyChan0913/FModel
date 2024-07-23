from pathlib import Path
import yfinance as yf
import pandas as pd
import sys

period = 'max' 

if len(sys.argv) < 2:
    print("Missing tickers")
    sys.exit(1)

tickers = [sys.argv[i].split(' ')[0] for i in range(1, len(sys.argv))]
print(tickers)
folder = Path("data/")
folder.mkdir(exist_ok=True)
    
for ticker in tickers:
    try:
        data = yf.download(ticker, period=period)
    except:
        print("Invalid ticker", ticker)
        continue
    if len(data.index) == 0:
        continue
    data = data.reset_index()
    data['ret'] = data['Adj Close'].pct_change() 
    data[['Date', 'ret']].to_csv('data/' + ticker + '.csv', index=False)
