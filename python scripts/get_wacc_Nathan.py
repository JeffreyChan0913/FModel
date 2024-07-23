import warnings

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yahoo_fin.stock_info as si
import yfinance as yf

warnings.filterwarnings("ignore")
pd.set_option("display.max_rows", None)


def getWACC(ticker, riskFreeRate2024=0.0458, sp500Annualret=0.1032009):
    """Calculate the Weighted Average Cost of Capital (WACC) for a company.
    Args:
        ticker (str): string of company ticker
        riskFreeRate2024 (float): risk free rate in 2024
        sp500Annualret (float): Annual returns from sp500. Defaults to 0.1032009.
    Returns:
        float: WACC
    """

    Tick = yf.Ticker(ticker)

    try:
        beta = Tick.info["beta"]
    except KeyError:
        print(f"{ticker} does not have beta value")
        return None

    try:
        totalLiabilities = (
            Tick.balancesheet.loc["Current Liabilities"][1]
            - Tick.balancesheet.loc["Other Current Liabilities"][1]
        )
    except KeyError:
        totalLiabilities = Tick.balancesheet.loc["Current Liabilities"][1]

    costOfEquity = riskFreeRate2024 + beta * (sp500Annualret - riskFreeRate2024)
    FinancialStatement = Tick.financials
    totalDebt = Tick.info["totalDebt"]

    equity = Tick.balancesheet.loc["Total Assets"][1] - totalLiabilities
    totalValue = equity + totalDebt
    interest = FinancialStatement.loc["Interest Expense"][0]

    interestRate = interest / totalDebt
    taxRate2024 = (
        FinancialStatement.loc["Tax Provision"][0]
        / FinancialStatement.loc["Pretax Income"][0]
    )

    costOfDebt = interestRate
    WACC = ((equity / totalValue) * costOfEquity) + (
        (totalDebt / totalValue) * costOfDebt * (1 - taxRate2024)
    )
    print(f"{ticker} 2024 wacc: {WACC:.2%}")
    return WACC


def main():
    tickers = [
        "NVDA",
        "AMD",
        "TSM",
        "ASML",
        "MU",
        "AMAT",
        "GOOG",
        "AAPL",
        "META",
        "TSLA",
        "DUK",
        "GLD",
        "X",
        "PG",
        "TLT",
        "CVS",
        "VGIT",
        "GOVT",
    ]

    wacc_list = []

    for ticker in tickers:
        wacc = getWACC(ticker)
        wacc_list.append(wacc)

    pd.DataFrame({"Ticker":tickers,
                  "wacc":wacc_list}).to_csv("wacc_list.csv")


if __name__ == "__main__":
    main()
