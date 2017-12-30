library(scales)
library(tidyverse)
library(tidyquant)
library(plotly)

source("scripts/data_helpers.R")
source("scripts/plot_helpers.R")

# Load data
portfolios <- loadPortfolioData(file.path("data", "portfolio.csv"))

stocks <- getLatestStockData(portfolios)

transactions <- loadTransactionData(file.path("data", "investment_transactions.txt")) %>%
  analyzeTransactions(portfolios)


portfolioSummary <- summaryByAccountSymbol(transactions, stocks)

p <- plotPortfolioSummary(filter(portfolioSummary, account == "Brian USAA SD IRA"))
p

write.csv(portfolioSummary, file.path("data", paste0("portfolio_summary_", max(stocks$date), ".csv")), row.names = FALSE)
