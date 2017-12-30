loadTransactionData <- function(fileName)
{
  result <- readr::read_tsv(fileName, 
                          col_names = c("x1", "trans_date", "account", "action", "security", "symbol", "price", "shares", "commission", "cash", "amount_invested", "x2", "x3"), 
                          skip = 7,
                          col_types = "?cccccnnnnn??") %>%
    filter(!is.na(symbol) & !is.na(trans_date)) %>%
    filter(!is.na(price)) %>%
    mutate(trans_date = parse_date(trans_date, "%m/%d/%Y")) %>%
    select(-x1, -x2, -x3) %>%
    arrange(account, symbol, trans_date) %>%
    mutate(id = c(1:nrow(.))) %>%
    data.frame()
  
}

loadPortfolioData <- function(fileName)
{
  readr::read_csv(fileName)
}

analyzeTransactions <- function(transactions, portfolios)
{
  result <- transactions %>%
    inner_join(portfolios, c("account", "symbol")) %>%
    mutate(transaction_amount = round(price * shares, 2),
           weighted_investment_years = (as.numeric(today() - trans_date)/365.25) * shares) %>%
    group_by(account, symbol) %>%
    mutate(cum_shares = cumsum(shares),
           cum_cost_basis = cumsum(amount_invested),
           cum_weighted_investment_years = cumsum(weighted_investment_years)) %>%
    mutate(cum_avg_share_price = cum_cost_basis / cum_shares,
           cum_avg_weighted_investment_years = cum_weighted_investment_years / cum_shares) %>%
    mutate(net_profit = case_when(action == "Sold" ~ abs(transaction_amount) - abs(amount_invested),
                                  grepl("Reinv", action) ~ amount_invested,
                                  TRUE ~ 0)) %>%
    mutate(cum_net_profit = cumsum(net_profit)) %>%
    data.frame()
}

summaryByAccountSymbol <- function(transactions, stocks)
{
  result_account <- transactions %>%
    group_by(account, symbol) %>%
    filter(id == max(id)) %>%
    left_join(select(stocks, symbol, close), "symbol") %>%
    mutate(unrealized_profit = (cum_shares * close) - cum_cost_basis) %>%
    mutate(total_return = unrealized_profit + cum_net_profit) %>%
    group_by(account) %>%
    summarise(portfolio_cost_basis = sum(cum_cost_basis),
              portfolio_realized_profit = sum(cum_net_profit),
              portfolio_unrealized_profit = sum(unrealized_profit),
              portfolio_total_return = sum(total_return))
  
  result <- transactions %>%
    group_by(account, symbol) %>%
    filter(id == max(id)) %>%
    left_join(select(stocks, symbol, close), "symbol") %>%
    inner_join(result_account, "account") %>%
    mutate(unrealized_profit = (cum_shares * close) - cum_cost_basis,
           portfolio_cost_basis_percent  = cum_cost_basis / portfolio_cost_basis) %>%
    mutate(total_return = unrealized_profit + cum_net_profit,
           total_return_factor = ((cum_shares * close) + cum_net_profit) / cum_cost_basis) %>%
    mutate(total_return_pct = total_return_factor - 1,
           total_return_cagr_pct = total_return_factor ^ (1 / cum_avg_weighted_investment_years) - 1,
           portfolio_total_return_percent = total_return / portfolio_total_return) %>%
    select(account, 
           symbol, 
           security, 
           shares = cum_shares, 
           cum_avg_weighted_investment_years,
           current_share_price = close,
           avg_share_price = cum_avg_share_price, 
           cost_basis = cum_cost_basis, 
           portfolio_cost_basis_percent,
           realized_profit = cum_net_profit,
           unrealized_profit,
           total_return,
           total_return_pct,
           portfolio_total_return_percent,
           total_return_cagr_pct) %>%
    data.frame()
    
}

getLatestStockData <- function(portfolios)
{
  stocks <- portfolios %>%
    distinct(symbol)
  
  result <- tq_get(stocks$symbol, from = today() - 4, to = today())
  
  result <- result %>%
    mutate(id = c(1:nrow(.))) %>%
    group_by(symbol) %>%
    filter(id == max(id))
}

