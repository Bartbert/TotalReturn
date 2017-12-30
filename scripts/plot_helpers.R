plotPortfolioSummary <- function(portfolioSummary)
{
  plotData <- portfolioSummary %>%
    mutate(`CAGR (%)` = total_return_cagr_pct,
           symbol = reorder(symbol, total_return_cagr_pct, FUN = mean))
  
  p <- ggplot(plotData, aes(x = `CAGR (%)`, y = symbol))
  p <- p + geom_point(size = 3)
  p <- p + xlab("CAGR (%)")
  p <- p + ylab("Investment")
  p <- p + labs(fill = "")
  p <- p + scale_x_continuous(labels = percent)
  p <- p + theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(colour = "grey60", linetype = "dashed"))
  
  p <- ggplotly(p)
}

