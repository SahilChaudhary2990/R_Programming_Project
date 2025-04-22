# Stock Price Analysis Using R

# 1. Install and Load quantmod
install.packages("quantmod")
library(quantmod)

# 2. Fetch Historical Stock Data
tryCatch({
  getSymbols("AAPL", src = "yahoo", from = "2022-01-01", to = Sys.Date())
  print("Data fetched successfully.")
}, error = function(e) {
  print("Error fetching data.")
  print(e)
})

# 3. Calculate Simple Moving Averages (SMA)
AAPL_close <- Cl(AAPL)
sma50 <- SMA(AAPL_close, n = 50)
sma200 <- SMA(AAPL_close, n = 200)

# 4. Visualize Stock Prices with SMAs
chartSeries(AAPL, theme = chartTheme("white"), TA = NULL)
addSMA(n = 50, col = "blue", on = 1)
addSMA(n = 200, col = "red", on = 1)

# 5. Detect Crossover Points
bullish_crossovers <- which(sma50 > sma200 & lag(sma50) <= lag(sma200))
bearish_crossovers <- which(sma50 < sma200 & lag(sma50) >= lag(sma200))

# Highlight crossover points
addTA(AAPL_close[bullish_crossovers], type = "p", col = "green", on = 1, pch = 16, cex = 0.8)
addTA(AAPL_close[bearish_crossovers], type = "p", col = "red", on = 1, pch = 16, cex = 0.8)
