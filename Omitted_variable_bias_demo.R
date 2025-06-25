
# Examining the relationship between Gold returns and VIX over the past 32 months.

library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Step 1: Set date range
end_date <- Sys.Date()
start_date <- end_date %m-% months(32)

# Step 2: Fetch data
getSymbols("GOLDBEES.BO", from = start_date, to = end_date, auto.assign = TRUE)
getSymbols("^VIX", from = start_date, to = end_date, auto.assign = TRUE)

# Step 3: Convert to monthly close
gold_monthly = to.monthly(GOLDBEES.BO, indexAt = "lastof", drop.time = TRUE)[, 4]
vix_monthly  = to.monthly(VIX, indexAt = "lastof", drop.time = TRUE)[, 4]

# Step 4: Compute returns
gold_returns = monthlyReturn(gold_monthly, type = "log") %>% round(2)*100
vix_pct_change = monthlyReturn(vix_monthly, type = "arithmetic") %>% round(2)*100

# Step 5: Combine data
data_combined = merge(gold_returns, vix_pct_change, join = "inner")
colnames(data_combined) = c("Gold_Returns", "VIX_Change")
data_combined = na.omit(data_combined)

# Convert to data.frame for ggplot
data_df = data.frame(Date = index(data_combined), coredata(data_combined))
head(data_df)

# Step 6: Visualize using ggplot2

# Plot 1: Gold Returns (time series)
ggplot(data_df, aes(x = Date, y = Gold_Returns)) +
  geom_line(color = "goldenrod", linewidth = 1) +
  labs(title = "Monthly Gold Returns", x = "Date", y = "▲ in Gold") 

# Plot 2: VIX % Change (time series)
ggplot(data_df, aes(x = Date, y = VIX_Change)) +
  geom_line(color = "darkblue", linewidth = 1) +
  labs(title = "▲ in VIX", x = "Date", y = "% Change")

# Plot 3: Scatterplot with regression line
ggplot(data_df, aes(x = VIX_Change, y = Gold_Returns)) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Gold Returns vs % Change in VIX",
       x = "% Change in VIX", y = "Gold Monthly Returns")

ggplot(data_df, aes(x = VIX_Change, y = Gold_Returns)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Gold Returns vs % Change in VIX",
       x = "% Change in VIX", y = "Gold Monthly Returns")

# Step 7: Run regression
model = lm(Gold_Returns ~ VIX_Change, data = data_df)
summary(model)

# Step 8: Check for omitted variable bias
data2=data.frame(gold_return=data_df$Gold_Returns,residuals=model$residuals)
ggplot(data2,aes(x=gold_return,y=residuals))+geom_point(col="dark red")+ggtitle("Dependent variable (Gold Returns) vs Residuals")+xlab("Gold Returns (%)")+ylab("Residuals (Model: Gold~VIX)")
