
# To test whether Bitcoin behaves like Gold (a safe haven) or a Tech stock (risky asset)


# Load libraries
library(tidyquant)
library(dplyr)
library(tidyr)
library(purrr)

# Define tickers (Yahoo Finance)
tickers = c("BTC-USD", "GC=F", "^NDX")  # Bitcoin, Gold Futures, Tech Stock index)

# Set time period (last 5 years)
start_date = Sys.Date() - 365 * 5
end_date = Sys.Date()

# Function to fetch returns for each ticker
get_returns = function(ticker) {
  tq_get(ticker, from = start_date, to = end_date, get = "stock.prices", periodicity = "monthly") %>%
    select(date, adjusted) %>%
    mutate(return = (adjusted / lag(adjusted)) - 1) %>%
    select(date, return) %>%
    rename_with(~ ticker, .cols = return)  # Rename return column to ticker name
}

# Apply function to all tickers and join results
returns_list <- map(tickers, get_returns)  # Fetch returns for each asset
returns_df <- reduce(returns_list, left_join, by = "date")  # Merge into one table

# Rename columns for clarity
colnames(returns_df) <- c("date", "btc_ret", "gold_ret", "tech_ret")

# View the first few rows
head(returns_df)

# Ready Data
data=returns_df %>% na.omit() %>% select(btc_ret,gold_ret,tech_ret) %>% round(4)*100

head(data)

write.csv(data, file="D:/RWorks/Quant_Finance/Data/bit_gold_tec.csv", row.names = FALSE)

# Visualization
library(GGally)
ggpairs(data)

## Customize ggpairs() with a green theme
ggpairs(data, 
        lower = list(continuous = wrap("smooth", color = "darkgreen")),  # Regression line in green
        diag = list(continuous = wrap("barDiag", fill = "lightgreen")),  # Green histograms
        upper = list(continuous = wrap("cor", size = 5, color = "darkgreen")))

ggplot(data=data,aes(x=btc_ret,y=tech_ret))+geom_point(col="dark green")

#Regression
library(car)
library(broom)

model_1=lm(data,formula=btc_ret~gold_ret)
model_2=lm(data,formula=btc_ret~tech_ret)
model_3=lm(data,formula=btc_ret~gold_ret+tech_ret)

##Regression summary
summary(model_3)

## Regression anova
anova(model_3)

## AIC, BIC
AIC(model_3)
BIC(model_3)

## Model summary
library(modelsummary)

models = list("Model 1" = model_1, "Model 2" = model_2, "Model 3" = model_3)

modelsummary(models,fmt=2,estimate = "{estimate}{stars}",statistic = "{p.value}",title = "Regression of Bitcoin returns on Gold and Tech stock returns")


library(ggfortify)
# Spread-Level Plot (Residuals vs. Fitted Values)
spreadLevelPlot(mod)
autoplot(model,colour = "orange")
