
#Multiple Linear Regression in R

##Reading data
data=read.csv(file="D:/RWorks/Quant_Finance/Data/financial_data_NSE200.csv")
colnames(data)

##Selecting only required columns
library(dplyr)
data2 = data %>% select(tq,beta_adj,roa,fin_lev)
head(data2)

##Data Exploration: Visualization, Descriptive Stats

### Scatterplot matrix

pairs(data2) #Simplest

library(GGally)
ggpairs(data2)
# Customize ggpairs() with a green theme
ggpairs(data2, 
        lower = list(continuous = wrap("smooth", color = "darkgreen")),  # Regression line in green
        diag = list(continuous = wrap("barDiag", fill = "lightgreen")),  # Green histograms
        upper = list(continuous = wrap("cor", size = 5, color = "darkgreen")))+theme_minimal()


#Regression
library(car)
library(ggfortify)
library(ggplot2)

mod=lm(data=data2,formula=tq~beta_adj+roa+fin_lev)
summary(mod)  
# Spread-Level Plot (Residuals vs. Fitted Values)
spreadLevelPlot(mod)
autoplot(mod,colour = "orange")


# Get predicted values and residuals
Predicted = predict(mod)
Residuals = residuals(mod)
df=data.frame(Predicted,Residuals)

# Create the residual plot
ggplot(df, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot with transparency
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at 0
  labs(title = "Residuals vs. Predicted Values",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()  # Apply a clean theme
