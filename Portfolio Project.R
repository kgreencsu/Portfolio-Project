# Import Required packages
set.seed(500)
library(neuralnet)
library(MASS)
library(tidyverse)
library(Metrics)
library(NeuralNetTools)
library(readxl)
library(randomForest)
library(ggplot2)



final <- read_excel("final.xlsx")

data <- final
data <- data.frame(data)

z <-data %>%
  select(-c("Date"))

z$price <- as.numeric(z$price)

hist_data <- hist(data$price,xlim=c(0,150), ylim=c(0,150), main='Price Distribution', xlab = 'Price', col = 'red')
x_values <- seq(min(data$price), max(data$price), length = 150)
y_values <- dnorm(x_values, mean = mean(data$price), sd = sd(data$price)) 
y_values <- y_values * diff(hist_data$mids[1:2]) * length(data$price) 
lines(x_values, y_values, lwd = 2)
data <- z



# Normalize the data
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, 
                              scale = maxs - mins))

# Split the data into training and testing set
index <- sample(1:nrow(data), round(0.75 * nrow(data)))
train_ <- scaled[index,]
test_ <- scaled[-index,]



nn <- neuralnet(price ~ Open_Interest_All+ Prod_Merc_Positions_Long_All+Prod_Merc_Positions_Short_All+Swap_Positions_Long_All+
                     Swap__Positions_Short_All +Swap__Positions_Spread_All+M_Money_Positions_Long_All+M_Money_Positions_Short_All+
                     M_Money_Positions_Spread_All+Other_Rept_Positions_Long_All+Other_Rept_Positions_Short_All+Other_Rept_Positions_Spread_All+
                     Tot_Rept_Positions_Long_All + Tot_Rept_Positions_Short_All+NonRept_Positions_Long_All,
                   data = train_,
                   hidden = c(10,5,2),
                   linear.output = T,
                   lifesign = 'full',
                   rep=1, threshold = 0.01)

pr.nn<- neuralnet::compute(nn, test_[,1:19])




olden(nn, bar_plot = F)
# Compute mean squared error right
testpredict <- test_$price* (max(data$price) - min(data$price)) + min(data$price)
predictions <- pr.nn$net.result * (max(data$price) - min(data$price)) + min(data$price)
rmse(testpredict,predictions)
mse(testpredict,predictions)

# Plot the neural network
plot(nn)


# Plot regression line
plot(testpredict, predictions, col = rep(1:2, each = 10), pch = 19)
legend("bottomright", legend = paste("Group", 1:2), col = 1:2, pch = 19, bty = "n")
abline(0, 1, lwd = 2)




#Linear Model
linear_model <- lm(price ~ Open_Interest_All+ Prod_Merc_Positions_Long_All+Prod_Merc_Positions_Short_All+Swap_Positions_Long_All+
                     Swap__Positions_Short_All +Swap__Positions_Spread_All+M_Money_Positions_Long_All+M_Money_Positions_Short_All+
                     M_Money_Positions_Spread_All+Other_Rept_Positions_Long_All+Other_Rept_Positions_Short_All+Other_Rept_Positions_Spread_All+
                     Tot_Rept_Positions_Long_All + Tot_Rept_Positions_Short_All+NonRept_Positions_Long_All, data = train_)

hist(residuals(linear_model), col = "steelblue")

test_linear <- predict(linear_model, data = test_)
linear_test <- data.frame(actual = test_$price, predicted = test_linear)
plot(linear_test$actual, linear_test$predicted, col = rep(1:2, each = 10), main = "Linear Model Test", xlab = 'Actual', ylab= 'Predicted')
legend("bottomright", legend = c("Actual", "Predicted"), fill= c("black","Red"), pch = 19, bty = "n")
abline()

testpredict <- test_$price* (max(data$price) - min(data$price)) + min(data$price)
predictions <- test_linear * (max(data$price) - min(data$price)) + min(data$price)
rmse(testpredict,predictions)
mse(testpredict,predictions)


summary(linear_model)
plot (linear_model)


### Import libraries


rf.fit <- randomForest(price ~ Open_Interest_All+ Prod_Merc_Positions_Long_All+Prod_Merc_Positions_Short_All+Swap_Positions_Long_All+
                         Swap__Positions_Short_All +Swap__Positions_Spread_All+M_Money_Positions_Long_All+M_Money_Positions_Short_All+
                         M_Money_Positions_Spread_All+Other_Rept_Positions_Long_All+Other_Rept_Positions_Short_All+Other_Rept_Positions_Spread_All+
                         Tot_Rept_Positions_Long_All + Tot_Rept_Positions_Short_All+NonRept_Positions_Long_All, data = data, ntree=10000,
                       keep.forest=FALSE, importance=TRUE)

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

rf.fit$mse[which.min(rf.fit$mse)]
sqrt(rf.fit$mse[which.min(rf.fit$mse)])


