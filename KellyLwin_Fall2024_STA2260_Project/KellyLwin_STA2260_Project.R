# Kelly Lwin
# STA2260 Project
# 12/08/2024
#===============================================================================
library(ISLR) # install library

# import dataset and see head
my_data <- read.csv("C:/Users/646ca/Downloads/CPP Fall 2024/dataset/synthetic_health_data.csv")
head(my_data)

# y_axis: Health Score
# x_axis: BMI, Diet Quality, Sleep Hours, Alcohol Consumption

# list column names 
colnames(my_data)
# plot the data
plot(my_data)

# use one-predictor models
m1.bmi <- lm(Health_Score ~ BMI, data=my_data)
summary(m1.bmi)

m1.dq <- lm(Health_Score ~ Diet_Quality, data=my_data)
summary(m1.dq)

m1.sh <- lm(Health_Score ~ Sleep_Hours, data=my_data)
summary(m1.sh)

m1.ac <- lm(Health_Score ~ Alcohol_Consumption, data=my_data)
summary(m1.ac)
#===============================================================================
# Plot the line of best fit for Diet Quality
plot(my_data$Diet_Quality, my_data$Health_Score, main="Diet Quality vs Health Score", xlab="Diet Quality", ylab="Health Score")
abline(m1.dq, col="blue", lwd=3)
# Add a legend to explain the line
legend("bottomright",  # Position of the legend
       legend="Best Fit Line",  # Label for the line
       col="blue",  # Color of the line
       lty=1,       # Line type
       lwd=3)       # Line width

AIC(m1.bmi, m1.dq, m1.sh, m1.ac) # AIC for all the four models

# m1.dq has the lowest AIC and highest R-squared so it seems to be the best predictor model.
plot(m1.dq)
#===============================================================================
# Plot the line of best fit for the other three to compare
plot(my_data$BMI, my_data$Health_Score, main="BMI vs Health Score", xlab="BMI", ylab="Health Score")
abline(m1.dq, col="blue", lwd=3)
# Add a legend to explain the line
legend("bottomright",  # Position of the legend
       legend="Best Fit Line",  # Label for the line
       col="blue",  # Color of the line
       lty=1,       # Line type
       lwd=3)       # Line width

plot(my_data$Sleep_Hours, my_data$Health_Score, main="Sleep Hours vs Health Score", xlab="Sleep Hours", ylab="Health Score")
abline(m1.dq, col="blue", lwd=3)
# Add a legend to explain the line
legend("bottomright",  # Position of the legend
       legend="Best Fit Line",  # Label for the line
       col="blue",  # Color of the line
       lty=1,       # Line type
       lwd=3)       # Line width

plot(my_data$Alcohol_Consumption, my_data$Health_Score, main="Alcohol Consumption vs Health Score", xlab="Alcohol Consumption", ylab="Health Score")
abline(m1.dq, col="blue", lwd=3)
# Add a legend to explain the line
legend("bottomright",  # Position of the legend
       legend="Best Fit Line",  # Label for the line
       col="blue",  # Color of the line
       lty=1,       # Line type
       lwd=3)       # Line width
#===============================================================================
# this model is health score = beta_0 + beta_1*diet_quality + beta_2*diet_quality^2
m1.dq.sq <- lm(Health_Score ~ Diet_Quality + I(Diet_Quality^2), data=my_data)

summary(m1.dq.sq) # Multiple R-squared:  0.471,	Adjusted R-squared:  0.4699

AIC(m1.bmi, m1.dq, m1.dq.sq, m1.sh, m1.ac) # m1.dq.sq  7433.260

plot(m1.dq.sq) # plot the new model

# abline(m1.dq.sq, col="green", lwd=3)# add a line of best fit for the new model
#In abline(m1.dq.sq, col = "green", lwd = 3) :
#v only using the first two of 3 regression coefficients

# can't plot the new model with abline because there is a third term with beta_2. 

x <- my_data$Diet_Quality
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1) # getting lots of x values to draw the line
yhat <- predict(m1.dq.sq, newdata=data.frame(Diet_Quality=xmesh))

plot(my_data$Diet_Quality, my_data$Health_Score, main="Diet Quality vs Health Score", 
     xlab="Diet Quality", ylab="Health Score")
abline(m1.dq, col="blue", lwd=3)
lines(xmesh, yhat, col="red", lwd=3)
legend("bottomright", legend=c("Linear", "Quadratic"), col=c("blue", "red"),
       lty=c(1,1), lwd=c(2,2))
#===============================================================================
# an inverted term instead of squared
m1.dq.inv <- lm(Health_Score ~ Diet_Quality + I(1/Diet_Quality), data=my_data)

summary(m1.dq.inv) #Multiple R-squared:  0.4667,	Adjusted R-squared:  0.4656 

AIC(m1.bmi, m1.dq, m1.dq.sq, m1.dq.inv, m1.sh, m1.ac) #m1.dq.inv  7441.375

# notice that R-squared is higher and AIC is lower than before (worse)
#===============================================================================
# multiple linear regression
# we will use diet quality and BMI
m2.dq.bmi <- lm(Health_Score ~ Diet_Quality + BMI, data=my_data)
summary(m2.dq.bmi)
# Multiple R-squared:  0.6141,	Adjusted R-squared:  0.6133

# squared each predictor 
m2.dq.bmi.sq <- lm(Health_Score ~ Diet_Quality + I(Diet_Quality^2) 
                   + BMI + I(BMI^2), data=my_data)
summary(m2.dq.bmi.sq)
# Multiple R-squared:  0.6283,	Adjusted R-squared:  0.6268 

AIC(m2.dq.bmi, m2.dq.bmi.sq)

# compare model 1 and model 2 AIC
AIC(m1.dq, m1.dq.sq, m2.dq.bmi, m2.dq.bmi.sq)

AIC(m1.dq, m2.dq.bmi.sq)
#===============================================================================
# use everything
overfit <- lm(Health_Score ~ ., data=my_data)
summary(overfit)

# removing unnecessary predictors or names
overfit2 <- lm(Health_Score ~. -Age -Exercise_Frequency -Smoking_Status, data=my_data)
summary(overfit2)

# do not include overfit graphs
AIC(overfit, overfit2)
#===============================================================================
# there's no relationship between the two predictors; cannot draw abline
plot(my_data$Diet_Quality, my_data$BMI)
plot(my_data$BMI, my_data$Diet_Quality)

# can also do interaction terms, like how well do diet quality and BMI work together
m2.interaction <- lm(Health_Score ~ Diet_Quality*BMI, data=my_data)
summary(m2.interaction)

# getting rss for m2.dq.bmi
yhat_m2 <- predict(m2.dq.bmi)
rss_m2.dq.bmi <- sum((my_data$Health_Score -yhat_m2)/2); rss_m2.dq.bmi
#===============================================================================