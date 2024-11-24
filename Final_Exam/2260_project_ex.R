# mtcars is a built-in data set provided with R, so it has 
#   default documentation:
?mtcars

# Just to get a quick glimpse of the data:
head(mtcars)

# Variables are the columns, so pick one as a response 
#   that makes senseto use (I will use mpg here), and 
#   the others are all possible predictors.

# 32 rows with 11 columns, the rows are like "observations", 
#   columns are the response with possible predictors
dim(mtcars)

# this shows pairwise scatter plots but as you can tell, 
#   it is very hard to see... We can do a couple of things. 
plot(mtcars)

plot(mtcars[,1:6])

 # ncol just represents "number of columns". there is also 
#   nrow if you want.
plot(mtcars[,7:ncol(mtcars)])

# Since mpg is our response, we are using these plots to 
#   see if there is a pattern between mpg and any of the 
# predictors. So the last line above should be like:
plot(mtcars[, c(1, 7:ncol(mtcars))])

# or we could use a for loop... This may fail with certain
#   datasets, especially depending on syntax, so explore
#   with it a bit if you'd like.

# just extracting the response and storing 
#   the values into a single variable
y <- mtcars$mpg 

# this adjusts our window for plots to be a 2x2 window
par(mfrow=c(2,2)) 
for(i in 1:ncol(mtcars)){
  x <- mtcars[,i] # predictor i
  plot(x, y, 
       xlab=names(mtcars[i]), # just to have the names of the predictors on x-axis
       ylab="mpg")
}
# reset the plot window size back to normal.
par(mfrow=c(1,1)) 

# after running the for loop, click the left arrow "<-" 
#   at the top left of your plot window to cycle through 
#   the plots that were created. 

# looks like disp has some sort of negative correlation, 
#   maybe quadratic or inverse, similar with hp. 
#   drat seems somewhat linear but has a ton of noise, I 
#   would avoid it. wt seems linear, should try that. 
#   qsec does not look good, too noisy (moreso than drat).

# the discrete predictors (vertical lines at values) can 
#   technically be used, but I would avoid for this.

# trying some one-predictor models:

# lm(...) is used to make linear models in R. 
# The syntax is lm(response ~ predictors, data=data)
# There's a lot that can be done with this, ?lm for documentation.
fit.disp <- lm(mpg ~ disp, data=mtcars) # linear
fit.disp2 <- lm(mpg ~ disp + I(disp^2), data=mtcars) # quadratic
fit.disp3 <- lm(mpg ~ disp + I(1/disp), data=mtcars) # inverse/rational/whatever

# getting R^2 (look at adjusted R^2) and AIC of each model
# R^2 is how much variation is explained in the model, AIC 
#   is more like "predictive power". Adjusted R^2 and AIC 
#   both penalize overfitting. 
summary(fit.disp) # p-value being very small indicate a strong relationship
summary(fit.disp2)
summary(fit.disp3)
AIC(fit.disp, fit.disp2, fit.disp3)

# remember to use your scatterplots to try and determine what 
#   kind of relationship exists
fit.hp <- lm(mpg ~ hp, data=mtcars) # linear
fit.hp2 <- lm(mpg ~ hp + I(hp^2), data=mtcars) # quadratic
fit.hp3 <- lm(mpg ~ hp + I(1/hp), data=mtcars) # inverse/rational/whatever

summary(fit.hp)
summary(fit.hp2)
summary(fit.hp3)
AIC(fit.hp, fit.hp2, fit.hp3)

# now trying mpg predicted from wt. looked strongly linear 
#   with some slight offsets in the tail

fit.wt <- lm(mpg ~ wt, data=mtcars)
fit.wt2 <- lm(mpg ~ wt + I(wt^2), data=mtcars)

summary(fit.wt)
summary(fit.wt2)
AIC(fit.wt, fit.wt2)

# do this above process with all possible one-predictor models 
#   you can think of, then compare their AIC and adjusted R^2.

# Of the ones above, fit.disp3 did the best on basis of AIC, 
#   and also on the basis of adjusted R^2. So this is probably 
#   the (m1) I would choose.

# to plot the fitted line for a linear one-predictor model 
#   it is easy:
plot(mtcars$disp, mtcars$mpg)

# this overlays the plot with the best fit line of the form 
#   y=ax+b, changed color to blue to make it more obvious 
abline(fit.disp, col="blue")

# Plotting best fit line with other forms like quadratic, 
#   inverse, etc is a bit more complicated.

x <- mtcars$disp
# getting lots of x values to draw the line through
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1)
yhat <- predict(fit.disp3, 
                newdata=data.frame(disp=xmesh))

# to do this in general, change the capitalized words to your 
#   own model/predictors and such
# x <- DATASET$VARNAME
# xmesh <- seq(min(x)-1, max(x)+1, by=0.1) # don't change
# yhat <- predict(MODEL, newdata=data.frame(VARNAME=xmesh))

# lines(...) connects x and y vectors to each other sequentially
lines(xmesh, yhat, col="red") 
legend("topright", # where to draw the legend
       c("Linear", "Inverse"), # line names
       lty=c(1,1),  # just leave this as 1 for each line you're drawing)
       col=c("blue", "red") # colors of lines
)
summary(fit.disp3)
# so the fitted model is:
# yhat = beta_0hat + beta_1hat*x + beta_2hat*(1/x) which is 
# mpg = 12.94 - 0.005277disp + 1395/disp

m1 <- fit.disp3 # so we are saying fit.disp3 is our choice for m1




# YOU WILL NOT BE PLOTTING LINES FOR MODEL 2! 
# It is impossible to visualize anything with >2 predictors, 
#   each predictor you add adds another dimension to the data. 
# Two predictors = 3-dimensional, 3+ predictors = good luck LUL

# now for Model 2... Do the same as model 1 but now with many 
#   more possibilities, here are some:
m2.1 <- lm(mpg ~ wt + I(wt^2) + disp + I(disp^2) + I(1/disp) + hp + I(hp^2) + carb + I(carb^2), data=mtcars)
summary(m2.1)
AIC(m1, m2.1)

# R^2 is better slightly, as is m1, but the summary shows a LOT 
#   of stuffwe don't need. Remove those with very high p-values 
#   (the last column, Pr(>|t|)), namely carb:
m2.2 <- lm(mpg ~ wt + I(wt^2) + disp + I(disp^2) + I(1/disp) + hp + I(hp^2), data=mtcars)
summary(m2.2)
AIC(m1, m2.1, m2.2)
# again, a bit better.

# and just to give some ideas and how you can work with syntax:

# making a model using ALL predictors that are possible 
#   (linear form), this is not using quadratic terms, cubics, 
#   inverses, interactions, etc etc
m2.all <- lm(mpg ~ ., data=mtcars)
summary(m2.all)

# all predictors but removing carb:
m2.nocarb <- lm(mpg ~ . - carb, data=mtcars)
summary(m2.nocarb)

# "interaction terms" means putting two predictors together in 
#   a way. Using * gives the linear predictors as well as 
#   interaction, if you use : then its JUST interaction
m2.int_disp_wt <- lm(mpg ~ disp*wt, data=mtcars)
summary(m2.int_disp_wt)

m2.int_only <- lm(mpg ~ disp:wt, data=mtcars)
summary(m2.int_only)
AIC(m2.1, m2.2, m2.all, m2.int_disp_wt, m2.int_only, m2.nocarb)

# so in summary, m2's best model from what we made is 
#   probably m2.2
m2 <- m2.2
summary(m1) 
summary(m2)
AIC(m1, m2)

#       R^2      AIC 
# m1: 0.8526  149.3468
# m2: 0.8856  145.2009

# so m2 explains more variability in this data set 
#   (88.56% vs 85.26%), and also has better predictive 
#   power on the basis of AIC.