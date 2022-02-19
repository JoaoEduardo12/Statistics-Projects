######### 
#' Author: Joao Eduardo Teixeira
#' Nonparametric kernel regression: NW estimator

data <- read.csv("river_measurements.csv") # my data
# individual plots
hist(data$Conductivity) # bimodal distribution
hist(data$OD) # most values concentrated between 7 and 9, which is ideal for river water quality in general

# scatterplot of Conductivity vs DO
plot(data$Conductivity, data$OD, main = "Scatterplot", xlab = "Conductivity", ylab = "DO")
# we see a descending pattern, which is expected, but how it decreases changes, being more
# prominent in the beginning and less prevalent in high values of conductivity,
# as well as also having more dispersed data

# How well does a simple linear regression model this relationship?
# This is just for exploration purposes. So I can better see how NW changes the result

mod_linear <- lm(OD ~ Conductivity,
                 data = data)

summary(mod_linear)
# the coefficient is negative as we expect and is statistically significant by wald test
confint(mod_linear)

# The r-squared is low overall, on 0.24, thus explaining only 24% of variability in DO through Cond

par(mfrow = c(2,2))
plot(mod_linear) # to quickly see if model is valid

# Visualising the fitted line
plot(data$Conductivity, data$OD, main = "Simple Linear Regression", xlab = "Conductivity", ylab = "DO")
abline(lm(data$OD ~ data$Conductivity), col = "red")


# Now let's see how np regression fares!

#' First iteration, choosing h as the default as 10
#' ksmooth is the function to use in R for NW regression
#' In this work, only the Gaussian kernel is explored
kernel_h10 <- ksmooth(data$Conductivity, data$OD, 
        kernel = "normal", 
        bandwidth = 10)

# Let's plot it!
plot(data$Conductivity, data$OD, main = "NW Regression, h = 10", xlab = "Conductivity", ylab = "OD")
lines(kernel_h10, lwd = 2, lty = 3, col = "blue")

# Now let's explore 2 more situations, where we change the bandwith and see if
# the line fits the data better

#' Low bandwidth, expecting the line to overfit the data
kernel_h2 <- ksmooth(data$Conductivity, data$OD, 
                      kernel = "normal", 
                      bandwidth = 2)

# Let's plot it
plot(data$Conductivity, data$OD, main = "Simple Linear Regression", xlab = "Conductivity", ylab = "OD")
lines(kernel_h2, lwd = 2, lty = 3, col = "blue")

#' Very high bandwidth, expecting the line to underfit the data
kernel_h50 <- ksmooth(data$Conductivity, data$OD, 
                      kernel = "normal", 
                      bandwidth = 50)

# The plots
plot(data$Conductivity, data$OD, main = "Simple Linear Regression", xlab = "Conductivity", ylab = "OD")
lines(kernel_h50, lwd = 2, lty = 3, col = "blue")

# Let's visualize every bandwidth effect on the same graph
plot(data$Conductivity, data$OD, main = "NW Regression", xlab = "Conductivity", ylab = "OD")
lines(kernel_h2, lwd = 2, lty = 4, col = "#337ab7")
lines(kernel_h10, lwd = 2, lty = 4, col = "#e8a317")
lines(kernel_h50, lwd = 2, lty = 4, col = "#6cbb3c")
legend("topright", c("h=2","h=10","h=50"), lwd=6, col=c("#337ab7","#e8a317","#6cbb3c"))

# The best smoothing bandwidth should balance both the bias and variability!
# I will apply leave-one-out cross-validation to find the optimal bandwidth

#' The function created below will perform LOOCV, and find the h value that minimizes
#' the mean squared error of LOOCV
h_optimizer <- function(X,Y,h_range,plot_mse = FALSE) {
    #' X and Y: explanatory and response variable, respectively
    #' h_range: vector of range to test bandwidth values
    #' plot_mse: switch to turn on a graph showing the values of MSE obtained by each h iteration
    sample_size <- length(X) # size of data
    h_iterations <- length(h_range) # size of h vector input
    mse_h <- c() # initialize empty vector of mean MSE's got for each h
    for (h in h_range) {
        current_h <- h_range[h] # saving the value of h corresponding to the current h in loop
        mse_cv <- c() # initialize empty vector of mse's got from LOOCV
        for (i in 1:sample_size) {
            X_training <- X[-i]; Y_training <- Y[-i] # training set
            X_test <- X[i]; Y_test <- Y[i] # validation set
            Y_pred <- ksmooth(X_training, Y_training, "normal", h, x.points = X_test) # get predicted y
            mse_cv <- c(mse_cv, (Y_test - Y_pred$y)^2) # store the MSE value in the vector of LOOCV MSEs!
        }
        mse_h <- c(mse_h, mean(mse_cv)) # get and keep the mean of MSEs of LOOCV, in the vector of mean MSE's
    }
    min_h <- h_range[which.min(mse_h)] # get the minimum value in the vector, this will be our optimal h
    if (plot_mse) { # plot
        plot(h_range, mse_h, type="o", lwd=2, col="blue",
             xlab="Values of h(x)", ylab="Mean Squared Error")
        abline(v = min_h, col = "red", lwd = 2)
    }
    return(min_h)
}

# Let's get the best h value, that will be stored in best_h
best_h <- h_optimizer(data$Conductivity, data$OD, seq(2:100), plot_mse = TRUE)

# Now let's see the NW with best_h
kernel_best_h <- ksmooth(data$Conductivity, data$OD, 
                      kernel = "normal", 
                      bandwidth = best_h)

plot(data$Conductivity, data$OD, main = "NW Regression, h = 31", xlab = "Conductivity", ylab = "DO")
lines(kernel_best_h, lwd = 2, lty = 3, col = "blue")

# All done!
