
# dataset: https://www.kaggle.com/harlfoxem/housesalesprediction
# this dataset had to be changed in order to fit the scope of this assignment
# eight explanatory variables were selected based on interest and interpretability
library(corrplot)
library(cowplot)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(viridis)
require(ggmap)
library(GGally)
library(car)
library(regclass)
library(faraway)
library(ggcorrplot)
library(sjPlot)
library(equatiomatic)

# (a) State, in a clear and precise way, the problem that you will be solving in this assignment.

# The goal of this assignment is to fit the best possible linear model 
# and to study the effects of 8 explanatory variables on one response variable, price

###### Data Importation #####
# import data
kc_houses <- read.csv("kc_house_data.csv")
dim(kc_houses)
str(kc_houses)

##### Data Preprocessing #####

#' Our decisions on choosing the predictor variables are on interest, interpretability,
#' and the correlation analysis done

# 1. price - Price of each home sold
# 2. X bedrooms - Number of bedrooms
# 3. X bathrooms - Number of bathrooms, where .5 accounts for a room with a toilet but no shower
# 4. sqft_living - Square footage of the apartments interior living space
# 5. X sqft_lot - Square footage of the land space
# 6. X floors - Number of floors
# 7. waterfront - A dummy variable for whether the apartment was overlooking the waterfront or not
# 8. X view - An index from 0 to 4 of how good the view of the property was
# 9. X condition - An index from 1 to 5 on the condition of the apartment,
# 10. grade - An index from 1 to 13, where 1-3 falls short of building construction and design, 7 has an average level of construction and design, and 11-13 have a high quality level of construction and design.
# 11. X sqft_above - The square footage of the interior housing space that is above ground level
# 12. X sqft_basement - The square footage of the interior housing space that is below ground level
# 13. yr_built - The year the house was initially built
# 14. yr_renovated - The year of the house’s last renovation
# 15. X zipcode - What zipcode area the house is in
# 16. lat - Lattitude
# 17. long - Longitude
# 18. sqft_living15 - The square footage of interior housing living space for the nearest 15 neighbors
# 19. X sqft_lot15 - The square footage of the land lots of the nearest 15 neighbors

# Hence I will eliminate these columns for now, and proceed

library(dplyr)
# manual elimination of columns i won't be interested in
kc_houses <- kc_houses %>% 
    dplyr::select(-c(id,date,bedrooms,bathrooms,sqft_lot,floors,view,condition,
              sqft_above,sqft_basement,zipcode,sqft_lot15))

# MDA analysis 
apply(kc_houses,2, function(x) {any(is.na(x) | is.infinite(x) | is.nan(x))})
# no missing data at all!

# transform year built into interval, which is numeric and may be more informative
kc_houses <- kc_houses %>%
    mutate(yr_interval = 2015 - yr_built) %>%
    select(-c(yr_built))

# yr_renovated - we are changing it as a dichotomous variable
kc_houses <- kc_houses %>%
    mutate(yr_renovated = if_else(yr_renovated == 0, 0, 1))

boxplot(price ~ grade, data = kc_houses)
# So here, our decision is that since the first level of grades are so few
# and they are not actually changing the median of price
# we will choose to simplify every observation with less than 5 with just 5,
# as we believe this will improve the upcoming linear regression models
kc_houses <- kc_houses %>%
    mutate(grade = ifelse(grade < 5, 5, grade))

# Factorize our categorical variables waterfront, grade, and yr_renovated
kc_houses[c("grade","waterfront","yr_renovated")] <- lapply(kc_houses[c("grade","waterfront","yr_renovated")], factor)

# (b) Make a statistical description, numerical and graphical, of the data, within the problem framework.

### (b.1) price
# Histogram
p1<-ggplot(kc_houses, aes(x=price)) + geom_histogram(fill="#000000", color="#000000", alpha=0.5) + labs(x=" ", y="Frequency") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
p1
# Boxplot
p2<-ggplot(kc_houses, aes(x=price)) + geom_boxplot(color="#000000", outlier.colour="#999999", outlier.shape=8, outlier.size=2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  labs(x="Price")
p2
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
# Minimum
min(kc_houses$price)
# Maximum
max(kc_houses$price)
# Median
median(kc_houses$price)

### (b.2) sqft_living
# Histogram
p3<-ggplot(kc_houses, aes(x=sqft_living)) + geom_histogram(fill="#000000", color="#000000", alpha=0.5) + labs(x=" ", y="Frequency") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
p3
# Boxplot
p4<-ggplot(kc_houses, aes(x=sqft_living)) + geom_boxplot(color="#000000", outlier.colour="#999999", outlier.shape=8,outlier.size=2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  labs(x="Square Footage")
p4
# Pair Plot
cowplot::plot_grid(p3, p4, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
# Minimum
min(kc_houses$sqft_living)
# Maximum
max(kc_houses$sqft_living)
# Median
median(kc_houses$sqft_living)

### (b.3) waterfront
# Barplot
p5 <- ggplot(as.data.frame(table(kc_houses$waterfront)), aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity") + labs(x=" ", y="Frequency", fill="Waterfront") + scale_x_discrete(labels=c("0" = "No Waterfront", "1" = "With Waterfront")) +scale_fill_manual(labels = c("No Waterfront", "With Waterfront"),values=c("#999999", "#80CEE1"))
p5
# Frequency Table
table(kc_houses$waterfront)
# Frequency Table (%)
table(kc_houses$waterfront)/21613*100

### (b.4) grade
# Barplot
p6 <- ggplot(as.data.frame(table(kc_houses$grade)), aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity", alpha=c(1,0.15,0.15,0.15,0.15,0.15,0.15,0.15,0.15)) + labs(x=" ", y="Frequency", fill="Grade") +scale_fill_viridis(discrete = TRUE, option = "B")
p6
# Frequency Table
table(kc_houses$grade)
# Frequency Table (%)
round(prop.table(table(kc_houses$grade))*100,2)

### (b.5) yr_renovated
# Barplot
p7 <- ggplot(as.data.frame(table(kc_houses$yr_renovated)), aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat="identity") + labs(x=" ", y="Frequency", fill="Renovation")+ scale_x_discrete(labels=c("0" = "Not Renovated", "1" = "Renovated"))+scale_fill_manual(labels = c("Not Renovated", "Renovated"),values=c("#999999", "#98BF64"))
p7
# Frequency Table
table(kc_houses$yr_renovated)
# Frequency Table (%)
round(prop.table(table(kc_houses$yr_renovated))*100,2)

### (b.6) lat and long
## Histograms
# Lat
p8<-ggplot(kc_houses, aes(x=lat)) + geom_histogram(fill="#000000", color="#000000", alpha=0.5) + labs(x=" ", y="Frequency") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
p8
# Long
p9<-ggplot(kc_houses, aes(x=long)) + geom_histogram(fill="#000000", color="#000000", alpha=0.5) + labs(x=" ", y="Frequency") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
p9
## Boxplots
# Lat
p10<-ggplot(kc_houses, aes(x=lat)) + geom_boxplot(color="#000000", outlier.colour="#999999", outlier.shape=8,outlier.size=2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  labs(x="Latitude Coordinates")
p10
# Long
p11<-ggplot(kc_houses, aes(x=long)) + geom_boxplot(color="#000000", outlier.colour="#999999", outlier.shape=8,outlier.size=2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  labs(x="Longitude Coordinates")
p11
# Pair Plot
cowplot::plot_grid(p8, p9,p10,p11, ncol = 2, rel_heights = c(2, 2),align = 'v', axis = 'lr', labels="AUTO")  
## Minimum
# Lat
min(kc_houses$lat)
# Long
min(kc_houses$long)
## Maximum
# Lat
max(kc_houses$lat)
# Long
max(kc_houses$long)
## Median
# Lat
median(kc_houses$lat)
# Long
median(kc_houses$long)

# EXTRA: creating a map for geo spacial visualization
# define geo box with min and max coordinates
sbbox <- make_bbox(lon = c(-122.5, -121.3), lat = c(47.18, 47.8), f = .1)
# extract map from repository
target = get_map(location=sbbox, zoom=10, maptype="terrain")
# create map for visualization
targetmap = ggmap(target)
# displaying map
targetmap +geom_point(data = kc_houses, mapping = aes(x = long, y = lat, color=log(price)), size=0.1)+scale_color_viridis(discrete = FALSE, option = "inferno") + labs(x="Longitude Coordinates", y="Latitude Coordinates", color="Price (log)")

### (b.7) sqft_living15
# Histogram
p12<-ggplot(kc_houses, aes(x=sqft_living15)) + geom_histogram(fill="#000000", color="#000000", alpha=0.5) + labs(x=" ", y="Frequency") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
p12
# Boxplot
p13<-ggplot(kc_houses, aes(x=sqft_living15)) + geom_boxplot(color="#000000", outlier.colour="#999999", outlier.shape=8, outlier.size=2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  labs(x="Square Footage")
p13
# Pair Plot
cowplot::plot_grid(p12, p13, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")
# Minimum
min(kc_houses$sqft_living15)
# Maximum
max(kc_houses$sqft_living15)
# Median
median(kc_houses$sqft_living15)


### (b.8) yr_interval
# Histogram
p14<-ggplot(kc_houses, aes(x=yr_interval)) + geom_histogram(fill="#000000", color="#000000", alpha=0.5) + labs(x=" ", y="Frequency") + theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
p14
# Boxplot
p15<-ggplot(kc_houses, aes(x=yr_interval)) + geom_boxplot(color="#000000", outlier.colour="#999999", outlier.shape=8, outlier.size=2) + theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +  labs(x="Year Intervals")
p15
# Pair Plot
cowplot::plot_grid(p14, p15, ncol = 1, rel_heights = c(2, 1), align = 'v', axis = 'lr', labels="AUTO")
# Minimum
min(kc_houses$yr_interval)
# Maximum
max(kc_houses$yr_interval)
# Median
median(kc_houses$yr_interval)

# (c) Carefully select a final model, also including an analysis of goodness-of-fit and diagnostics of the model.
# Remark: Goodness-of-fit and diagnostics should only regard the final model, not the intermediate ones.
# The final model should be as complete as possible.

# Scatterplot matrix
pairs(kc_houses)

library(GGally)
ggpairs(kc_houses)

##### Model 1 #####
# possible transformations for latitude and longitude: square cube
# kc_houses$lat <- (kc_houses$lat)^(1/3)
# kc_houses$long <- (kc_houses$long)^(1/3) i didnt actually so far

# Initial complete model:
model_1 <- lm(price ~ ., kc_houses)

# model diagnostics
summary(model_1) # summary of the model
anova(model_1) # all terms are significant
# Note the large residual standard error
par(mfrow = c(2, 2))
plot(model_1)
#' here there is a violation of model assumptions, since the first graph,
#' shows the variance increasing in a pattern, we must transform the data
#' in order to get something out of this data, in linear regression

#' as shown previously, price, sqft_living, and sqft_living15 are high magnitude
#' variables, and their positive distribution skew, shows that logarithmization
#' would make the distributions close to symmetrical

# In this chunk of code we are computing the standardized values of each of the continuous variables
# so that alongside each model, we get standardized coefficients as well.
kc_houses_std <- kc_houses
kc_houses_std$price <- log(kc_houses_std$price)
kc_houses_std$sqft_living <- log(kc_houses_std$sqft_living)
kc_houses_std$sqft_living15 <- log(kc_houses_std$sqft_living15)
kc_houses_std[-c(3:5)] <- as.data.frame(lapply(kc_houses_std[-c(3:5)], scale))

##### Second Model #####
# Second model with transformations:
model_2 <- lm(log(price) ~ log(sqft_living) + waterfront + 
                  grade + yr_renovated + lat +
                  long + log(sqft_living15) + yr_interval, kc_houses)


# For getting standardized coefficients
model_2_std <- lm(price ~ sqft_living + waterfront + 
                  grade + yr_renovated + lat +
                  long + sqft_living15 + yr_interval, kc_houses_std)

# model diagnostics
summary(model_2) # summary of the model
anova(model_2) # all coefficients are returned significant
par(mfrow = c(2, 2))
plot(model_2) # plot the fitted values vs residuals etc
#' here there is no violation of homoscedasticity.

confint(model_2) # 95% confidence interval for coefficients in model_2

model_2_std # standardized coefficients
confint(model_2_std) # 95% confidence interval for standardized coefficients in model_2


# some manual calculations for regSS and residualSS
regressionSS <- sum(anova(model_2)[1:8,2]) # for viewing the regression sum of squares
regressionSS # 4505.883
residualSS <- anova(model_2)[9,2] # for viewing the residual sum of squares (therefore not explained by model_2)
residualSS # 1489.212

###### Residual analysis of Model 2 #####
hist(rstudent(model_2)) # aprox. symmetric
boxplot(rstudent(model_2)) # aprox. symmetric
library(car)
qqPlot(rstudent(model_2)) # slight deviation from normality
length(which(rstudent(model_2)>3.3)) # 36 points with large studentized residuals

####### VIF analysis for collinearity ######
library(regclass)
vif(model_2) # for analysing continuous variables only, none of them have values even higher than 5.
# values above 10, could immediatly indicate collinearity

##### More analysis plots on Model 2 ####
library(faraway)
halfnorm(rstandard(model_2)) # 12552 and 18333 as outliers
plot(hatvalues(model_2)) # to see some levarage points
plot(fitted.values(model_2), rstudent(model_2)) # no pattern == homocedascity

#' Now model_2 is perfectly valid and even has a decent r squared
#' but we can do better, let's look out for the outliers, interactions,
#' polynomial interactions, and exlusion of predictors, and overall let's do correlation analyses,
#' but before that we can try algorithms to automatically choose regressors
#' just for the purpose of helping us make decisions

# Using algorithms for for guidince purposes
model_null <- lm(log(price) ~ 1, kc_houses)
model_complete <- lm(log(price) ~ log(sqft_living) + waterfront + 
                         grade + yr_renovated + lat +
                         long + log(sqft_living15) + yr_interval, kc_houses)

# backwards selection
step.backward <- step(model_complete, 
                      scope = list(lower = model_null, upper = model_complete),
                      trace = TRUE,
                      direction="backward")

summary(step.backward)

# forward selection
step.forward <- step(model_null, 
                     scope = list(lower = model_null, upper = model_complete),
                     trace = TRUE,
                     direction="forward")

summary(step.forward)

# forward selection
step.stepwise <- step(model_null, 
                      scope = list(lower = model_null, upper = model_complete),
                      trace = TRUE,
                      direction="both")

summary(step.stepwise)
# All 3 methods agree on keeping all regressors

# this section is for correlations:
cor(kc_houses[-c(3:5)], method = "pearson") 
# log(sqft_living) and log(sqft_living15) are highly correlated (=.75), so we will
# drop the variable that the coefficient in the linear model
# had the lowest t value which is sqft_living15

##### Model 3 #####
# Third model w/o sqft_living15
model_3 <- lm(log(price) ~ log(sqft_living) + waterfront + 
                  grade + yr_renovated + lat +
                  long + yr_interval, kc_houses)

# For getting standardized residuals
model_3_std <- lm(price ~ sqft_living + waterfront + 
                  grade + yr_renovated + lat +
                  long + yr_interval, kc_houses_std)

summary(model_3) # summary of the model
anova(model_3) # all variables significant

# An F test is performed, since model_2 and model_3 are nested models
anova(model_3,model_2) # highly significant means that we can reject that both models have
# the same goodness of fit, therefore model_2 has better GOF according to test...
# However, a change of 8% of the estimate of log(sqft_living) is detected, alongside with a high correlation
# detected between the two predictors. Hence we exclude the predictor that has the lowest t value, log(sqft_living15).

#' long in model_3 has a relatively low t value. To see its impact in the model compared with the rest of 
#' the predictor, we took a look into the standarsized coefficients.
model_3_std
#' A seen here, long is very close to zero, even when standardized
confint(model_3)
confint(model_3_std)
# a wide interval is also present in long
#' Additionally, in the visualizations above, we saw in the map graph that price does not seem to have a strong
#' relationship with long.
#' longitute's coefficient was therefore removed as we proceed with model_4

model_4 <- lm(log(price) ~ log(sqft_living) + waterfront + 
                  grade + yr_renovated + lat 
              + yr_interval, kc_houses) # now w/o long

# For getting standardized coefficients
model_4_std <- lm(price ~ sqft_living + waterfront + 
                      grade + yr_renovated + lat +
                       yr_interval, kc_houses_std)

# F test, since they are nested models
anova(model_4,model_3) # well now F statistic calculated in the sample, does not have a very high value
# compared with other tests performed in this work, though it is stated as significant (1.261% values as extreme
# as this one or more extreme, given that the two models have the same goodness of fit)

summary(model_4) # summary of model_4
anova(model_4) # all coefficients significant

model_4_std # standardized regression coefficients

confint(model_4) # 95% confidence interval
confint(model_4_std) # 95% C.I. for standardized estimates

# Furthermore, we also evaluated model_3 and model_4 on the Bayesian Information Criteria (BIC).
extractAIC(model_3,k = log(nrow(kc_houses))) # -57148.36
extractAIC(model_4,k = log(nrow(kc_houses))) # -57152.12

##### Model 4 analyses #####

anova(model_4) # all terms returned significant
par(mfrow = c(2, 2))
plot(model_4)
# no violation of assumptions, slight deviation from normality won't be an issue due to 
# central limit theorem. Some points with high leverages can be viewed here.

###### Residual analysis of Model 4 #####
hist(rstandard(model_4)) # aprox. symmetric distribution
boxplot(rstandard(model_4))
library(car)
qqPlot(rstudent(model_4))

# analysis of residuals vs the observation index, to see if observations are independent
plot(rstandard(model_4)) # seems like random variation from zero, indicating independence of observations

####### VIF analysis for collinearity ######
library(regclass)
vif(model_4) # None continuous variables have very high VIF values

##### More graphical analyses on Model 4 ####
library(faraway)
halfnorm(rstandard(model_4)) # points 412 and 12552 identified as outliers
# Let's manually check these outliers
kc_houses[412,] 
kc_houses[12552,] # extreme values, they present a very low sqft_living pf 833 and 790, but those are most definetly possible
plot(hatvalues(model_4)) # leverage graph
plot(fitted.values(model_4), rstandard(model_4)) # no pattern

##### Outlier analysis #####

# outlier values
high_res_values <- kc_houses[which(rstandard(model_4)>3.3),] # 51 observations
# These values were explored, but none seem too out of place

# we will refit model_4 without these points and compare
# clone the dataset:
kc_houses_test <- kc_houses
# removing outlier points
kc_houses_test <- kc_houses_test[-which(rstandard(model_4)>3.3),]

model_4_test <- lm(log(price) ~ log(sqft_living) + waterfront + 
                  grade + yr_renovated + lat 
              + yr_interval, kc_houses_test) # using the modified dataset

summary(model_4)
summary(model_4_test)
# The models are not too different, therefore the model's conclusion
# is not too reliable on these outliers, and thus they will not be removed.
# In addition, manually checking these outliers, we saw that they were all possible
# and not just mistakes

# calculating the high leverage values
high_leverage_values <- kc_houses[which(hatvalues(model_4) >= (2 * (13+1))/nrow(kc_houses)),] # 1531
# calculating cook's distance for all points (therefore also having the high leverage points calculated)
cook_d<- cooks.distance(model_4)

# We want to see if any point has a cook's distance above 1
plot(cook_d, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# It is not the case, and therefore, no outliers were excluded in this study

# looking at the data, they are extreme values.. but they are valid, they are influential
# and should not be removed.

# (d) Give the mathematical equation of the final model

# done in the pdf

# (e) For a continuous variable X1 and a categorical variable with more than 2 categories X2 in the final model
# (e.1) interpret the crude effect of X1 and the adjusted effect of X2

# The coefficients (from simple linear regressions) are usually called crude effect, i.e., only having one predictor
# variable, and thus does not have an adjusted effect. In the context of X 1 , log(sqft_living)’s effect on
# log(price) has not been adjusted on the remaining variables.

#Crude effect of the variables X1
grade6    <- ifelse(kc_houses$grade == '6', 1, 0)
grade7    <- ifelse(kc_houses$grade == '7', 1, 0)
grade8    <- ifelse(kc_houses$grade == '8', 1, 0)
grade9    <- ifelse(kc_houses$grade == '9', 1, 0)
grade10   <- ifelse(kc_houses$grade == '10', 1, 0)
grade11   <- ifelse(kc_houses$grade == '11', 1, 0)
grade12   <- ifelse(kc_houses$grade == '12', 1, 0)
grade13   <- ifelse(kc_houses$grade == '13', 1, 0)

# kc_dummies 
kc_dummies <- data.frame(price = log(kc_houses$price), sqft_living = log(kc_houses$sqft_living), waterfront=kc_houses$waterfront, yr_renovated=kc_houses$yr_renovated, yr_interval=kc_houses$yr_interval, lat=kc_houses$lat,
                         grade6=grade6, grade7=grade7, grade8=grade8, grade9=grade9, grade10=grade10, grade11=grade11, grade12=grade12, grade13=grade13)

#Crude effect of the variables X1
model_crude_sqft <- lm(log(price) ~ log(sqft_living), data = kc_houses)
model_crude_sqft

# Adjusted effect of the variable X2
model_4
#' The coefficients (from multiple regressions) are usually called 'adjusted effects' 
#' when there is more than a variable and, as such, the equation has been adjusted for 
#' other predictive variables of interest. In this example, we use the adjusted coefficients
#' of model_4. The interpretation of, for instance, grade6 is that, in average, price
#' varies 100 x ($e^{0.098268}$-1) = 10.33% between the reference category (grade5) 
#' and the comparison category, grade6, after adjusting for the remaining explanatory 
#' variables. This explanation holds for every coefficient of grade. 

# Final Model (+dummies)
model_4_dum <- lm(price ~ sqft_living + waterfront + grade6 + grade7 + grade8 + grade9 + grade10 + grade11 + grade12 + grade13 + yr_renovated + lat + yr_interval, kc_dummies)
model_4_dum

model_crude_grade <- lm(log(price) ~ grade, kc_houses)

# (e.2) plot confidence and prediction bands for the response as a function of X1 values, fixing the continuous
# explanatory variables at its medians and the categorical explanatory variables at its modes.

Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
}

Modes(kc_houses$grade)            #7
Modes(kc_houses$waterfront)       #0
Modes(kc_houses$yr_renovated)     #0

#Median
median(kc_houses$sqft_living)     #7.554859 Chosen X1
median(kc_houses$lat)             #47.5718
median(kc_houses$yr_interval)     #40

#Calculate the Prediction and Confidence Bands #price added for plotting
newdata <- data.frame(price = kc_dummies$price, sqft_living = kc_dummies$sqft_living, grade6=0, grade7=1, grade8=0, grade9=0, grade10=0, grade11=0, grade12=0, grade13=0, waterfront=0, yr_renovated=0, lat=47.5718, yr_interval=40)

newdata$waterfront <- as.factor(newdata$waterfront)
newdata$yr_renovated <- as.factor(newdata$yr_renovated)

conf.int <- predict(model_4_dum, newdata, interval="confidence") #We're 95%confident that the expected value of price is between ...
pred.int <- predict(model_4_dum, newdata, interval="prediction") #We're 95%confident that the actual price with these characteristics is between ...

#' The R objects created conf.int and pred.int are matrices of prediction, with the upper and lower bound
#' set as a confidence interval or prediction interval, respectively. With the information stored in these two
#' objects, we can plot the scatterplot of log(sqft_living) in response of log(price)

mydata_1 <- cbind(newdata, conf.int)

#Pred.int
mydata_2 <- cbind(newdata, pred.int)

#BOTH
test <- data.frame(upr_p = mydata_2$upr, lwr_p = mydata_2$lwr, fit_p = mydata_2$fit)
mydata_3 <- cbind(mydata_1, test)

p <- ggplot(mydata_3, aes( sqft_living, price)) + theme_bw()+ 
    geom_point()
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(y = upr), color = "red", linetype = "dashed")+
    geom_line(aes(y = fit), color = "blue")+ 
    geom_line(aes(y = lwr_p), color = "darkgreen", linetype = "dashed")+
    geom_line(aes(y = upr_p), color = "darkgreen", linetype = "dashed")+
    geom_ribbon(data=subset(mydata_3), 
                aes(ymin=upr,ymax=lwr), fill="red", alpha=0.35)+
    geom_ribbon(data=subset(mydata_3), 
                aes(ymin=upr_p,ymax=lwr_p), fill="darkgreen", alpha=0.35)

# (e.3) interpret the effect induced by a change from the 2nd category of X2 to the 3rd category, and give a
# 95% confidence interval for that effect.

#' For this exercise, the second category of grade is grade6, and the third category is grade7. 
#' For the purpose of studying the change in price from the change from grade6 to grade7,
#'  we will fix the remaining explanatory variables at their medians and modes.

newdata_1 <- data.frame(sqft_living = 7.554859, grade6=1, grade7=0, grade8=0, grade9=0, grade10=0, grade11=0, grade12=0, grade13=0, waterfront=0, yr_renovated=0, lat=47.5718, yr_interval=40) #grade 6
newdata_2 <- data.frame(sqft_living = 7.554859, grade6=0, grade7=1, grade8=0, grade9=0, grade10=0, grade11=0, grade12=0, grade13=0, waterfront=0, yr_renovated=0, lat=47.5718, yr_interval=40) # grade 7

newdata_1$waterfront <- as.factor(newdata_1$waterfront)
newdata_1$yr_renovated <- as.factor(newdata_1$yr_renovated)
newdata_2$waterfront <- as.factor(newdata_2$waterfront)
newdata_2$yr_renovated <- as.factor(newdata_2$yr_renovated)

predict(model_4_dum, newdata_1, interval="confidence")
predict(model_4_dum, newdata_2, interval="confidence")

#' The values of fit in both instances can now be compared. By taking their ratio, 
#' 100 * (exp(12.89599)/exp(12.72066)-1) = 19.16%, means that, in average, price will 
#' increase by 19.16% from a house with a change of grade6 to grade7. 
#' The 95% confidence interval, as seen by this effect, is calculated with the same
#' type of calculations for the ratio [100 * (exp(12.88982)/exp(12.70643)-1),100 * 
#' (exp(12.90217)/exp(12.73487)-1)] = [18.21%,20.12%].

# (e.4) interpret the effect induced by an increase of two standard deviations in X1

# To answear this question, the standardized coefficients must be calculated.

model_4_std # standardized coefficients from model_4

#' In model_4, price (response) and sqft_living are both log transformed, thus will 
#' cause some changes in interpretation. The unstandardized coefficient of 
#' log(sqft_living) in model_4 means that a aproximately 1%  of change in 
#' sqft_living will induce a change of around 0.47% in price, adjusting for the 
#' remaning explanatory variables. When the data is scaled prior to regression, 
#' a change of 1 standard deviation in log(sqft_living) is equivalent to 0.3788 
#' standard deviations in the mean of log(price), and therefore a change of 
#' 2 standard deviations will affect the mean of log(price) by 0.7576 standard deviations,
#'  adjusting for the remaining explanatory variables.


# (e.5) study the existence of a statistical significant interaction between X1 and X2. Independently from its
# statistical significance, interpret those interaction effects.

#' model_4 was changed to include this interaction, along with their base predictors, 
#' thus creating model_4_interaction.

model_4_interaction <- lm(log(price) ~ sqft_living + waterfront + 
                              grade + yr_renovated + lat 
                          + yr_interval + sqft_living * grade, kc_houses)

summary(model_4_interaction)

model_4_interaction
model_4

#' The very low coefficients (and standardized coefficients) of this model suggests there 
#' is not a prevalent interaction between these variables. Independently from this 
#' conclusion, the interpretation is that, for instance, in the interaction 
#' log(sqft_living):grade6, the effect of log(sqft_living) on log(price) will increase
#'  by approximately 0.061% from grade5 to grade6, after adjusting for the remaining 
#'  explanatory variables. This holds for the rest of the interactions. 
#'  As another example, in the interaction log(sqft_living):grade13, means that the 
#'  effect of log(sqft_living) on log(price) will increase by approximately 0.64% 
#'  from grade5 to grade13, after adjusting for the remaining explanatory variables.
