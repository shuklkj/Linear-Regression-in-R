# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("eeptools")
# install.packages("lmtest")
# install.packages("relaimpo")
# install.packages("Metrics")
# install.packages("olsrr")

library(dplyr)
library(tidyverse)
library(eeptools)
require(lmtest)
library(olsrr)




data_2016 <- read.csv('~/Desktop/Final_Assignment/2016_brooklyn.csv')
data_2017 <- read.csv('~/Desktop/Final_Assignment/2017_brooklyn.csv')
data_2018 <- read.csv('~/Desktop/Final_Assignment/2018_brooklyn.csv')
data_2019 <- read.csv('~/Desktop/Final_Assignment/2019_brooklyn.csv')
data_2020 <- read.csv('~/Desktop/Final_Assignment/2020_brooklyn.csv')

# ---------------------------------------------------------------------
#   1.1---Standardize column names 
# ---------------------------------------------------------------------
col_names = c('borough','neighborhood','bldclasscat','taxclasscurr','block',
              'lot','easement','bldclasscurr','address','aptnum','zip',
              'resunits','comunits','totunits','landsqft','grosssqft'
              ,'yrbuilt','taxclasssale','bldclasssale','price','date') 
names(data_2016) <- c(col_names)
names(data_2017) <- c(col_names)
names(data_2018) <- c(col_names)
names(data_2019) <- c(col_names)
names(data_2020) <- c(col_names)

# removed first 4 rows
data_2016 <- data_2016[-c(1:4), ]
data_2017 <- data_2017[-c(1:4), ]
data_2018 <- data_2018[-c(1:4), ]
data_2019 <- data_2019[-c(1:4), ]
data_2020 <- data_2020[-c(1:7), ]


#-----------------------------------2016----------------------------------

data_2016$neighborhood <- trimws(data_2016$neighborhood, which = c("both"))

data_2016$price <- gsub(',', '', data_2016$price)
data_2016$price <- gsub('-', '', data_2016$price)
data_2016$price <- as.integer(data_2016$price)

data_2016$landsqft <- gsub(',', '', data_2016$landsqft)
data_2016$landsqft <- as.integer(data_2016$landsqft)

data_2016$grosssqft <- gsub(',', '', data_2016$grosssqft)
data_2016$grosssqft <- as.integer(data_2016$grosssqft)

data_2016$resunits <- as.integer(data_2016$resunits)
data_2016$totunits <- as.integer(data_2016$totunits)
data_2016$yrbuilt <- as.integer(data_2016$yrbuilt)

data_2016$date <- as.Date(data_2016$date,format = "%m/%d/%y")




#-----------------------------------2017----------------------------------

data_2017$neighborhood <- trimws(data_2017$neighborhood, which = c("both"))

data_2017$price <- gsub(',', '', data_2017$price)
data_2017$price <- as.integer(data_2017$price)

data_2017$landsqft <- gsub(',', '', data_2017$landsqft)
data_2017$landsqft <- as.integer(data_2017$landsqft)

data_2017$grosssqft <- gsub(',', '', data_2017$grosssqft)
data_2017$grosssqft <- as.integer(data_2017$grosssqft)

data_2017$resunits <- as.integer(data_2017$resunits)
data_2017$totunits <- as.integer(data_2017$totunits)
data_2017$yrbuilt <- as.integer(data_2017$yrbuilt)

data_2017$date <- as.Date(data_2017$date,format = "%m/%d/%y")




#-----------------------------------2018----------------------------------

data_2018$neighborhood <- trimws(data_2018$neighborhood, which = c("both"))

data_2018$price <- gsub(',', '', data_2018$price)
data_2018$price <- substring(data_2018$price, 2)
data_2018$price <- as.integer(data_2018$price)

data_2018$landsqft <- gsub(',', '', data_2018$landsqft)
data_2018$landsqft <- as.integer(data_2018$landsqft)

data_2018$grosssqft <- gsub(',', '', data_2018$grosssqft)
data_2018$grosssqft <- as.integer(data_2018$grosssqft)

data_2018$resunits <- as.integer(data_2018$resunits)
data_2018$totunits <- as.integer(data_2018$totunits)
data_2018$yrbuilt  <- as.integer(data_2018$yrbuilt)

data_2018$date <- as.Date(data_2018$date,format = "%m/%d/%y")





#-----------------------------------2019----------------------------------

data_2019$neighborhood <- trimws(data_2019$neighborhood, which = c("both"))

data_2019$price <- gsub(',', '', data_2019$price)
data_2019$price <- as.integer(data_2019$price)

data_2019$landsqft <- gsub(',', '', data_2019$landsqft)
data_2019$landsqft <- as.integer(data_2019$landsqft)

data_2019$grosssqft <- gsub(',', '', data_2019$grosssqft)
data_2019$grosssqft <- as.integer(data_2019$grosssqft)

data_2019$resunits <- as.integer(data_2019$resunits)
data_2019$totunits <- as.integer(data_2019$totunits)
data_2019$yrbuilt  <- as.integer(data_2019$yrbuilt)

data_2019$date <- as.Date(data_2019$date,format = "%m/%d/%y")



#-----------------------------------2020----------------------------------

data_2020$neighborhood <- trimws(data_2020$neighborhood, which = c("both"))

data_2020$price <- gsub(',', '', data_2020$price)
data_2020$price <- as.integer(data_2020$price)

data_2020$landsqft <- gsub(',', '', data_2020$landsqft)
data_2020$landsqft <- as.integer(data_2020$landsqft)

data_2020$grosssqft <- gsub(',', '', data_2020$grosssqft)
data_2020$grosssqft <- as.integer(data_2020$grosssqft)

data_2020$resunits <- as.integer(data_2020$resunits)
data_2020$totunits <- as.integer(data_2020$totunits)
data_2020$yrbuilt  <- as.integer(data_2020$yrbuilt)

data_2020$date <- as.Date(data_2020$date,format = "%m/%d/%y")




# ---------------------------------------------------------------------
#    1.2---Join data 
# ---------------------------------------------------------------------
final_big_data <- rbind(data_2016, data_2017, data_2018, data_2019, data_2020)

# ---------------------------------------------------------------------
#    1.3---Filter data 
# ---------------------------------------------------------------------

final_big_data <- final_big_data %>% filter(
  (str_detect(bldclasssale, "A") | str_detect(bldclasssale, "R"))  &
    resunits == 1 & totunits == 1 & grosssqft > 0 & !is.na(price))

nrow(final_big_data)



# ---------------------------------------------------------------------
#                   2.1 ---EDA
# ---------------------------------------------------------------------

#new df for the modeling

str(final_big_data)
new_data = final_big_data[-c(1,5,6,7,9,10,12,13,14)]
str(new_data)
nrow(new_data)


new_data$neighborhood = as.factor(new_data$neighborhood)
new_data$bldclasscat = str_squish(new_data$bldclasscat)
new_data$bldclasscat = as.factor(new_data$bldclasscat)
new_data$bldclasscurr = as.factor(new_data$bldclasscurr)
new_data$zip = as.factor(new_data$zip)
new_data$landsqft = as.numeric(new_data$landsqft)
new_data$grosssqft = as.numeric(new_data$grosssqft)
new_data$yrbuilt = as.numeric(new_data$yrbuilt)
new_data$taxclasscurr = as.factor(new_data$taxclasscurr)
new_data$taxclasssale = as.factor(new_data$taxclasssale)
new_data$bldclasssale = as.factor(new_data$bldclasssale)
new_data$price = as.numeric(new_data$price)

str(new_data)
nrow(new_data)


##########################################################################################
#---------------------------------------------------------------------------
new_data <- new_data[order(new_data$price, decreasing = TRUE), ]  # Order data descending
nrow(new_data)

price_out5 = (new_data %>% group_by(zip) %>% slice(which.max(price)))$price

new_data = new_data[-which(new_data$price %in% price_out5),]
nrow(new_data)   #13591

boxplot(new_data[, c('price')])

nrow(new_data)   #13591
boxplot(new_data[, c('price')])

price_out6 = (new_data %>% group_by(zip) %>% slice(which.min(price)))$price
new_data = new_data[-which(new_data$price %in% price_out6),]
nrow(new_data) #13232

price_out7 = (new_data  %>% group_by(zip) %>% slice(which.max(price)))$price[1:5]
new_data = new_data [-which(new_data $price %in% price_out7),]
nrow(new_data ) #13209

model = lm( price ~ ., new_data)
summary(model)
sqrt(mean(model$residuals^2))

nrow(new_data) #13209
price_out8 = (new_data  %>% group_by(zip) %>% slice(which.min(price)))$price
new_data  = new_data [-which(new_data $price %in% price_out8),]

### NUMBER OF OBSERVATIONS USED IN MY REGRESSION MODEL
nrow(new_data ) #13001
dim(new_data)

hist(new_data$price)    #skewed to the right




########################---------------------------------------------##############################
###################################### LINEAR REGRESSION MODEL  ################################## 
########################---------------------------------------------##############################

nrow(new_data)
model = lm( price ~ ., new_data)
summary(model)
sqrt(mean(model$residuals^2))

model1 = lm( price ~ neighborhood + bldclasscat   + zip + landsqft + grosssqft , new_data)
summary(model1)
sqrt(mean(model1$residuals^2))


step(model,direction='backward')

nrow(new_data)
model2 = lm(formula = price ~ neighborhood + bldclasscurr + zip + landsqft + grosssqft  + date, data = new_data)
summary(model2)
sqrt(mean(model2$residuals^2))
plot(model2$residuals)


nrow(new_data)
model3 = lm(formula = price ~ neighborhood  + zip + landsqft + grosssqft + date, data = new_data)
summary(model3)
sqrt(mean(model3$residuals^2))
plot(model3, which=1)
plot(model3$residuals)

str(new_data)
############################## Transformation ##############################
nrow(new_data)
model4 = lm(formula = price ~ neighborhood  + zip*grosssqft + landsqft + grosssqft + zip , data = new_data)
summary(model4)
sqrt(mean(model4$residuals^2))
plot(model4, which = 1)


model5 = lm(formula = price ~ zip*grosssqft + landsqft + grosssqft , data = new_data)
summary(model5)
sqrt(mean(model5$residuals^2))
plot(model5, which = 1)


nrow(new_data)
str(new_data)
new_data$zip = as.numeric(new_data$zip)
str(new_data)


######################### FINAL MODEL ########################################
hist(sqrt(new_data$price))    
hist(new_data$zip* new_data$grosssqft)
hist(new_data$zip)

brooklyn_linear_model = lm(formula = sqrt(price) ~ zip*grosssqft  + factor(zip) , data = new_data)
summary(brooklyn_linear_model) #  df: 39 and adjusted r2: 0.6191
rmse <- sqrt(mean(brooklyn_linear_model$residuals^2))
rmse # 193.8391
plot(brooklyn_linear_model$fitted.values,brooklyn_linear_model$residuals)
ks.test(brooklyn_linear_model$residuals/summary(brooklyn_linear_model)$sigma,pnorm)   # p-value < 2.2e-16

###########################################################################


########################---------------------------------------------##############################
###################################### OLS MODEL ASSUMPTION  ################################## 
########################---------------------------------------------##############################

# Plot linear model 
ggplot(new_data, aes(x=zip*grosssqft + zip, y=sqrt(price)))  +
  labs(title = 'Brooklyn Housing Prices', 
       x='Interaction of Gross Square Feet and ZipCode', y='Sale Prices') +
  geom_smooth(method='lm', formula= y~x, color="blue")
sqrt(mean(brooklyn_linear_model$residuals^2))


# plot the residuals of the linear model
ols_plot_resid_fit(brooklyn_linear_model)
ols_plot_resid_qq(brooklyn_linear_model)
ols_plot_resid_hist(brooklyn_linear_model)

# Test the linear model 
res <- resid(brooklyn_linear_model)
ols_test_normality(brooklyn_linear_model)

# Normality
plot(ecdf(res),main='Empirical CDF of residuals')
interval1 = (max(res)-min(res))/length(res)
plot(ecdf(res),main='Empirical CDF of residuals')
lines(x=seq(min(res),max(res),interval1),y=pnorm(seq(min(res),max(res),interval1),mean(res),sqrt(var(res))),col='blue')

shapiro.test(brooklyn_linear_model$residuals)
ks.test(res, pnorm) #normal distribution

# Serial correlation
require(lmtest)
dwtest(brooklyn_linear_model)

plot(res,type="l")
length(res)#1314
plot(x=res[-1314],y=res[-1],
     xlab='Lagged values',ylab='Current values',main = "Serial correlation")

# heteroscedasticity:
bptest(brooklyn_linear_model)
car::ncvTest(brooklyn_linear_model)



#IID
sqrt(mean((brooklyn_linear_model$fitted.values - new_data$price)^2))
require(lmtest)
hist(brooklyn_linear_model$residuals, breaks = 100)
ks.test(brooklyn_linear_model$residuals/summary(model)$sigma, pnorm)
bptest(brooklyn_linear_model)
dwtest(brooklyn_linear_model)
plot(brooklyn_linear_model$residuals, brooklyn_linear_model$residuals)
acf(brooklyn_linear_model$residuals)


dim(new_data)









############ SAVE THE FILE #####################

write_csv(new_data, "~/Desktop/Final_Assignment/brooklyn_linear_model.csv")
brooklyn_linear_model_data<- read.csv("~/Desktop/Final_Assignment/brooklyn_linear_model.csv")
head(brooklyn_linear_model_data)

saveRDS(list(model=brooklyn_linear_model, data=new_data), file='~/Desktop/Final_Assignment/kajalshukla.RDS') 











































