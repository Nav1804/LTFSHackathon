########## Objective ###########
# To forecast the number of cases/applications for next 3 months

##### Setting Working Directory #####
setwd("C:/Users/NAVIN/Desktop/LTFS Hackathon Data Set")

##### Libraries ########
## Data Cleaning
library(readr)
library(dplyr)
library(lubridate) # Working with Dates
library(reshape)
library(reshape2)# Reshaping the data
library(tidyr)
library(tidyverse)

## Extracting list of Holidays
# bizdays
# timedate
# RQuantLib
library(RQuantLib)
library(bizdays)
library(timeDate)

## Visualizations
library(ggplot2)
library(plotly)

### ARCH ###
library(prophet)

## Model Building and Forecasting
library(timeSeries)
library(fpp2)
library(forecast)

##### Reading Data Set ##########
dat_train = data.frame(read.csv("fintch.csv"))
dat_test = data.frame(read.csv("fintch test.csv"))
dat_test$application_date<-as.Date(dat_test$application_date,format = "%d-%m-%Y")

##### Checking Structure and Summary of the data ######
str(dat_train)
summary(dat_train)

### Checking percentage of missing values across different columns 
colSums(is.na(dat_train))/nrow(dat_train)*100

##### Variable conversion #######
# Converting applicate date into relevant date format.
dat_train$application_date<-as.Date(dat_train$application_date,format = "%d-%m-%Y")
dat_train$segment<-as.character(dat_train$segment)
str(dat_train)

## Sorting the dataset as per Date and Segment
dat_train<-dat_train[order(dat_train$segment,dat_train$application_date),]

## Creating month variable
dat_train$yr<-year(dat_train$application_date)
dat_train$mnth<-month(dat_train$application_date)
dat_train$qrtr<-quarters(dat_train$application_date)
dat_train$yr_mnth<-factor(paste(dat_train$yr,dat_train$mnth,sep = ""))
dat_train$yr_quarter<-factor(paste(dat_train$yr,dat_train$qrtr,sep ="-"))
dat_train$week<-week(dat_train$application_date)

## Visualization ##
# Dataset
library(MASS)
mdl_dt<-dat_train[,c("segment","application_date","case_count")]

#### Number of cases/Applications 
## Daily trend
cs_trend<-mdl_dt%>%group_by(segment,application_date)%>%summarise(No_cases = sum(case_count))

## Plot - Daily Trend
ggplot(cs_trend,aes(x = application_date,y = No_cases,color = segment))+geom_line(stat = "identity")+labs(title = "Number of cases")+scale_x_date(date_labels = "%b-%Y")+facet_grid(segment~.,scale = "free")

## Monthly Trend
# mn_trend<-aggregate(case_count~mnth+yr+segment,data = dat_train,sum)
# mn_trend$yr_mnth<-paste(mn_trend$yr,mn_trend$mnth,sep = "-")
# mn_trend$yr_mnth<-as.Date(mn_trend$yr_mnth,format = "%b %Y")

## Plot
# ggplot(mn_trend,aes(x = yr_mnth,y = case_count,color = segment))+geom_line(stat = "identity")+labs(title = "Number of cases - Monthly Trend")+facet_grid(segment~.,scale = "free")

# Number of cases from APR'2017 to JUL'2019
fin_cs<-mdl_dt %>% group_by(segment,application_date) %>% summarise(No_cases = sum(case_count))

######### Visualisation for checking seasonality ###########

# Segmenting data
seg_1<-subset(fin_cs,fin_cs$segment == 1)
seg_1$segment<-NULL

seg_2<-subset(fin_cs,fin_cs$segment == 2)
seg_2$segment<-NULL

##### Train and Test Split ######
# Segment 1
# Converting to xts object
seg1ts<- ts(seg_1[,2], frequency=365, start=c(2017,90),end = c(2019,165))
seg1ts_tr<-ts(seg1ts[1:(0.75*nrow(seg1ts))])
seg1ts_ts<-ts(seg1ts[(0.75*nrow(seg1ts)):nrow(seg1ts)])

# Segment 2
## Converting to xts object
library(xts)
seg2ts<- ts(seg_2[,2], frequency=365, start=c(2017,90),end = c(2019,203))
seg2ts_tr<-ts(seg2ts[1:(0.75*nrow(seg2ts))])
seg2ts_ts<-ts(seg2ts[(0.75*nrow(seg2ts)):nrow(seg2ts)])

seg2<-xts(seg_2[,-1],order.by = as.POSIXct(strptime(seg_2$application_date, "%Y-%m-%d")))

seg2.train<-window(seg2,end = "2018-12-31")
seg2.test<-window(seg2,start = "2019-01-01")

##### Model Building ########
## Segment 2 ##
######## Holtwinters Triple Exponential Smoothing ##########
## Decomposition of Time Series ##
decmp_seg2<-decompose(seg2ts,type = "additive")
autoplot(decmp_seg2)

## Holtwinters Double Exponential Smoothing
fit_ets<-ets(seg2.train,model = "AAN")
fit_ets
ft<-forecast.ets(fit_ets,h = 204)

accuracy(ft,seg2.test)


# Optimum value of Beta
beta <- seq(0.001, 0.05, 0.001)
RMSE<-NA
for(i in seq_along(beta)) {
  hw.expo <- ets(seg2.train, model = "AAN", alpha = 0.9919,beta = beta[i])
  future <- forecast.ets(hw.expo, h = 204)
  RMSE[i] = accuracy(future, seg2.test)[2,2]
}

error <- data_frame(beta, RMSE)
minimum <- filter(error, RMSE == min(RMSE))
ggplotly(ggplot(error, aes(beta, RMSE)) +
           geom_line() +
           ggtitle("beta's impact on forecast errors"))

## Building model with different alpha value
fit_hw<-ets(seg2.train,model = "AAN",alpha = 0.9919,beta = 0.0011)
summary(fit_hw)
checkresiduals(fit_hw)

future<-forecast.ets(fit_hw,h = 204)

# Model Accuracy #
accuracy(future,seg2.test)

seg2.test[1:20]
############## Segment 1 Model #############
library(prophet)
seg_1_tr<-seg_1[1:(0.80*nrow(seg_1)),]
names(seg_1_tr)<-c("ds","y")

seg_1_ts<-seg_1[(0.80*nrow(seg_1)+1):nrow(seg_1),]
names(seg_1_ts)<-c("ds","y")

dat_tst<-data.frame(dat_test[dat_test$segment == 1,2])
names(dat_tst)<-"ds"

### Model Building ##
prp_mdl<-prophet(seg_1_tr,seasonality.mode = "multiplicative")
forecst1 = data.frame(predict(prp_mdl,seg_1_ts))
head(forecst1$yhat)
accuracy(forecst1$yhat,seg_1_ts$y)

################## Final Prediction #################
# Segment 1 #
seg1<-data.frame(seg_1)
names(seg1)<-c("ds","y")

dat_ts1<-dat_test[dat_test$segment == 1,2:3]
names(dat_ts1)<-c("ds","y")

prp_mdl1<-prophet(seg1,seasonality.mode = "multiplicative")
forecst1 = data.frame(predict(prp_mdl1,dat_ts1))

dat_ts3<-dat_test[dat_test$segment == 1,]
dat_ts3<-cbind(dat_ts3,case_count = round(forecst1$yhat))
names(dat_ts3)<-c("id","application_date","segment","case_count")
# Segment 2 #
fit_hlw<-ets(seg2.train,model = "AAN",alpha = 0.9919,beta = 0.0004)
summary(fit_hlw)

forcst_val<-data.frame(forecast.ets(fit_hlw,h = 297,alpha = 90))
nrow(forcst_val)
case_count<-data.frame(forcst_val$Point.Forecast[205:297])

dat_ts<-dat_test[dat_test$segment == 2,]
dat_ts<-cbind(dat_ts,case_count = round(case_count))
names(dat_ts)<-c("id","application_date","segment","case_count")

dat_t<-rbind(dat_ts3,dat_ts)
write.csv(dat_t,"dat_test2.csv")
