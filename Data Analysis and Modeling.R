#############################################################
#############################################################
#                   CODE FOR DATA TABLES                    #    
#############################################################
#############################################################

#Importing required packages
library(dplyr)
library(knitr)
library(tidyverse)
library(sqldf)
library(glmnet)
library(leaps)
library(stargazer)
library(kableExtra)
library(haven)
library(Hmisc)
library(readxl)
#library(sjPlot)

#Importing StubHub data set

stubhub_panel <- read_csv("Stubhub_Data_Final_V2.csv")

####### Dropping variables we don't need to create a data frame with the necessary variables #######

stubhub <- stubhub_panel
names(stubhub)

drop <- c("numrec", "rec", "noteach", "notonmap", "notonmapx",
          "suite", "nostdsection", "nostdrow", "numb",
          "neweventidhtml", "multiplegamedates", "homepriortofirstgame",
          "awaypriortofirstgame", "gamedownfe", "dtg", "gamepercentile",
          "sectionpercentile", "percgrp", "remainder", "rowna", "y",
          "numbsection", "orderrowx", "orderrow", "realnumb", "avday",
          "EBlist", "EBpossover", "EBalllist", "EBallpossover", "EBregpnlist",
          "EBregplist", "SHlist", "SHnumbpossover", "SHalllist", "SHallpossover",
          "SHregpnlist", "SHregplist", "_merge", "gamefex", "x", "sumx",
          "EBlistdum", "EBalllistdum", "EBregpnlistdum", "EBregplistdum",
          "EBlist2", "EBalllist2", "EBregpnlist2", "EBregplist2",
          "SHlist2", "SHalllist2", "SHregpnlist2", "SHregplist2")


stubhub2 <- stubhub

#Gettign the description of important variables for summary stats
seats_desc <- describe(as.numeric(stubhub2$number))
face_value_desc <- describe(as.numeric(stubhub2$regprice))
posted_price_desc <- describe(as.numeric(stubhub2$sellerpricenum))
transaction_price_desc <- describe(as.numeric(stubhub2$pricenum))
attendance_desc <- describe(as.numeric(stubhub2$att))

as.numeric(stubhub$number)
summary(stubhub$number)

#Extracting only those variables for which we want to analyze summary stats
summary_stats <- stubhub2[, c("numb", "regprice", "sellerpricenum", "att",  "pricenum")]

#Converting all entries in the summary stats table to numeric format
summary_stats$number < - as.numeric(as.character(summary_stats$number))
summary_stats$'Ticket Face Value' <- as.numeric(summary_stats$regprice)
summary_stats$'Seller Posted Price' <- as.numeric(summary_stats$sellerpricenum)
summary_stats$'Ticket Transaction Price' <- as.numeric(summary_stats$pricenum)
summary_stats$'Game Attendance' <- as.numeric(summary_stats$att)
summary_stats$seats <- as.numeric(summary_stats$numb)
colnames(summary_stats)[colnames(summary_stats) == "seats"] <- 'Number of Seats'


#Converting tibble to data frame for ease of use with the stargazer package
summary_stats <- as.data.frame(summary_stats)

# Creating latex code for summary stats table
stargazer(summary_stats[,27:31])

rm(list = ls())


######################################################
######################################################
#     SUMMARY STATISTICS AND GRAPHS SECTION 3        #
######################################################
######################################################

# load data
stubhub_panel <- read_dta("stubhub_panel.dta")

#######################################################
# CODE BELOW IS FOR DATA SECTION (SECTION 3 IN PAPER)
#######################################################

######################################
# variable numbers
# newgameid (1)
# home team (2)
# away team (3)
# ticketprice (12)
# record of home team (38)
# record of away team (47)
# attendance (54)
# month of game (81)
######################################
data <- subset(stubhub_panel, , c(1:3,12,38,47,54,81))

######################################
# GRAPH 1: ticket price by month
######################################

# average ticket price for each month
tp_permonth <- data %>% group_by(monthgame)
tp_permonth <- tp_permonth %>% summarise(avg_price=mean(pricenum))
tp_permonth$monthgame <- as.factor(tp_permonth$monthgame)

# graph of ticket price dependence on month of game
ggplot(tp_permonth, aes(x=tp_permonth$monthgame)) +
  geom_line(mapping=aes(y=tp_permonth$avg_price, group=1), color="red", size=1.5) +
  scale_x_discrete(labels=c("April", "May", "June", "July", "August", "September")) + 
  scale_y_continuous(limits=c(80,100), breaks=c(80,84,88,92,96,100)) +
  xlab("Month of Baseball Game") + ylab("Ticket Price (in $)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 2: attendance by month
######################################

# subset to observations with attendance available
att_permonth <- subset(data, !is.na(att))

# average attendance for each month
att_permonth <- att_permonth %>% group_by(monthgame)
att_permonth <- att_permonth %>% summarise(avg_att=mean(att))

# convert average attendance to thousands of people
att_permonth$avg_att_e3 <- att_permonth$avg_att/1000
att_permonth$monthgame <- as.factor(att_permonth$monthgame)

# graph of attendance, month of game
ggplot(att_permonth, aes(x=att_permonth$monthgame)) +
  geom_line(mapping=aes(y=att_permonth$avg_att_e3, group=1), color="#0000FF", size=1.5) +
  scale_x_discrete(labels=c("April", "May", "June", "July", "August", "September")) + 
  scale_y_continuous(limits=c(38,43), breaks=c(38,39,40,41,42,43)) +
  xlab("Month of Baseball Game") + ylab("Attendance (in thousands)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 3: ticket price by record of home team
######################################

# average ticket price by home win percentage
tp_perwinperc <- data %>% group_by(homerecord)
tp_perwinperc <- tp_perwinperc %>% summarise(avg_price=mean(pricenum))

# graph of ticket price, win percentage of home team
ggplot(tp_perwinperc, aes(x=tp_perwinperc$homerecord)) +
  geom_point(mapping=aes(y=tp_perwinperc$avg_price), color="red") +
  scale_x_continuous(limits=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
  scale_y_continuous(limits=c(0,250), breaks=c(0,25,50,75,100,125,150,175,200,225,250)) +
  xlab("Win Percentage of Home Team") + ylab("Ticket Price (in $)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 4: attendance by record of home team
######################################

# average attendance by home win percentage
att_perwinperc <- subset(data, !is.na(att))
att_perwinperc <- att_perwinperc %>% group_by(homerecord)
att_perwinperc <- att_perwinperc %>% summarise(avg_att=mean(att))
att_perwinperc$avg_att_e3 <- att_perwinperc$avg_att/1000

# graph of attendance, win percentage of home team
ggplot(att_perwinperc, aes(x=att_perwinperc$homerecord)) +
  geom_point(mapping=aes(y=att_perwinperc$avg_att_e3), color="#0000FF") +
  scale_x_continuous(limits=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
  #scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  xlab("Win Percentage of Home Team") + ylab("Attendance (in thousands)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 5: ticket price by record of away team
######################################

# average ticket price for away win percentage
tp_perwinperc_away <- data %>% group_by(awayrecord)
tp_perwinperc_away <- tp_perwinperc_away %>% summarise(avg_price=mean(pricenum))

# graph of ticket price, win percentage of away team
ggplot(tp_perwinperc_away, aes(x=tp_perwinperc_away$awayrecord)) +
  geom_point(mapping=aes(y=tp_perwinperc_away$avg_price), color="red") +
  scale_x_continuous(limits=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
  scale_y_continuous(limits=c(0,250), breaks=c(0,25,50,75,100,125,150,175,200,225,250)) +
  xlab("Win Percentage of Home Team") + ylab("Ticket Price (in $)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 6: attendance by record of away team
######################################

# average attendance for away win percentage
att_perwinperc_away <- subset(data, !is.na(att))
att_perwinperc_away <- att_perwinperc_away %>% group_by(awayrecord)
att_perwinperc_away <- att_perwinperc_away %>% summarise(avg_att=mean(att))
att_perwinperc_away$avg_att_e3 <- att_perwinperc_away$avg_att/1000

# graph of attendance, win percentage of away team
ggplot(att_perwinperc_away, aes(x=att_perwinperc_away$awayrecord)) +
  geom_point(mapping=aes(y=att_perwinperc_away$avg_att_e3), color="#0000FF") +
  scale_x_continuous(limits=c(0,1), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
  #scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  xlab("Win Percentage of Home Team") + ylab("Attendance (in thousands)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

# clean environment
rm(tp_permonth, tp_perwinperc, tp_perwinperc_away)
rm(att_permonth, att_perwinperc, att_perwinperc_away)
rm(data)

#################################################
# GRAPH 7: ticket price by game day of the week
#################################################

# find average ticket price for each game day of the week
price_day <- stubhub_panel[,c("dowgame","pricenum")]
price_by_day <-price_day %>%
  group_by(dowgame) %>%
  summarize(AveragePrice = mean(pricenum))
price_by_day$dowgame <- as.factor(price_by_day$dowgame)

# graph of ticket price dependence on month of game
ggplot(price_by_day,aes(x=dowgame,y=AveragePrice)) +
  xlab("Game Day of the Week") + ylab("Ticket Price (in $)") +
  geom_line(color='red', size=1.5, aes(group=1)) +
  scale_x_discrete(labels=c("Sun","Mon","Tues","Wed","Thur","Fri","Sat"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

#################################################
# GRAPH 8: attendence by game day of the week
#################################################

# find average attendance for each game day of the week
att_day <- stubhub_panel[,c("dowgame","att")]
att_by_day <-att_day %>%
  group_by(dowgame) %>%
  summarize(AverageAttendance = mean(att,na.rm = TRUE))
att_by_day$AverageAttendance_e3 <- att_by_day$AverageAttendance/1000
att_by_day$dowgame <- as.factor(att_by_day$dowgame)

# graph of attendance against each game day of the week
ggplot(att_by_day,aes(x=dowgame,y=AverageAttendance_e3)) +
  xlab("Game Day of the Week") + ylab("Attendance (in thousands)") +
  geom_line(color='blue', size=1.5, aes(group=1)) +
  scale_x_discrete(labels=c("Sun","Mon","Tues","Wed","Thur","Fri","Sat"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

#################################################
# GRAPH 9: average ticket price by seat row order
#################################################

# average ticket price by seat row order
price_row <- stubhub_panel[,c("orderrow", "pricenum")]
by_row <-price_row %>%
  group_by(orderrow) %>%
  summarize(AveragePrice = mean(pricenum))

# graph of average ticket price against seat row order
ggplot(by_row,aes(x=orderrow,y=AveragePrice)) +
  xlab("Row Order of Seat") + ylab("Ticket Price (in $)") +
  geom_line(color='red', size=1.5) +
  scale_x_continuous(breaks=seq(0,30,5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

rm(list=ls())



#############################################################
#############################################################
# CODE FOR CROSS VALIDATION WITH RIDGE AND LASSO ON STUBHUB #
#############################################################
#############################################################

# load relevant packages
library(glmnet)

# load raw data
stubhub_panel <- read_dta("stubhub_panel.dta")

# subset data to 86 agreed upon variables
data <- subset(stubhub_panel, , c(1:6,12,15,21:26,30:32,34,36:43,45:52,54:57,60:61,65:67,72:74,80:82,98:99,101:102,107:127,150:161))
rm(stubhub_panel)


# remove newgameid, home, away, ticketidnum
data <- subset(data, , c(4:52, 54:86))

# deal with missing observations
data <- data[rowSums(is.na(data)) == 0, ]

# make sure all variables are of numeric type
data$auctiondum <- as.numeric(data$auctiondum)

# divide data into training and test sets (80% training, 20% test)
set.seed(0)
#Input training set split
train_split <- 0.8
test_ind = sample.int(nrow(data),round(train_split*nrow(data)))

#####################################################
# cross validation for ridge and lasso, ticket price
#####################################################

######## OLS for Ticket prices #################################################

# remove attendance for ticket price analysis
drop <- c("att")
data_ticketprice <- data[ , !(names(data) %in% drop)]
data$propmaxatt

# linear model with all covariates only
ticketprice_mod1 <- lm(pricenum ~ . - sellerpricenum - buyerpricenum - RELSPRICE - propmaxatt, data_ticketprice[test_ind,], x=TRUE)
ticketprice_mod1_test <- lm(pricenum ~ . - sellerpricenum - buyerpricenum - RELSPRICE - propmaxatt, data_ticketprice[-test_ind,], x=TRUE)
X <- ticketprice_mod1$x[,-1]
y <- ticketprice_mod1$model$pricenum
X_test <- ticketprice_mod1_test$x[,-1]
y_test <- ticketprice_mod1_test$model$pricenum

#Predicting test sample on OLS
p_OLS <- predict(ticketprice_mod1, data_ticketprice[-test_ind,])
SD_MSE_OLS_tp <- sqrt(mean((y_test - p_OLS)^2))

rm(ticketprice_mod1, ticketprice_mod1_test, p_OLS)

######## Cross Validation ridge/lasso for ticket prices ######################

# ridge 10-fold cross validation
ridge_tp <- cv.glmnet(X, y, alpha = 0, nfold = 10)
ridge_tp_lambda <- ridge_tp$lambda.min

# minimum mean squared error for ridge in training sample
index <- which(ridge_tp$lambda == ridge_tp_lambda)
ridge_tp$cvm[index]

# defining ridge regression that takes in lambda min from cv above
ridge_tp_min <- glmnet(X, y, alpha = 0, lambda = ridge_tp_lambda)

#running a prediction for price using the ridge above
p_cv_min_ridge_test <- predict(ridge_tp_min, newx = X_test, s = "lambda.min")
#Defining the square root of the mean squared error to make it comparable to price
SD_MSE_ridge_tp <- sqrt(mean((y_test - p_cv_min_ridge_test)^2))

# lasso 10-fold cross validation
lasso_tp <- cv.glmnet(X, y, alpha = 1, nfold = 10)
lasso_tp_lambda <- lasso_tp$lambda.min

# minimum mean squared error for lasso in training sample
index <- which(lasso_tp$lambda == lasso_tp_lambda)
lasso_tp$cvm[index]

# defining ridge regression that takes in lambda min from cv above
lasso_tp_min <- glmnet(X, y, alpha = 1, lambda = lasso_tp_lambda)

#running a prediction for price using the ridge above
p_cv_min_lasso_test <- predict(lasso_tp_min, newx = X_test, s = "lambda.min")
#Defining the square root of the mean squared error to make it comparable to price
SD_MSE_lasso_tp <- sqrt(mean((y_test - p_cv_min_lasso_test)^2))

rm(X, X_test)

#####################################################
# cross validation for ridge and lasso, attendance
#####################################################

######## OLS for attendance #################################################

# remove ticket price for attendance analysis
drop2 <- c("pricenum")
data_att <- data[ , !(names(data) %in% drop2)]

# linear model with all covariates only
att_mod1 <- lm(att ~ . - sellerpricenum - buyerpricenum - RELSPRICE - propmaxatt, data_att[test_ind,], x=TRUE)
att_mod1_test <- lm(att ~ . - sellerpricenum - buyerpricenum - RELSPRICE - propmaxatt, data_att[-test_ind,], x=TRUE)
X <- att_mod1$x[,-1]
y <- att_mod1$model$att
X_test <- att_mod1_test$x[,-1]
y_test <- att_mod1_test$model$att

#Predicting test sample on OLS
p_OLS_att <- predict(att_mod1, data_att[-test_ind,])
SD_MSE_OLS_att <- sqrt(mean((y_test - p_OLS_att)^2))


######## Cross Validation ridge/lasso for attendance ######################

# ridge 10-fold cross validation
ridge_att <- cv.glmnet(X, y, alpha = 0, nfold = 10)
ridge_att_lambda <- ridge_att$lambda.min

# minimum mean squared error for ridge
index <- which(ridge_att$lambda == ridge_att_lambda)
ridge_att$cvm[index]

# defining ridge regression that takes in lambda min from cv above
ridge_att_min <- glmnet(X, y, alpha = 0, lambda = ridge_att_lambda)

#running a prediction for price using the ridge above
p_cv_min_ridge_test_att <- predict(ridge_att_min, newx = X_test, s = "lambda.min")
#Defining the square root of the mean squared error to make it comparable to price
SD_MSE_ridge_att <- sqrt(mean((y_test - p_cv_min_ridge_test_att)^2))

# lasso 10-fold cross validation
lasso_att <- cv.glmnet(X, y, alpha = 1, nfold = 10)
lasso_att_lambda <- lasso_att$lambda.min

# minimum mean squared error for lasso
index <- which(lasso_att$lambda == lasso_att_lambda)
lasso_att$cvm[index]

# defining ridge regression that takes in lambda min from cv above
lasso_att_min <- glmnet(X, y, alpha = 0, lambda = lasso_att_lambda)

#running a prediction for price using the ridge above
p_cv_min_lasso_test_att <- predict(lasso_att_min, newx = X_test, s = "lambda.min")
#Defining the square root of the mean squared error to make it comparable to price
SD_MSE_lasso_att <- sqrt(mean((y_test - p_cv_min_lasso_test_att)^2))

rm(list = ls())


##############################################
##############################################
#   CODE FOR PRINCIPAL COMPONENT ANALYSIS    #
##############################################
##############################################

library(Lahman)
library(modelr)
library(shiny)
library(boot)
library(pls)

memory.limit(100000)

#Importing data
stubhub_panel <- read_dta("stubhub_panel.dta")
#Eleminnating irrelevant data 
data <- subset(stubhub_panel, , c(1:6,12,15,21:26,30:32,34,36:43,45:52,54:57,60:61,65:68,72:74,80:82,98:99,101:102,107:127,150:161))
rm(stubhub_panel)

#Dealing with NA values
data[(is.na(data))]<-0
#Checking for NA
colSums(is.na(data))

set.seed(0) 
#sampling the data
data2<-sample_frac(data,.1)

#Splitting data into test and train samples
data.train<-sample_frac(data2,.8)
train.index<-as.numeric(rownames(data.train))
data.test<-data2[-train.index, ]

#constructing a covariate matrix by removing dependents, non-numerics and constant variables from data
COVRTS.train <- subset(data.train, select = -c(newgameid,home,away,att,propmaxatt,pricenum,sellerpricenum,buyerpricenum,RELSPRICE,ticketidnum,piggy,multirow,homediv,homeleague,ssectionnumb))
COVRTS.test <- subset(data.test, select = -c(newgameid,home,away,att,propmaxatt,pricenum,sellerpricenum,buyerpricenum,RELSPRICE,ticketidnum,piggy,multirow,homediv,homeleague,ssectionnumb))
DEPENDENTS.train<-subset(data.train,select=c(att,pricenum))
DEPENDENTS.test<-subset(data.test,select=c(att,pricenum))

#Turning auctiondum into numeric 
COVRTS.train$auctiondum<-as.numeric(COVRTS.train$auctiondum)
COVRTS.test$auctiondum<-as.numeric(COVRTS.test$auctiondum)

#PCA with scale for graphs
data.train0.pca <- prcomp(COVRTS.train, center = TRUE,scale = TRUE)
#PCA with no scale for graphs
data.train1.pca <- prcomp(COVRTS.train, center = TRUE,scale = FALSE)

#Fitting PCR for PRICE
pcr_fit_PRICE = pcr(DEPENDENTS.train$pricenum~., data = COVRTS.train, scale = T, validation = "CV")
validationplot(pcr_fit_PRICE, val.type = "RMSEP")
summary(pcr_fit_PRICE)

#Model Selection with one-sigma method for Price
ncomp.onesigma.price<-selectNcomp(pcr_fit_PRICE, method = "onesigma", plot = TRUE, xlim = c(60, 73))
ncomp.onesigma.price<-selectNcomp(pcr_fit_PRICE, method = "onesigma", plot = TRUE)

#Prediction with the result of one-sigma method & Test RMSE for Price
pcr_pred_PRICE<-as.data.frame(predict(pcr_fit_PRICE, COVRTS.test, ncomp=ncomp.onesigma.price ))
TEST_RMSE_PRICE<-sqrt(mean((pcr_pred_PRICE$`DEPENDENTS.train$pricenum.65 comps`-DEPENDENTS.test$pricenum)^2))

#Fitting PCR for ATTENDANCE
pcr_fit_ATT = pcr(DEPENDENTS.train$att~., data = COVRTS.train, scale = T, validation = "CV")
validationplot(pcr_fit_ATT, val.type = "RMSEP")
summary(pcr_fit_ATT)

#Model Selection with one-sigma method for Attendance
ncomp.onesigma.att <- selectNcomp(pcr_fit_ATT, method = "onesigma", plot = TRUE, xlim = c(60, 73))
ncomp.onesigma.att <- selectNcomp(pcr_fit_ATT, method = "onesigma", plot = TRUE)

#Prediction with the result of one-sigma method & Test RMSE for Attendance
pcr_pred_ATT<-as.data.frame(predict(pcr_fit_ATT, COVRTS.test, ncomp=ncomp.onesigma.att))
TEST_RMSE_ATT<-sqrt(mean((pcr_pred_ATT$`DEPENDENTS.train$att.69 comps`-DEPENDENTS.test$att)^2))


##############################################
##############################################
#           CODE FOR PCA GRAPHS              #
##############################################
##############################################


##############################################
#           GRAPHS FOR FIGURE 5
##############################################

#Plot with scale
pr_var<-as.data.frame((data.train0.pca$sdev)^2)
colnames(pr_var) <- c('X')
prop_varex <- as.data.frame(pr_var$X/sum(pr_var$X))
colnames(prop_varex) <- c('Variance_Proportion')
PCVARPROP<-prop_varex[order(-prop_varex$Variance_Proportion), , drop = FALSE]
PCVARPROPCUM<-as.data.frame(cumsum(prop_varex$Variance_Proportion))
colnames(PCVARPROPCUM) <- c('Variance_Proportion')
PCVARPROP20<-as.data.frame(head(sort(PCVARPROP$Variance_Proportion,decreasing=TRUE), n = 10))
colnames(PCVARPROP20) <- c('Variance_Proportion')

ggplot(data=PCVARPROP20, aes(x=as.numeric(row.names(PCVARPROP20)), y= Variance_Proportion))+
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = "Principal Components",y='Variance Proportion')+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  scale_y_continuous(labels = scales::percent, limits = c(0, .1))+
  scale_x_continuous(breaks = seq(1,10,by=1))

#Plot with no Scale
pr_var<-as.data.frame((data.train1.pca$sdev)^2)
colnames(pr_var) <- c('X')
prop_varex <- as.data.frame(pr_var$X/sum(pr_var$X))
colnames(prop_varex) <- c('Variance_Proportion')
PCVARPROP<-prop_varex[order(-prop_varex$Variance_Proportion), , drop = FALSE]
PCVARPROP20<-as.data.frame(head(sort(PCVARPROP$Variance_Proportion,decreasing=TRUE), n = 10))
colnames(PCVARPROP20) <- c('Variance_Proportion')

ggplot(data=PCVARPROP20, aes(x=as.numeric(row.names(PCVARPROP20)), y= Variance_Proportion))+
  geom_bar(stat="identity", fill="steelblue")+
  labs(x = "Principal Components",y='Variance Proportion')+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  scale_x_continuous(breaks = seq(1,10,by=1))

rm(list = ls())

##############################################
##############################################
#           CODE FOR RANDOM FOREST           #
##############################################
##############################################

library(randomForest)

# load raw data 
stubhub <- read_dta('stubhub_panel.dta')
# Subset data to 87 agreed upon variables
data <- subset(stubhub, , c(1:6,12,15,21:26,30:32,34,36:43,45:52,54:57,60:61,65:67,72:74,80:82,98:99,101:102,107:127,150:161))

# Remove newgameid(1), home(2), away(3) and ticketidnum(53) which don't have prediction power
data1 <- subset(data, , c(4:52, 54:86))

# Deal with missing observations
data <- data1[rowSums(is.na(data1)) == 0, ]

# Make sure all variables are of numeric type
data$auctiondum <- as.numeric(data$auctiondum)

# Converte variable names to all be legal to avoid Error in randomForest
names(data) <- make.names(names(data))

# Remove 'propmaxatt','sellerpricenum', 'ticketidnum', 'buyerpricenum' and 'RELSPRICE' which cannot be used in prediction
data<- data[, -which(names(data) %in% c('propmaxatt','sellerpricenum', 'buyerpricenum', 'RELSPRICE'))]

# Save data.frame as csv file "data"
write.csv(data, file='data.csv')

# divide data into training and test sets (20%)
set.seed(0)
test_ind = sample.int(nrow(data),round(0.2*nrow(data)))

#####################################################
# Random Forest for ticket price
#####################################################

# remove attendance for ticket price analysis
data_ticketprice <- data[, -which(names(data)=="att")]

# Find optimal number of trees from [10, 50, 100, 200] for price 
# random forest method with 10 trees
set.seed(0)
rf_price_10 = randomForest(pricenum ~ ., data=data_ticketprice[-test_ind, ],
                           mtry=25, ntree=10, importance=TRUE)

yhat_rf_price_10 = predict(rf_price_10, newdata=data_ticketprice[test_ind,])
data_ticketprice_test = data_ticketprice[test_ind, "pricenum"]
mean((yhat_rf_price_10 - data_ticketprice_test)^2)

# random forest method with 50 trees
set.seed(0)
rf_price_50 = randomForest(pricenum ~ ., data=data_ticketprice[-test_ind, ],
                           mtry=25, ntree=50, importance=TRUE)

yhat_rf_price_50 = predict(rf_price_50, newdata=data_ticketprice[test_ind,])
data_ticketprice_test = data_ticketprice[test_ind, "pricenum"]
mean((yhat_rf_price_50 - data_ticketprice_test)^2)

# random forest method with 100 trees
set.seed(0)
rf_price_100 = randomForest(pricenum ~ ., data=data_ticketprice[-test_ind, ],
                            mtry=25, ntree=100, importance=TRUE)

yhat_rf_price_100 = predict(rf_price_100, newdata=data_ticketprice[test_ind,])
data_ticketprice_test = data_ticketprice[test_ind, "pricenum"]
mean((yhat_rf_price_100 - data_ticketprice_test)^2)

# random forest method with 200 trees
set.seed(0)
rf_price_200 = randomForest(pricenum ~ ., data=data_ticketprice[-test_ind, ],
                            mtry=25, ntree=200, importance=TRUE)

yhat_rf_price_200 = predict(rf_price_200, newdata=data_ticketprice[test_ind,])
data_ticketprice_test = data_ticketprice[test_ind, "pricenum"]
mean((yhat_rf_price_200 - data_ticketprice_test)^2)

#####################################################
# Random Forest for attendance
#####################################################

# remove ticket price for attendance analysis
data_att <- data[, -which(names(data)=="pricenum")]

# find optimal number of trees from [10, 50, 100, 200] for attendance
# random forest method with 10 trees
set.seed(0)
rf_att_10 = randomForest(att ~ ., data=data_att[-test_ind, ],
                         mtry=25, ntree=10, importance=TRUE)

yhat_rf_att_10 = predict(rf_att_10, newdata=data_att[test_ind,])
data_att_test = data_att[test_ind, "att"]
mean((yhat_rf_att_10 - data_att_test)^2)

# random forest method with 50 trees
set.seed(0)
rf_att_50 = randomForest(att ~ ., data=data_att[-test_ind, ],
                         mtry=25, ntree=50, importance=TRUE)

yhat_rf_att_50 = predict(rf_att_50, newdata=data_att[test_ind,])
data_att_test = data_att[test_ind, "pricenum"]
mean((yhat_rf_att_50 - data_att_test)^2)

# random forest method with 100 trees
set.seed(0)
rf_att_100 = randomForest(att ~ ., data=data_att[-test_ind, ],
                          mtry=25, ntree=100, importance=TRUE)

yhat_rf_att_100 = predict(rf_att_100, newdata=data_att[test_ind,])
data_att_test = data_att[test_ind, "pricenum"]
mean((yhat_rf_att_100 - data_att_test)^2)

# random forest method with 200 trees
set.seed(0)
rf_att_200 = randomForest(att ~ ., data=data_att[-test_ind, ],
                          mtry=25, ntree=200, importance=TRUE)

yhat_rf_att_200 = predict(rf_att_200, newdata=data_att[test_ind,])
data_att_test = data_att[test_ind, "pricenum"]
mean((yhat_rf_att_200 - data_att_test)^2)

rm(list=ls())


##############################################
##############################################
#         GRAPHS FOR SELLER STRATEGY         #
##############################################
##############################################

# load data
stubhub_panel <- read_dta("stubhub_panel.dta")

#######################################################
# CODE BELOW IS FOR MARGINAL PROFIT (SECTION 6 IN PAPER)
#######################################################

######################################
# variable numbers
# newgameid (1)
# home team (2)
# away team (3)
# section number (9)
# row number (10)
# face value (25)
# record of home team (38)
# record of away team (47)
# seller price (61)
# month of game (81)
# day of week (82)
######################################
data2 <- subset(stubhub_panel, , c(1:3,9:10,25,38,47,61,81,82))

# subset to Los Angeles Dodgers
data2 <- subset(data2, home == "LAD")

# average marginal price for monthgame, dowgame, section, row, away

# create marginal profit = (seller price - face value)/face value
data2$marg_profit <- (data2$sellerpricenum-data2$regprice)/data2$regprice
data2 <- subset(data2, !is.na(marg_profit))

######################################
# GRAPH 1: marginal price by month
######################################

# find average marginal price for each month
mp_permonth <- data2 %>% group_by(monthgame)
mp_permonth <- mp_permonth %>% summarise(avg_marg_profit=mean(marg_profit))
mp_permonth$monthgame <- as.factor(mp_permonth$monthgame)

# graph of marginal price, month of game
ggplot(mp_permonth, aes(x=mp_permonth$monthgame)) +
  geom_line(mapping=aes(y=mp_permonth$avg_marg_profit, group=1), color="red", size=1.5) +
  scale_x_discrete(labels=c("April", "May", "June", "July", "August", "September")) + 
  xlab("Month of Baseball Game") + ylab("Marginal Profit") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 2: marginal price by day of week
######################################

# find average marginal price for day of week
mp_perday <- data2 %>% group_by(dowgame)
mp_perday <- mp_perday %>% summarise(avg_marg_profit=mean(marg_profit))
mp_perday$dowgame <- as.factor(mp_perday$dowgame)

# graph of marginal price, day of week of game
ggplot(mp_perday, aes(x=mp_perday$dowgame)) +
  geom_line(mapping=aes(y=mp_perday$avg_marg_profit, group=1), color="red", size=1.5) +
  scale_x_discrete(labels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")) + 
  xlab("Day of Week of Baseball Game") + ylab("Marginal Profit") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 3: marginal price by home record
######################################

# find average marginal price for home record
mp_homerec <- data2 %>% group_by(homerecord)
mp_homerec <- mp_homerec %>% summarise(avg_marg_profit=mean(marg_profit))

# graph of marginal price, home record
ggplot(mp_homerec, aes(x=mp_homerec$homerecord)) +
  geom_point(mapping=aes(y=mp_homerec$avg_marg_profit), color="red") +
  scale_x_continuous(limits=c(0.5,0.8), breaks=c(0.5,0.6,0.7,0.8)) + 
  xlab("Win Percentage of LA Dodgers") + ylab("Marginal Profit") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10),
        legend.position="none",
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 4: marginal price by away team
######################################

# find average marginal price for away team
mp_away <- data2 %>% group_by(away)
mp_away <- mp_away %>% summarise(avg_marg_profit=mean(marg_profit))
mp_away$away <- as.factor(mp_away$away)

# graph of marginal price, away team
ggplot(mp_away, aes(x=mp_away$away)) +
  geom_point(mapping=aes(y=mp_away$avg_marg_profit), color="red") +
  #scale_x_discrete(labels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")) + 
  xlab("Away Team") + ylab("Marginal Profit") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10, angle=80, hjust=1),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 5: marginal price by seat section number
######################################

# find average marginal price for section
mp_section <- data2 %>% group_by(section)
mp_section <- mp_section %>% summarise(avg_marg_profit=mean(marg_profit))
mp_section$section <- as.factor(mp_section$section)
mp_section_best <- subset(mp_section, avg_marg_profit>4)

# graph of marginal price, section
ggplot(mp_section_best, aes(x=mp_section_best$section)) +
  geom_point(mapping=aes(y=mp_section_best$avg_marg_profit), color="red") +
  xlab("Section Listed") + ylab("Marginal Profit") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10, angle=80, hjust=1),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

######################################
# GRAPH 6: marginal price by seat row number
######################################

# find average marginal price for row
mp_row <- data2 %>% group_by(row)
mp_row <- mp_row %>% summarise(avg_marg_profit=mean(marg_profit))
mp_row$row <- as.factor(mp_row$row)
mp_row_best <- subset(mp_row, avg_marg_profit>4)

# graph of marginal price, row
ggplot(mp_row_best, aes(x=mp_row_best$row)) +
  geom_point(mapping=aes(y=mp_row_best$avg_marg_profit), color="red") +
  xlab("Row Listed") + ylab("Marginal Profit") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=10, angle=80, hjust=1),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8,0.2),
        panel.background=element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="white"), 
        axis.line.x = element_line(color="black", size=0.6),
        axis.line.y = element_line(color="black", size=0.6))

rm(list=ls())
