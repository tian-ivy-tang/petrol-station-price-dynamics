library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(pheatmap)
library(glmnet)
library(caTools)
library(devtools)

##############################################################################
#part 1 pre-process data
#filter stations & price data, save as csv files
#read Rosenheim stations selected from QGIS
stations <- read.csv('Stations_Rosenheim.csv')
stations <- as.data.table(stations)
stations[is.na(stations)] <- 0
stations$driving.time <- scale(stations$driving.time)
str(stations)

#get driving time to closest 3 stations
distances <- read_csv('Distance_between_stations.csv')
distances <- as.data.table(distances)
driving_time_summary <- distances %>% 
  group_by(FROM_INDEX) %>% 
  slice_min(DURATION_H, n=4) %>% 
  slice_max(DURATION_H, n=3) #remove 0 driving time to itself
str(driving_time_summary)

#plot figure 3 driving time matrix
driving_matrix <- distances[, 1:3]
driving_matrix$DURATION_H <- round(driving_matrix$DURATION_H, digits=2)
driving_matrix_casted <- dcast(driving_matrix,
                               ...~TO_INDEX,
                               value.var = 'DURATION_H')
colnames(driving_matrix_casted)[1] <- 'index'
pheatmap(driving_matrix_casted[, 2:21],
         cluster_rows = F,
         cluster_cols = F)

#read all price csv & combine into 1 table (Apr 13 - May 14, 2020)
price_file_list <- list.files('data/price', full.names=TRUE)
price_tables <- lapply(price_file_list, fread)
pt <- rbindlist(price_tables, idcol='filepath')

#filter price table to Rosenheim stations & e5 gas type
prices <- pt[station_uuid %in% stations[, uuid] &
               e5change==1,
             .(date, station_uuid, e5)]

#tidy prices' date
colnames(prices)[2] <-'uuid'
prices[, p_date:= as.Date(date, format='%m/%d')]
prices[, p_year :=format(prices$p_date, '%y')]
prices$p_date <- format(prices$p_date, '%m-%d')
prices[, p_hour:= format(prices$date, 
                         format='%H:%M')]
prices[, p_weekday:= weekdays(date, abbr=T)]
is_weekend <- ifelse(prices$p_weekday=='Sat'| prices$p_weekday=='Sun',
                     1, 0)
prices[, is_weekend:= is_weekend]

#merge stations & prices table (Apr 13 - May 14, 2020)
colnames(stations)
prices_merged <- merge(prices, 
                       stations[, -c(2,4,5,6,7,8,9,10,11)], 
                       by='uuid', all=T)

#extract last prices before Apr 15 as initial prices, for both 2020+2021
initial_prices_20 <- prices_merged[p_year==20 &
                                   p_date %in% c('04-10','04-11','04-12','04-13','04-14'),
                                   .(date, brand, index, p_date, p_hour, e5)] %>% 
  group_by(index) %>% 
  filter(date==max(date))
write_csv(initial_prices_20, 'initial_prices_2020.csv')

#save prices during Apr 15 - May 14 for 2020
prices_tba <- prices_merged[p_year==20 & 
                              !(p_date %in% c('04-10','04-11','04-12','04-13','04-14'))]
write_csv(prices_tba, 'Prices_Rosenheim.csv')

###############################################################################
#part 2 get descriptives on station rankings
#read pre-processed merged prices data
prices <- as.data.table(read.csv('Prices_Rosenheim.csv'))
current <- as.data.table(read.csv('initial_prices_2020.csv'))

#station 1 & 11 are not on the initial price list, earliest price on Apr 20
#so manually add 2 rows
station1 <- data.table('date'=NA, 'brand'='TOTAL', 'index'=1,
                       'p_date'=NA, 'p_hour'=NA, 'e5'=10)
station11 <- data.table('date'=NA, 'brand'='ARAL', 'index'=11,
                        'p_date'=NA, 'p_hour'=NA, 'e5'=10)
current <- rbindlist(list(current, station1, station11))

#add rank column to initial (current) price table to display inital (current) ranking
current <- current %>% arrange(index)
current[, rank:= 0]
current <- current %>% arrange(e5, date, brand)
current$rank <- 1:20
current <- current %>% arrange(index)

#add columns to prices table as stations' ranking before & after price change
prices <- prices %>% arrange(date)
prices[, rank1:= NA]
prices[, rank2:= NA]

rank1 <- rep(NA, nrow(prices))
rank2 <- rep(NA, nrow(prices))


#write loop to get station rank before and after price change, append to price table
for (i in 1:nrow(prices)){
  index <- prices$index[i]  #get station index
  rank1[i] <- current$rank[index]  #get station rank before price change
  
  #update price & time
  current$e5[index] <- prices$e5[i]
  current$date[index] <- prices$date[i]
  
  #sort updated current price list & get new rank
  current <- current %>% arrange(e5, date, brand)
  current$rank <- 1:20
  current <- current %>% arrange(index)
  
  #get station rank after price change
  rank2[i] <- current$rank[index]
}
prices$rank1 <- rank1
prices$rank2 <- rank2
prices[, rank_change := rank2-rank1]

#determine each price change is an increase or reduction
price_changes <- prices %>% arrange(date) %>% 
  group_by(index) %>% 
  mutate(e5_0 = lag(e5))

#transform %h:%m into decimal hours
price_changes <- price_changes %>% 
  mutate(p_min = as.numeric(substr(p_hour,4,5))) %>% 
  mutate(d_hour = as.numeric(substr(p_hour,1,2))) %>% 
  mutate(d_hour = round(d_hour + p_min/60, digits=2))

#filter price reduction events and opening hour between 7-21
price_reductions <- price_changes %>% 
  filter(e5 < as.numeric(e5_0)) %>% 
  filter(d_hour >= 7, d_hour <=21)
  
#plot figure 5 histogram of overall rank distribution before & after price reduction
ggplot(price_reductions, aes(rank1))+
  geom_bar(aes(y = (..count..)/sum(..count..)))
ggplot(price_reductions, aes(rank2))+
  geom_bar(aes(y = (..count..)/sum(..count..)))

#plot figure 6 histogram of rank distribution per brand
ggplot(price_reductions, aes(rank1))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~brand)
ggplot(price_reductions, aes(rank2))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  facet_wrap(~brand)

#plot figure 7 cumulative histogram of rank change
ggplot(price_reductions, aes(rank_change))+
  geom_histogram(aes(y = cumsum(..count..)/sum(..count..)))

##############################################################################
#part 3 prepare entire price change (+/-) table & perform lasso
#create 0/1 cut to represent whether a price reduction is performed, y of model
#create 0/1 out_top10 represent whether station's ranking before price change is lower than 10
price_changes <- price_changes %>% 
  mutate(cut = ifelse(rank2<=rank1, 1, 0)) %>% 
  mutate(out_top10 = ifelse(rank1 >10, 1, 0)) %>% 
  filter(d_hour >= 7, d_hour <=21) #filter opening hours

#select data for lasso
price_lasso <- price_changes[, c(35, 36, 7:30)]

#split data into train & test
sample <- sample.split(price_lasso$cut,
                      SplitRatio=0.8)
train <- subset(price_lasso, sample==T)
test <- subset(price_lasso, sample==F)
x <- model.matrix(cut~., train)[,-1]
y <- train$cut

#find best lambda with cross-validation
cv.lasso <- cv.glmnet(x, y, 
                      family='binomial', 
                      alpha=1,
                      lamda=NULL,
                      standardize=T)
#plot figure 8 deviance vs. log(lambda)
plot(cv.lasso)
cv.lasso$lambda.min
cv.lasso$lambda.1se
coef_matrix <- as.matrix(coef(cv.lasso, cv.lasso$lambda.1se))

#unable to export using R, must save in csv file first then read
coef_lasso <- as.data.frame(coef_matrix) 
write.csv(coef_lasso, 'lasso_coef.csv')

#too many variables left with min lambda
#fit 1se lambda into model
model <- glmnet(x, y,
                alpha=1,
                family='binomial',
                lamda=cv.lasso$lambda.1se,
                standardize=T)

#predict model on test data
x.test <- model.matrix(cut~., test)[,-1]
prob <- model %>% predict(newx = x.test)
predicted_y <- ifelse(prob >=0.5, 1, 0)

#get model accuracy
observed_y <- test$cut
mean(predicted_y==observed_y)

#get list of coefficients picked by Lasso
coefs <- as.data.table(read.csv('lasso_coef.csv'))
str(coefs)
coefs_short <- coefs %>% filter(value>0) %>% select(variable)

#############################################################
#part 4 use variables selected by Lasso to perform logistic regression
#filter & format price changes table for logistic regression
price_changes_glm <- price_changes[, c(35,36,7,9, 10, 16, 20, 22, 23, 24)]
str(price_changes_glm)
as.factor(price_changes_glm$p_weekday)
as.factor(price_changes_glm$brand)
as.factor(price_changes_glm$index)

logit_model <- glm(cut~., 
                   family='binomial',
                   data=price_changes_glm)
summary(logit_model)
tidy_logit <- tidy(logit_model)
