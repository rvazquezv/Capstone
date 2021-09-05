library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(purrr)
library(RColorBrewer)
library(dplyr)
library(tidyr)


##Following code has been tested on a train set much more slow than this model one. In fact by taking random sample on 2400 movies and 15000 users building a subset of edx 
## of 386.482 rows instead of 9.000.055 rows. Despite that fact Results obtained not improved too much


## NOT RUNNING THIS CODE ON EDX BUT ON SMALL_EDX OTHERWISE LAPTOP WILL CRACK

predicted_ratings <- train_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  mutate(bin=case_when(difftime(date,first_rate_i,units="days")<=n ~ 1,
                       difftime(date,first_rate_i,units="days")>n & difftime(date,first_rate_i,units="days")<=2*n ~ 2,
                       difftime(date,first_rate_i,units="days")>2*n & difftime(date,first_rate_i,units="days")<=3*n ~ 3,
                       difftime(date,first_rate_i,units="days")>3*n & difftime(date,first_rate_i,units="days")<=4*n ~ 4,
                       difftime(date,first_rate_i,units="days")>4*n & difftime(date,first_rate_i,units="days")<=5*n ~ 5,
                       difftime(date,first_rate_i,units="days")>5*n & difftime(date,first_rate_i,units="days")<=6*n ~ 6,
                       difftime(date,first_rate_i,units="days")>6*n & difftime(date,first_rate_i,units="days")<=7*n ~ 7,
                       difftime(date,first_rate_i,units="days")>7*n & difftime(date,first_rate_i,units="days")<=8*n ~ 8,
                       difftime(date,first_rate_i,units="days")>8*n & difftime(date,first_rate_i,units="days")<=9*n ~ 9,
                       difftime(date,first_rate_i,units="days")>9*n~10))  %>%
  left_join(user_avgs_reg, by='userId')%>%
  left_join(movie_avgs_reg_bin, by=c('movieId','bin'))%>%
  mutate(dev_u_t=as.numeric(difftime(date,t_u,units="days"))) %>%
  mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b)) %>%
  mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)+dev_u_t,resid=rating-pred) %>% 
  select(userId, movieId, resid)

# Once resids of my predictions have been calculated just transform them into a resid matrix
y <-as_tibble(predicted_ratings)
z<- y %>% pivot_wider(names_from = "movieId", values_from = "resid") %>%
  as.matrix()

# Add rownames and columnanes to the matrix
rownames(z)<- z[,1]
z <- z[,-1]

movie_titles <- small_edx %>% 
  select(movieId, title) %>%
  distinct()

colnames(z) <- with(movie_titles, title[match(colnames(z), movieId)])

#Erase NA values and plot explanaition on variability of the sd
z[is.na(z)] <- 0
plot(summary(pc)$importance[3,])

#Applying Sigle Value Decomposition
s<-svd(z)
z_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(z - z_svd))

#Accroding to previous plot considering 50 eigenvalues that will explain 34.44393% of total variability
sum(s$d[1:50]^2) / sum(s$d^2)

#Calculating my predictor
z_hat <- with(s,sweep(u[, 1:50], 2, d[1:50], FUN="*") %*% t(v[, 1:50]))


#Transforming it from matrix to data.frame again
rownames(z_hat)<- rownames(z)
colnames(z_hat) <- colnames(z)
userId<-as.numeric(rownames(z_hat))
z_hat <- as_tibble(z_hat,rownames=NA) %>%   cbind(userId,.)  %>% 
         gather(data=.,-userId,key='title',value=resid) 



###########################################################################################################☺
##Predicting
###########################################################################################################☺


predicted_ratings <- test_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  mutate(bin=case_when(difftime(date,first_rate_i,units="days")<=n ~ 1,
                       difftime(date,first_rate_i,units="days")>n & difftime(date,first_rate_i,units="days")<=2*n ~ 2,
                       difftime(date,first_rate_i,units="days")>2*n & difftime(date,first_rate_i,units="days")<=3*n ~ 3,
                       difftime(date,first_rate_i,units="days")>3*n & difftime(date,first_rate_i,units="days")<=4*n ~ 4,
                       difftime(date,first_rate_i,units="days")>4*n & difftime(date,first_rate_i,units="days")<=5*n ~ 5,
                       difftime(date,first_rate_i,units="days")>5*n & difftime(date,first_rate_i,units="days")<=6*n ~ 6,
                       difftime(date,first_rate_i,units="days")>6*n & difftime(date,first_rate_i,units="days")<=7*n ~ 7,
                       difftime(date,first_rate_i,units="days")>7*n & difftime(date,first_rate_i,units="days")<=8*n ~ 8,
                       difftime(date,first_rate_i,units="days")>8*n & difftime(date,first_rate_i,units="days")<=9*n ~ 9,
                       difftime(date,first_rate_i,units="days")>9*n~10))  %>%
  left_join(user_avgs_reg, by='userId')%>%
  left_join(movie_avgs_reg_bin, by=c('movieId','bin'))%>%
  left_join(z_hat, by=c('title','userId'))%>%
  mutate(dev_u_t=as.numeric(difftime(date,t_u,units="days"))) %>%
  mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b)) %>%
  mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)+dev_u_t+sum_b_g+ifelse(is.na(resid),0,resid)) %>%
  pull(pred)

resid_rmse<-RMSE(predicted_ratings, test_set$rating)
resid_mae<-MAE(predicted_ratings, test_set$rating)



