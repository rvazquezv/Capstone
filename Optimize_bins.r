
library(tidyverse)
library(caret)
library(data.table)


library(lubridate)
library(purrr)
library(RColorBrewer)



bins<-function(n){
movie_avgs_reg<- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i_reg = sum(rating - mu)/(n()+l1),first_rate_i=min(date))
  
date_user_avgs_reg <- train_set %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - mu - b_i_reg)/(n()+l2),t_u = mean(date))

movie_avgs_reg_bin<-train_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId')  %>%
  mutate(bin=case_when(difftime(date,first_rate_i,units="days")<=n ~ 1,
                       difftime(date,first_rate_i,units="days")>n & difftime(date,first_rate_i,units="days")<=2*n ~ 2,
                       difftime(date,first_rate_i,units="days")>2*n & difftime(date,first_rate_i,units="days")<=3*n ~ 3,
                       difftime(date,first_rate_i,units="days")>3*n & difftime(date,first_rate_i,units="days")<=4*n ~ 4,
                       difftime(date,first_rate_i,units="days")>4*n & difftime(date,first_rate_i,units="days")<=5*n ~ 5,
                       difftime(date,first_rate_i,units="days")>5*n & difftime(date,first_rate_i,units="days")<=6*n ~ 6,
                       difftime(date,first_rate_i,units="days")>6*n & difftime(date,first_rate_i,units="days")<=7*n ~ 7,
                       difftime(date,first_rate_i,units="days")>7*n & difftime(date,first_rate_i,units="days")<=8*n ~ 8,
                       difftime(date,first_rate_i,units="days")>8*n & difftime(date,first_rate_i,units="days")<=9*n ~ 9,
                       difftime(date,first_rate_i,units="days")>9*n~10)
  )%>%
  group_by(movieId,bin) %>% 
  summarize(b_i = mean(rating - mu - b_i_reg - b_u_reg))

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
                       difftime(date,first_rate_i,units="days")>9*n~10)
  )  %>%
  left_join(date_user_avgs_reg, by='userId')%>%
  left_join(movie_avgs_reg_bin, by=c('movieId','bin'))%>%
  mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)) %>%         
  pull(pred)
return(RMSE(predicted_ratings, test_set$rating))
}


n <- seq(380, 830, 50)
n1<-sapply(n,bins)
plot(n,n1)
n1<-n[which.min(n1)]
n1


