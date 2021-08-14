###############################################################################
###############################################################################

## ML algorithm for Harvardx Capstone Course
## Version: 1.0
## Author: Ruben Vázquez del Valle
## Comments: 10M version of movielens dataset has been prepared by edx and splitted in two datasets:
##           - edx dataset for creating and training the model
##           - Validation dataset for validation purposes only


###############################################################################
###############################################################################
##  Initialize parameters
###############################################################################
###############################################################################
l1<-2.75
l2<-5
a<- -0.00075
b<- 0.4
n<-780

##############################################################################
###############################################################################


###############################################################################
###############################################################################
##        Functions needed for the project
###############################################################################
###############################################################################


## RMSE function to compute Root Mean Squared errors between any pair of vectors 
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}


## MAE function to compute Mean Absolute errors between any pair of vectors 
MAE <- function(true, predicted){
  mean(abs(true - predicted))
}


## substrRight function to substract n last characters on a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



## rating_stats_bygenre function to calculate statistics on a data frame df by genre s
rating_stats_bygenre<-function(df,avg,s){
  aux<- df  %>% filter(grepl(s,genres)) %>%  summarize(n=n(),mu=mean(rating),sigma=sd(rating),b_g=mean(rating - avg - b_i_reg - b_u_reg - ifelse(is.na(b_i),0,b_i) - dev_u_t))
  tibble(genre=s,n=aux$n,mu=aux$mu,sigma=aux$sigma,b_g=aux$b_g)  
}

## agg_sum_b_g to aggregate genre bias on genre g for those movies belonging to more than one genre
agg_sum_b_g<-function(df,g){
  auxdf<- df  %>% filter(grepl(g,genres)) %>% mutate(new_b_g=all_genres_stats$b_g[which(all_genres_stats$genre==g)]) %>% select(userId,movieId,new_b_g)
  df<- left_join(df, auxdf, by = c("userId","movieId")) %>% mutate(sum_b_g=ifelse(is.na(new_b_g),sum_b_g,sum_b_g+new_b_g)) %>% select(-new_b_g)
}


## lambda1 function to regularize parameter λ1 according to The BellKor Solution pdf
lambda1<-function(l1){
  mu <- mean(train_set$rating)
  movie_avgs_reg <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l1))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
}


## lambda2 function to regularize parameter λ2 according to The BellKor Solution pdf
lambda2<-function(l2){
  mu <- mean(train_set$rating)
  user_avgs <- train_set %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i_reg)/(n()+l2))
  
  predicted_ratings <- test_set %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i_reg + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
}

## dev1 function to calculate α(u) on dev_u(t) (temporal effect on user bias) according to the BellKor Solution pdf after applying  bi(t) = bi +bi,Bin(t)
dev1<-function(a){
  movie_avgs_reg<- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i_reg = sum(rating - mu)/(n()+l1),first_rate_i=min(date))
  
  date_user_avgs_reg <- train_set %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - mu - b_i_reg)/(n()+l2),t_u = mean(date))
  
  movie_avgs_reg_bin<-train_set %>% 
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(date_user_avgs_reg, by='userId')  %>%
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
    )  %>%  group_by(movieId,bin) %>% 
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
    mutate(dev_u_t=as.numeric(difftime(date,t_u,units="days"))) %>%
    mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b)) %>%
    mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)+dev_u_t) %>%         
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
}


## dev2 function to calculate β(u) on dev_u(t) (temporal effect on user bias) according to the BellKor Solution pdf after applying movies bias reg as time function bi(t) = bi +bi,Bin(t)
dev2<-function(b){
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
    )  %>% group_by(movieId,bin) %>% 
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
    mutate(dev_u_t=as.numeric(difftime(date,t_u,units="days"))) %>%
    mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b)) %>%
    mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)+dev_u_t) %>%         
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
}


###############################################################################
###############################################################################


## Preprocessing original dataset to add info


library(lubridate)
library(purrr)
library(RColorBrewer)
library(dplyr)

## Release Year is included in the title, adding a column with the year

edx<-edx %>% mutate( date = as_datetime(timestamp),year=as.numeric(str_extract(str_extract(substrRight(title,6),"\\([^()]+.\\d"),"\\d+\\d")))

## Adding a column b_g, initialized to 0 that will contain Σ(b_g), the sum of all genre bias that each movie belongs to
edx<-edx %>% mutate(sum_b_g=0)


## Selecting a random seed to allow replicability
set.seed(1978, sample.kind="Rounding")


## Creating training a testing partitions on edx movielens dataset 

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]


## Making testing partition comparable by taking out movies and users not present on training partition   

test_set <-test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


###############################################################################
##        Modelling
##        I.) BASELINE
###############################################################################




################## 1. Adding Naive model
mu_hat <- mean(train_set$rating)
mu_hat
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_mae <- MAE(test_set$rating, mu_hat)

## Save results to table
Results <- tibble(method = "Naive", RMSE = naive_rmse, MAE = naive_mae)



################## 2. Adding movies bias

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
movbias_rmse<-RMSE(predicted_ratings, test_set$rating)
movbias_mae<-MAE(predicted_ratings, test_set$rating)

## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias", RMSE = movbias_rmse, MAE = movbias_mae))


################## 2.1. Adding movies bias regularized 
##
## Following recommendations made by The BellKor Solution pdf, averages are shrunk towards zero by using the regularization 
## parameters, λ1,λ2, which are determined by validation on the test set

l <- seq(0, 20, 0.25)
l1<-l[which.min(sapply(l,lambda1))]
l1

movie_avgs_reg<- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i_reg = sum(rating - mu)/(n()+l1),first_rate_i=min(date))

predicted_ratings <- 
  test_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  mutate(pred = mu + b_i_reg) %>%
  pull(pred)

movbiasreg_rmse<-RMSE(predicted_ratings, test_set$rating)
movbiasreg_mae<-MAE(predicted_ratings, test_set$rating)


## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias regularized", RMSE = movbiasreg_rmse, MAE = movbiasreg_mae))



################## 3.Adding user bias

user_avgs <- train_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i_reg))

qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i_reg + b_u) %>%
  pull(pred)
movuserbias_rmse<-RMSE(predicted_ratings, test_set$rating)
movuserbias_mae<-MAE(predicted_ratings, test_set$rating)

## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias reg + User bias", RMSE = movuserbias_rmse,MAE = movuserbias_mae))


################## 3.1. Adding user bias regularized 
##
## Following recommendations made by The BellKor Solution pdf, averages are shrunk towards zero by using the regularization 
## parameters, λ1,λ2, which are determined by validation on the test set

l <- seq(5, 25, 1)
l2<-l[which.min(sapply(l,lambda2))]
l2

user_avgs_reg <- train_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u_reg = sum(rating - mu - b_i_reg)/(n()+l2),t_u = mean(date))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  mutate(pred = mu + b_i_reg + b_u_reg) %>%
  pull(pred)
movuserbiasreg_rmse<-RMSE(predicted_ratings, test_set$rating)
movuserbiasreg_mae<-MAE(predicted_ratings, test_set$rating)

## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias reg + User bias reg", RMSE = movuserbiasreg_rmse,MAE = movuserbiasreg_mae))


################## 4.1 Adding movies bias regularized as a function of time segmented in bins
## Following recommendations made by The BellKor Solution pdf bi(t) = bi +bi,Bin(t) 
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
  )  %>% group_by(movieId,bin) %>% 
  summarize(b_i = mean(rating - mu - b_i_reg - b_u_reg))

qplot(b_i, data = movie_avgs_reg_bin, bins = 10, color = I("black"))

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
  left_join(user_avgs_reg, by='userId')%>%
  left_join(movie_avgs_reg_bin, by=c('movieId','bin'))%>%
  mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)) %>%         
  pull(pred)
movbiasregbin_rmse<-RMSE(predicted_ratings, test_set$rating) 
movbiasregbin_mae<-MAE(predicted_ratings, test_set$rating)



## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias reg + User bias reg+ time effect on movie Bias reg", RMSE = movbiasregbin_rmse, MAE = movbiasregbin_mae))


################## 4.2 Adding user rating date bias  

##
## Following recommendations made by The BellKor Solution pdf, the users tend to change their baseline ratings over time including
## a natural drift in a user’s rating scale, so we build user bias bu as a function of time dev_u(t) = α·sign(t −tu)·|t −tu|^β
## α,β are determined by validation on the test set

a<- seq(-0.0015,0.0005,0.00025)
b<-0.4                               # initial β proposed in The BellKor Solution paper
devut<-sapply(a,dev1)
a<-a[which.min(devut)]
a

b <- seq(0, 1,0.1)
devut<-sapply(b,dev2)
b<-b[which.min(devut)]
b


## We took advantage of user_avgs_reg not only for calculating b_u_reg but also t_u, so we do not need to calculate it here
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
  mutate(dev_u_t=as.numeric(difftime(date,t_u,units="days"))) %>%
  mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b)) %>%
  mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)+dev_u_t) %>%         
  pull(pred)
usertimebias_rmse<-RMSE(predicted_ratings, test_set$rating)
usertimebias_mae<-MAE(predicted_ratings, test_set$rating)

## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias reg + User bias reg+ time effect on movie Bias reg + dev_u(t)", RMSE = usertimebias_rmse,MAE = usertimebias_mae))



################## 5.Adding genre bias  
genre_avgs<-  train_set %>% 
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
  mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b))



## Extracting different existing genres
all_genres<-genre_avgs  %>% select(genres) %>% group_by(genres) %>% summarize(n=n())  %>% pull(genres)
all_genres<-enframe(str_split(all_genres, pattern="\\|")) %>% unnest(value) %>% group_by(value) %>% summarize(n=n())
all_genres<-all_genres %>% mutate(genre=value) %>% select(genre)


## Let's start by calculating average rating by genre and genre bias b_g
all_genres_stats<-tibble(.rows = NULL)
for(i in (1:length(all_genres$genre))){
  all_genres_stats<-rbind.data.frame(all_genres_stats,rating_stats_bygenre(genre_avgs,mu,all_genres$genre[i]))
}

all_genres_stats %>% ggplot(aes(x=genre,y=mu)) +
  geom_point() +
  ylab("Average Ratings over general average") +
  geom_errorbar(aes(ymin=mu-sigma, ymax=mu+sigma,col=genre)) +
  geom_hline(yintercept=mu)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## Let's now aggregate genre bias to test_set dataset for each genre present in all_genres

for(i in (1:length(all_genres$genre))){
  test_set<-agg_sum_b_g(test_set,all_genres$genre[i])
}

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
  mutate(dev_u_t=as.numeric(difftime(date,t_u,units="days"))) %>%
  mutate(dev_u_t=a*sign(dev_u_t)*(abs(dev_u_t)^b)) %>%
  mutate(pred = mu + b_i_reg + b_u_reg + ifelse(is.na(b_i),0,b_i)+dev_u_t+sum_b_g) %>%         
  pull(pred)
movusergenrebias_rmse<-RMSE(predicted_ratings, test_set$rating)
movusergenrebias_mae<-MAE(predicted_ratings, test_set$rating)

## Save results to table
Results<-rbind(Results,tibble(method = "Movie Bias reg + User bias reg+ time effect on movie Bias reg + dev_u(t) + Genre bias", RMSE = movusergenrebias_rmse,MAE = movusergenrebias_mae))


################## 6.Adding bias  number of rates  --- tasks to be added 6.2 exam


train_set %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


test_set %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer movies, 
## starting in 1993, the number of ratings decreases with year: the more recent a movie is, the less time users have had to rate it.


## The more often a movie is rated, the higher its average rating.

## It will be needed to fill in the missing values with a lower value than the average rating across all movies.

edx %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

## It will be needed to fill in the missing values with a lower value than the average rating across all movies.

edx %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()