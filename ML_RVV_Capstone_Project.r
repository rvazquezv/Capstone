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


###############################################################################
###############################################################################


## Preprocessing original dataset to add info

library(lubridate)

## Year is included in the title, adding a column with the year

edx<-edx %>% mutate( date = as_datetime(timestamp),year=as.numeric(str_extract(str_extract(title,"\\([^()]+.\\d"),"\\d+\\d")))



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




## Some analysis on dataset
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

users<-sample(edx$userId,500,replace=FALSE)
movies<-sample(edx$movieId,500,replace=FALSE)
plot(movies,users)


plot(test_set$movieId,test_set$userId)




################## 1.Adding Naive model
mu_hat <- mean(train_set$rating)
mu_hat
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_mae <- MAE(test_set$rating, mu_hat)

## Save results to table
rmse_results <- tibble(method = "Naive", RMSE = naive_rmse, MAE = naive_mae)



################## 2.Adding movies bias

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
rmse_results<-rbind(rmse_results,tibble(method = "Movie Bias", RMSE = movbias_rmse, MAE = movbias_mae))



################## 3.Adding user bias

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movuserbias_rmse<-RMSE(predicted_ratings, test_set$rating)
movuserbias_mae<-MAE(predicted_ratings, test_set$rating)

## Save results to table
rmse_results<-rbind(rmse_results,tibble(method = "Movie Bias + User bias", RMSE = movuserbias_rmse,MAE = movuserbias_mae))



################## 4.Adding bias by time, number of rates and genre --- tasks to be added 6.2 exam


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