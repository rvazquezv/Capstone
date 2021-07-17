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


###############################################################################
###############################################################################



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

## Save results to table
rmse_results <- tibble(method = "Naive", RMSE = naive_rmse)



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

## Save results to table
rmse_results<-rbind(rmse_results,tibble(method = "Movie Bias", RMSE = movbias_rmse))



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

## Save results to table
rmse_results<-rbind(rmse_results,tibble(method = "Movie Bias + User bias", RMSE = movuserbias_rmse))



################## 4.Adding 

