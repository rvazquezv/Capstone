library(tidyverse)
library(caret)
library(data.table)

head( all_genres_stats)




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


y <-as_tibble(predicted_ratings)


save(edx, file = "rdas/y.rda")

z<- y %>% pivot_wider(names_from = "movieId", values_from = "resid") %>%
  as.matrix()


rownames(z)<- z[,1]
z <- z[,-1]

movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

colnames(z) <- with(movie_titles, title[match(colnames(z), movieId)])

save(z, file = "rdas/z.rda")


m_1 <- "Godfather, The (1972)"
m_2 <- "Godfather: Part II, The (1974)"
p1 <- qplot(z[ ,m_1], z[,m_2], xlab = m_1, ylab = m_2)


cor(z, use="pairwise.complete")

z[is.na(z)] <- 0
pca <- prcomp(y)


for(i in (1:nrows(z))){
  for (j in (1:nrows(z))){
    z[i,j]<-if(is.na(z[i,j]),0,z[i,j])
  }
}


for(i in (1:nrow(z))){
  for (j in (1:ncol(z))){
    cat(i,j)  }
}