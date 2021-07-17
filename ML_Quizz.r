str(edx)
dim(edx)

################################
edx %>% filter(rating == 0) %>% tally()
edx%>%filter(rating==3)%>%group_by()%>%summarize(n=n())

################################

edx%>%group_by(movieId)%>%summarize(n=n())

################################

n_distinct(edx$userId)

################################


edx%>%group_by(genres)%>%summarize(n=n())%>%filter(genres %in% c("Drama","Comedy","Thriller","Romance"))



edx%>%filter(genres %like% c("Drama","Comedy","Thriller","Romance"))%>%group_by(genres)%>%summarize(n=n())

edx%>%filter(genres %like% "Drama")
sum(str_detect(edx$genres, "Drama"))
edx%>%filter(genres %like% "Comedy")
edx%>%filter(genres %like% "Thriller")
edx%>%filter(genres %like% "Romance")


# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


################################
"Forrest Gump"
"Jurassic Park"


edx%>%filter(title %like% "Gump")%>%group_by(movieId)%>%summarize(n=n())
sum(str_detect(edx$title,"Jurassic"))
edx%>%filter(title %like% "Jurassic")%>%group_by(movieId)%>%summarize(n=n())

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


################################
hist(edx$rating)
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

################################

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


