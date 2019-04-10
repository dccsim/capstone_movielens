#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# set data source
#
# 1: local PC folder (c:/apps/data/r/ml)
# 2: local mac folder (~/Desktop/movielen)
# others: download
# 
# if using local data source: 
# - place movies.dat and ratings.dat in subfolder "ml-10M100K" of the PC/mac folders above
data_source <- 0

if (data_source == 1) {
  print("using data on local windows PC")
  setwd("c:/apps/data/r/ml")
} else if (data_source == 2) {
    print("using data on local mac")
    setwd("~/Codes/movielens")
} else {
  print("download data from GroupLens")
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

if (data_source %in% c(1,2)) {
  print("use local data")
  movies <- str_split_fixed(readLines("ml-10M100K/movies.dat", encoding = "UTF-8"), "\\::", 3)
} else {
  print("unzip downloaded data")
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
} 

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

# Replaced read.table() with fread() for faster data loading
#
# ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                     col.names = c("userId", "movieId", "rating", "timestamp"))

if (data_source %in% c(1,2)) {
  print("use local data")
  ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
} else {
  print("unzip downloaded data")
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                col.names = c("userId", "movieId", "rating", "timestamp"))
} 

movielens <- left_join(ratings, movies, by = "movieId")

# Extract more information

# Movie Year: extract movie release year

# a feature of gsub it returns the input string if there are no matches to the supplied pattern
# matches returns the entire string, thus this modified gsub() to return empty na if that's the case
my_gsub <- function(pattern, replacement, x) {
  ans <- ifelse(grepl(pattern=pattern, x=x), 
                gsub(pattern=pattern, replacement=replacement, x=x), 
                NA)
  return(ans)
}

movielens <- movielens %>%
  mutate(movie_year = my_gsub(pattern = ".*?\\(([0-9]{4})\\)$", replacement = "\\1", x = title))

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# remove unsed data
# rm(dl, ratings, movies, test_index, temp, movielens, removed)
if (!(data_source %in% c(1,2))) { 
  rm(dl) 
}
rm(test_index, temp, removed)

# Quick look at the data set
movielens %>% 
  summarize(num_users = n_distinct(userId),
            num_movies = n_distinct(movieId))

as_tibble(movielens)

# User rating counts
sorted_user_rating <- ratings %>% 
  group_by(userId) %>% 
  summarise(user_rating_count=n()) %>% 
  arrange(user_rating_count) 
head(sorted_user_rating) 

# Movies not rated
movies_all <- as.numeric(unique(movies$movieId))
movies_rated <- as.numeric(unique(ratings$movieId))
movies_unrated <- setdiff(movies_all, movies_rated)
as_tibble(movies %>% 
            filter(movieId %in% movies_unrated))

# Movies with empty/unknown rating
sum(is.na(movielens$rating))

# 1: some movies get rated more than the rest - see the distribution
# there are blockbusters watched by millions and independent films watched by a few
# 
# rating_count %>% qplot(rating_count, geom="histogram", bins=20, data=., color=I("black"), log="x")
movie_rating_count <- movielens %>% group_by(movieId) %>% summarise(movie_rating_count=n()) 
movie_rating_count %>% 
  ggplot(aes(movie_rating_count)) + 
  geom_histogram(bins=20, color="black", fill="grey", alpha=0.3) + 
  ggtitle("Histogram: rating counts across movies") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rating counts (Log10)") +
  ylab("Number of movies") +
  scale_x_log10()

# 2. User activity in rating movies: some users rated over a thousand movies, 
# with most rated in hundreds or less; and some rated only a few
user_rate_count <- movielens %>% group_by(userId) %>% summarise(user_rating_count=n())
user_rate_count %>% 
  ggplot(aes(user_rating_count)) + 
  geom_histogram(bins=20, color="black", fill="grey", alpha=0.3) + 
  ggtitle("Histogram: rating counts of users") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Rating counts (Log10)") +
  ylab("Number of users") +
  scale_x_log10()

# 3. each movie has a different average rating (filter by movies that received more then 10 ratings)
movie_avg_rating <- movielens %>% 
  group_by(movieId) %>%
  filter(n()>10) %>%
  summarise(avg_rating=mean(rating))
movie_avg_rating <- cbind(movie_num=1:nrow(movie_avg_rating), movie_avg_rating)
movie_avg_rating %>% 
  ggplot(aes(x=movie_num,y=avg_rating)) + 
  geom_point(alpha=0.2, color="#33a02c") +
  ggtitle("Average rating for each movie") +
  xlab("Movies in data set") +
  ylab("Average rating of each movie") + 
  scale_x_continuous(breaks = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# 4. each user has a different average rating (filter by users who rated more than 300 movies)
user_avg_rating <- movielens %>% 
  group_by(userId) %>% 
  filter(n()>300) %>%
  summarise(avg_rating=mean(rating))
user_avg_rating <- cbind(user_num=1:nrow(user_avg_rating), user_avg_rating)
user_avg_rating %>% 
  ggplot(aes(x=user_num,y=avg_rating)) + 
  geom_point(alpha=0.2, color="#1f78b4") +
  ggtitle("Average rating for each user") +
  xlab("Users in data set") +
  ylab("Average rating given by each user") + 
  scale_x_continuous(breaks = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# 5. each year has a different average rating 
year_avg_rating <- movielens %>% 
  group_by(movie_year) %>% 
  summarise(avg_rating=mean(rating))
year_avg_rating$movie_year <- as.numeric(year_avg_rating$movie_year)
year_avg_rating %>% 
  ggplot(aes(x=movie_year,y=avg_rating)) + 
  geom_point(alpha=0.8, color="#ff7f00") +
  ggtitle("Average rating by movie release year") +
  xlab("Year") +
  ylab("Average rating for movies released in each year") + 
  scale_y_continuous(breaks = seq(0, 5, 0.5)) +
  ylim(3,4.5) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
#        axis.text.x=element_text(angle=90,hjust=1),
        panel.border = element_blank(),
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = "white"))

# define the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# further split edx into train/test sets - to experiment with different models
edx_test_ratio <- 0.1
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = edx_test_ratio, list = FALSE)
edx_train <- edx[-edx_test_index,]
temp_test <- edx[edx_test_index,]

# make sure userId and movieId in edx_test set are also in edx_train set
edx_test <- temp_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# add rows removed from edx_test set back into edx_train set
edx_removed <- anti_join(temp_test, edx_test)
edx_train <- rbind(edx_train, edx_removed)

# remove temp data
rm(temp_test, edx_test_index, edx_removed)

# completed creating edx_train and edx_test sets created from edx with ratio = test_ratio

# try out: naive
# predict average rating for all movies 
mu_hat <- mean(edx_train$rating)
mu_hat

# see how this model performs by predicting the average; it returns rmse > 1
naive_rmse <- RMSE(edx_test$rating, mu_hat)
naive_rmse

# create a table to store the results for each model, starting with the first simple model
rmse_results <- data_frame(method = "Predicting the average", RMSE = naive_rmse)
kp <- 5
rmse_results %>% knitr::kable(padding=kp)

# in fact, predicting any other numbers shows even a larger rmse
predictions <- rep(2.5, nrow(edx_test))
RMSE(edx_test$rating, predictions)

predictions <- rep(2, nrow(edx_test))
RMSE(edx_test$rating, predictions)


# conventions: movie i, user u

# try out: lm on movie effect

# we could use least squares to estimate the b_i. however, the lm function will be slow due to thousands of b_i and this could take a long time
# fit <- lm(rating ~ as.factor(userId), data = movielens)  
# we use this instead for each movie i: 
# b_i = y(u,i) - overall mean 
mu <- mean(edx_train$rating) 
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# the estimates varies substantially as shown in this plot - suggesting variability in average rating across different movies
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- mu + edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable(padding=kp)

# try out: lm on movie+user effects
# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# the estimates varies substantially as shown in this plot - suggesting variability in average rating across different users
user_avgs %>% qplot(b_u, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable(padding=kp)

# try out: lm on movie+user+year effects
# lm(rating ~ as.factor(movieId) + as.factor(userId) + as.factor(movie_year))
year_avgs <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

# the estimates varies substantially as shown in this plot - suggesting variability in average rating across different year of movie release
year_avgs %>% qplot(b_y, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='movieId') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable(padding=kp)

# store the rsme_results of experimenting with edx_train/edx_test
rmse_results_e <- rmse_results

## 
## change to train set (edx) and test set (validation)
##

# try out: naive
mu_hat <- mean(edx$rating)
mu_hat

# see how this model performs by predicting the average; it returns rmse > 1
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

# create a table to store the results for each model, starting with the first simple model
rmse_results <- data_frame(method = "Predicting the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable(padding=kp)

# in fact, predicting any other numbers shows even a larger rmse
predictions <- rep(2.5, nrow(validation))
RMSE(validation$rating, predictions)

predictions <- rep(2, nrow(validation))
RMSE(validation$rating, predictions)

# try out: lm on movie effect
# fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# the estimates varies substantially as shown in this plot - suggesting variability 
# in average rating across different movies
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable(padding=kp)

# try out: lm on movie+user effects
# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# the estimates varies substantially as shown in this plot - suggesting variability 
# in average rating across different users
user_avgs %>% qplot(b_u, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable(padding=kp)

# try out: lm on movie+user+year effects
# lm(rating ~ as.factor(movieId) + as.factor(userId) + as.factor(movie_year))
year_avgs <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

# the estimates varies substantially as shown in this plot - suggesting variability 
# in average rating across different years in which the movie is released
year_avgs %>% qplot(b_y, geom ="histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs, by='movieId') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable(padding=kp)

# RMSE returned by testing the final algorithm on the validation set
model_3_rmse

