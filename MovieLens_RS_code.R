##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(gridExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                           title = as.character(title),
#                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#### QUIZ

#number of rows in edx dataset
nrow(edx)

#number of columns in edx dataset
ncol(edx)

#number of 0 and 3 ratings
edx %>% filter(rating==0) %>% count()
edx %>% filter(rating==3) %>% count()

#number of different movies
length(unique(edx$movieId))

#number of different users
length(unique(edx$userId))

#number of movie ratings for each ganre
str_detect(edx$genres, "Drama") %>% replace_na(FALSE) %>% sum()
str_detect(edx$genres, "Comedy") %>% replace_na(FALSE) %>% sum()
str_detect(edx$genres, "Thriller") %>% replace_na(FALSE) %>% sum()
str_detect(edx$genres, "Romance") %>% replace_na(FALSE) %>% sum()

#count ratings for movies
movieApp <- edx %>% group_by(movieId) %>% summarize(title = first(title), nr = n())
movieApp[order(-movieApp$nr) [1:5],]

#popularity of different ratings
edx %>% group_by(rating) %>% summarize(n=n()) %>% arrange(n)
  
#half star ratings vs full star ratings
edx %>% filter(rating %% 1 == 0) %>% count()
edx %>% filter(rating %% 1 == 0.5) %>% count()

####### START OF ANALYSIS

#column types
str(edx)

#number of rows for test and validation
nrow(edx)
nrow(validation)

#data preview
edx %>% head()

#number of users
length(unique(edx$userId))

#average number of ratings per user
nrow(edx)/length(unique(edx$userId))

#average rating
mean(edx$rating)

#number of users per average rating
edx %>% group_by(userId) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(x = avg_rating)) +
    geom_bar(stat = "bin", color = "black", fill = "blue")

#number of ratings per user
edx %>% group_by(userId) %>%
  summarize(n_rating = n()) %>%
  ggplot(aes(x = n_rating)) +
  geom_bar(stat = "bin", color = "black", fill = "blue", bins = 50)

#genres
#number of genre combinations
length(unique(edx$genres))


#most popular ganres
edx %>% group_by(genres) %>%
  summarize(number_occurances = n()) %>%
  arrange(desc(number_occurances))

#combinations with the most genres
edx %>% group_by(genres) %>%
  summarize(genres = first(genres), number_of_genres = str_count(genres, "\\|")) %>%
  arrange(desc(number_of_genres)) %>%
  head()


#best genres based on mean and median
edx %>% group_by(genres) %>%
  summarize(mean_rating = mean(rating), median_rating = median(rating)) %>%
  arrange(desc(mean_rating)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(genres, mean_rating))) +
    geom_point(aes(y = mean_rating, color = "mean")) +
    geom_point(aes(y = median_rating, color = "median")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#best genres based on mean and median with at least 50,000 entries
edx %>% group_by(genres) %>%
  summarize(number_entries = n(), mean_rating = mean(rating), median_rating = median(rating)) %>%
  filter(number_entries >= 50.000) %>%
  arrange(desc(mean_rating)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(genres, mean_rating))) +
  geom_point(aes(y = mean_rating, color = "mean")) +
  geom_point(aes(y = median_rating, color = "median")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#get all independent genres
genres <- edx %>% separate_rows(genres, sep = "\\|", convert = TRUE) %>%
  group_by(genres) %>%
  summarize(avg_rating = mean(rating), n_ratings = n())

#number of movies per genre
edx %>% separate_rows(genres, sep = "\\|", convert = TRUE) %>%
  group_by(genres) %>%
  summarize(number_of_movies = n()) %>%
  ggplot(aes(x = reorder(genres, number_of_movies), y = number_of_movies)) +
    geom_bar(stat="identity", fill="blue") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#most popular movies (most appearances)
movieApp <- edx %>% group_by(movieId) %>% summarize(title = first(title), nr = n(), avg_rating = mean(rating))
movieApp[order(-movieApp$nr) [1:5],]

#occurances by average rating
edx %>% group_by(movieId) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(x = avg_rating)) +
    geom_bar(stat="bin", color = "black", fill = "blue")

#ratings
edx %>% group_by(rating) %>%  
  ggplot(aes(x = rating)) +
    geom_bar(stat="bin", fill="blue") 

#prior 2003 (full stars only)
library(dplyr)
library(dslabs)
edx <- edx %>% mutate(timestamp1 = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), year = gsub("[\\(\\)]", "", regmatches(title, gregexpr("\\(.*?\\)", title))))
rating_prior2003 <- edx %>% filter(format(as.Date(timestamp1, format="%d/%m/%Y"),"%Y") < 2003) %>% 
  group_by(rating) %>% 
  ggplot(aes(x = rating)) +
  geom_bar(stat="bin", fill="blue") +
  ggtitle("Movie ratings before 2003")


#after 2003
rating_after2003 <- edx %>%  filter(format(as.Date(timestamp1, format="%d/%m/%Y"),"%Y") >= 2003) %>%
  group_by(rating) %>% 
  ggplot(aes(x = rating)) +
  geom_bar(stat="bin", fill="blue") +
  ggtitle("Movie ratings after 2003")

grid.arrange(rating_prior2003, rating_after2003, ncol=2)


#average rating by year compared to overall average
avg_rating <- mean(edx$rating)

edx %>% filter(year %in% c(1900:2015)) %>%
  group_by(year) %>%
  summarize(avg_rating_year = mean(rating), sd = sd(rating), n = n(), se = sd/sqrt(n)) %>% 
  ggplot(aes(x = year, y = avg_rating_year)) +
    geom_point() +
    geom_errorbar(aes(ymin=avg_rating_year-se, ymax=avg_rating_year+se)) +
    geom_hline(yintercept = avg_rating, color = "red") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
#number of movies through years
edx %>% filter(year %in% c(1900:2015)) %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
    geom_bar(stat="identity", fill = "blue") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#stats
quantile(edx$rating) 

#timestamp on rating
edx %>% group_by(year_review = format(as.Date(timestamp1, format="%d/%m/%Y"),"%Y")) %>%
  summarize(avg_rating = mean(rating), sd = sd(rating), n = n(), se = sd/sqrt(n)) %>%
  ggplot(aes(x = year_review, y = avg_rating)) +
    geom_point() +
    geom_errorbar(aes(ymin=avg_rating-se, ymax=avg_rating+se)) 

#number of ratings per timestamp
edx %>% group_by(year_review = format(as.Date(timestamp1, format="%d/%m/%Y"),"%Y")) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year_review, y = n)) +
    geom_bar(stat = "identity", fill = "blue")


##### RECOMMENDATION SYSTEM

#first we need to split edx data into train and test data
# Create train set and test sets from edx
set.seed(2021, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set) 
train_set <- rbind(train_set, removed)

# Remove temporary files to tidy environment
rm(test_index, temp, removed) 

#start with naive RMSE
avg_rating <- mean(train_set$rating)

naive_rmse <- RMSE(test_set$rating, avg_rating)

rmse_results <- data_frame(method = "Overall average", RMSE = naive_rmse)

#Next we try using averages from movie ratings
#get movie rating averages
movie_ratings <- train_set %>% group_by(movieId) %>% 
                   summarize(avg_rating_movie = mean(rating))
#apply these averages to test dataset as predictions
movie_averages_prediction <- test_set %>% left_join(movie_ratings, by="movieId") %>% .$avg_rating_movie

#compare predictions with true values
movie_mean_rmse <- RMSE(test_set$rating, movie_averages_prediction)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movies' averages",
                                     RMSE = movie_mean_rmse ))

#We will now also add the user effect on top
#get effect of user ratings
user_ratings_effect <- train_set %>% group_by(userId) %>%
                        summarize(avg_rating_user = mean(rating) - avg_rating)
#apply effect on previously calculated values
user_effect_predictions <- test_set %>% left_join(user_ratings_effect, by="userId") %>% .$avg_rating_user

movie_user_mean_rmse <- RMSE(test_set$rating, movie_averages_prediction + user_effect_predictions)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="+Users' averages",
                                     RMSE = movie_user_mean_rmse ))


#We will add effect of genre
#to do that, we will first split data to genres, calculate the effects and then merge it together
train_set_genres <- train_set %>% separate_rows(genres, sep = "\\|", convert = TRUE)
#set test_set ids so we will be able to merge them again later
test_set$id <- seq.int(nrow(test_set))
test_set_genres <- test_set %>% separate_rows(genres, sep = "\\|", convert = TRUE)

movie_ratings_effect <- movie_ratings 
movie_ratings_effect$avg_rating_movie <- movie_ratings_effect$avg_rating_movie - avg_rating

#get genre effect; we need to take away previously calculated effects
genre_ratings_effect <- train_set_genres %>% group_by(genres) %>% left_join(movie_ratings_effect, by="movieId") %>% 
                          left_join(user_ratings_effect, by="userId") %>% summarize(avg_rating_genre = mean(rating-avg_rating_movie-avg_rating_user-avg_rating))

genre_user_movie_effect_predictions <- movie_averages_prediction + user_effect_predictions + test_set_genres %>% left_join(genre_ratings_effect, by="genres") %>% 
                                        group_by(id) %>% summarize(title = first(title), avg_rating_genre = mean(avg_rating_genre))%>% .$avg_rating_genre


movie_user_genre_mean_rmse <- RMSE(genre_user_movie_effect_predictions, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="+Genres' averages",
                                     RMSE = movie_user_genre_mean_rmse ))


#add effect of user liking genre
genre_user_ratings_effect <- train_set_genres %>% group_by(genres, userId) %>% left_join(movie_ratings_effect, by="movieId") %>% 
                               left_join(user_ratings_effect, by="userId") %>% left_join(genre_ratings_effect, by="genres") %>% 
                                summarize(avg_rating_genre_user = mean(rating-avg_rating_movie-avg_rating_user-avg_rating-avg_rating_genre))

genreuser_genre_user_movie_effect_predictions <- genre_user_movie_effect_predictions + test_set_genres %>%
                                                  left_join(genre_user_ratings_effect, by=c("genres","userId")) %>% 
                                                  group_by(id) %>% summarize(title = first(title), avg_rating_genre_user = mean(avg_rating_genre_user))%>% .$avg_rating_genre_user
#replace NAs with average rating
genreuser_genre_user_movie_effect_predictions <- replace_na(genreuser_genre_user_movie_effect_predictions, avg_rating)

movie_user_genre_genreuser_mean_rmse <- RMSE(genreuser_genre_user_movie_effect_predictions, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="+Users' genres preferences",
                                     RMSE = movie_user_genre_genreuser_mean_rmse ))




#######################
### test with real data
avg_rating <- mean(edx$rating)

edx_genres <- edx %>% separate_rows(genres, sep = "\\|", convert = TRUE)
#set test_set ids so we will be able to merge them again later
validation$id <- seq.int(nrow(validation))
validation_genres <- validation %>% separate_rows(genres, sep = "\\|", convert = TRUE)

movie_ratings <- edx %>% group_by(movieId) %>% 
  summarize(avg_rating_movie = mean(rating))

movie_ratings_effect <- movie_ratings 
movie_ratings_effect$avg_rating_movie <- movie_ratings_effect$avg_rating_movie - avg_rating

user_ratings_effect <- edx %>% group_by(userId) %>%
  summarize(avg_rating_user = mean(rating) - avg_rating)

genre_ratings_effect <- edx_genres %>% group_by(genres) %>% left_join(movie_ratings_effect, by="movieId") %>% 
  left_join(user_ratings_effect, by="userId") %>% summarize(avg_rating_genre = mean(rating-avg_rating_movie-avg_rating_user-avg_rating))

genre_user_ratings_effect <- edx_genres %>% group_by(genres, userId) %>% left_join(movie_ratings_effect, by="movieId") %>% 
  left_join(user_ratings_effect, by="userId") %>% left_join(genre_ratings_effect, by="genres") %>% 
  summarize(avg_rating_genre_user = mean(rating-avg_rating_movie-avg_rating_user-avg_rating-avg_rating_genre))

movie_averages_prediction <- validation %>% left_join(movie_ratings, by="movieId") %>% .$avg_rating_movie

user_effect_predictions <- validation %>% left_join(user_ratings_effect, by="userId") %>% .$avg_rating_user

user_effect_predictions <- validation %>% left_join(user_ratings_effect, by="userId") %>% .$avg_rating_user

genre_user_movie_effect_predictions <- movie_averages_prediction + user_effect_predictions + validation_genres %>% left_join(genre_ratings_effect, by="genres") %>% 
  group_by(id) %>% summarize(title = first(title), avg_rating_genre = mean(avg_rating_genre))%>% .$avg_rating_genre

genreuser_genre_user_movie_effect_predictions <- genre_user_movie_effect_predictions + validation_genres %>%
  left_join(genre_user_ratings_effect, by=c("genres","userId")) %>% 
  group_by(id) %>% summarize(title = first(title), avg_rating_genre_user = mean(avg_rating_genre_user)) %>% .$avg_rating_genre_user
#replace NAs with average rating
genreuser_genre_user_movie_effect_predictions <- replace_na(genreuser_genre_user_movie_effect_predictions, avg_rating)

movie_user_genre_genreuser_mean_rmse_FINAL <- RMSE(genreuser_genre_user_movie_effect_predictions, validation$rating)

#misses of alghoritm
differences_prediction <- genreuser_genre_user_movie_effect_predictions - validation$rating
differences_prediction_2d <- format(round(differences_prediction, 2), nsmall = 2)
differences_prediction_2d <- as.numeric(differences_prediction_2d)

#plot distribution of errors
ggplot() + aes(differences_prediction_2d_n) + 
  geom_histogram(binwidth=0.1, colour="black", fill="blue")

skewness(differences_prediction_2d)
median(differences_prediction_2d)

#average miss
mean(abs(genreuser_genre_user_movie_effect_predictions - validation$rating))