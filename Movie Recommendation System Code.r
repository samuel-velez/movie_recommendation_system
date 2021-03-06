# Introduction

#The Netflix challenge offered $1 million dollars to the team that could provide a movie reccomendation system that improved theirs by 10%. While the Netflix data set isn't public, MovieLens provides a similar one. We are going to attempt to provide a solution to the challenge (through the MovieLens data set) using linear regression, trying out three different approaches.
#Each row in the MovieLens database corresponds to the rating one user assigned to a movie, and there's and ID that identifies each individual user.

#The following lines download the data set en prepare the data

library(dslabs)
library(tidyverse)
library(data.table)
library(caret)
data("movielens")

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#What follows is the total number of users who rated and the number of movies rated:

library(dplyr)

count_users <-  length(unique(movielens$userId))
count_movies <- length(unique(movielens$movieId))
data.frame(count_users,count_movies)

#However not every user rated every movie, and therefore many fields in the matrix will be empty, as we can see in the following sample plot highlighting the ratings we do have:

users <- sample(unique(movielens$userId), 100)
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "light grey")

# Pre-processing
#We are going to partition the data into a training and a test data subset, the first containing 90% of the data and the latter the remaining.

library(caret)

set.seed(1)
data_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
train_set <- movielens[-data_index,]
temp <- movielens[data_index,]

#To make sure we don't have users or movies in one set that don't appear in the other we use the semi_join function

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#The rows that were removed from test_set are put back into train_set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(dl, ratings, movies, data_index, temp, movielens, removed)

#The winners of the challenge were defined based on the Residual Sum of Squared Errors, RMSE. To win, the resulting RMSE had to be lower than 0.85.
#The RMSE was stated like this:

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Recommendation system

#To create the recommendation system, we will use an iterative process, increasing on the complexity of the previous attempt until the data set is fully exploited.

## Simplest solution: disregarding movie ratings and user preferences.
#The simplest possible solution is to suggest the same movie for all users, regardless of their preferences. This would be represented by a model that chalks up all variations in preferences to the standard error:

average_rating <- mean(train_set$rating)
average_rating

#Through the previous code, our model will only use the average rating and an error term. The root mean squared-error can be calculated as follows:

simple_rmse <- RMSE(test_set$rating,average_rating)
simple_rmse

#And so, the simplest possible solution provides an RMSE of a little over 1.

## Recommendation based on movie ratings
#Another approach would imply taking into account movie ratings. Some movies have a better average rating than others:

library(ggplot2)
t_movieIds <- table(movielens$movieId)
movielens$times_rated <- t_movieIds[match(movielens$movieId, names(t_movieIds))]
movielens %>% filter(times_rated >= 320 & movieId != 2085| times_rated == 41 & movieId != 2085) %>% ggplot(aes(title,rating))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab(element_blank()) + ylab("Rating")

#And we can use this for the recommendation system by accounting for individual movie ratings.
#In the Netflix challenge, the difference between the overall average rating and the average rating of each movie was called the bias, and we will implement it with the following code, that groups by movie and takes the average of its ratings, to later substract the total average:

movie_average <- train_set %>% 
  group_by(movieId) %>% 
  summarize(movie_effects = mean(rating - average_rating), times_rated = n())

#And we can see the distribution of this bias:
movie_average %>% qplot(movie_effects, geom ="histogram", bins = 10, data = ., color = I("grey")) + xlab("Movie effects") + ylab("Number of movies")

#The expressed model would be of the form Prediction = Overall Movie Average + Movie Bias + Random Error, and its RMSE is the following:

predicted_ratings <- average_rating + test_set %>% 
  left_join(movie_average, by='movieId') %>% 
  pull(movie_effects)

movie_effects_rmse <- RMSE(predicted_ratings, test_set$rating)
movie_effects_rmse

#Taking each movie's average rating into account improves our RMSE slightly.

## Recommendation based on individual preference and movie rating
#Introducing the individual preference of each user will add the last layer of information provided by the MovieLens data set:

#The following code is similar to the one used to find the movie bias; we are now finding each user's preference bias by removing both the movie effect and the mean overall rating effect.

user_average <- train_set %>% 
  left_join(movie_average, by='movieId') %>% 
  group_by(userId) %>% 
  summarize(user_effects = mean(rating - average_rating - movie_effects))

#Afterwards, with the user bias, we create a formula of the form: Prediction = Overall Movie Average + Movie Bias + User Bias + Error, so when a hard to please user who rates all movies below their average rating needs a suggestion, the effect of his manifested preferences will be negated against the movie's rating, and we can also identify correctly when he isn't bashing a movie but giving a personally above average review:

predicted_ratings <- test_set %>% 
  left_join(movie_average, by='movieId') %>%
  left_join(user_average, by='userId') %>%
  mutate(prediction = average_rating + movie_effects + user_effects) %>%
  pull(prediction)

#Our results so far:

user_effects_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- data_frame(method = "Average of all ratings", RMSE = simple_rmse)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effects Model", RMSE = movie_effects_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie and User Effects Model", RMSE = user_effects_rmse))
rmse_results

#Each time we have incorporated more information into the model our results have improved, but so far won't win the Netflix prize.

## Introducing regularization
#The biggest problem with our previous result is how skewed our movie effect prediction is with movies that receive very few ratings. As we can see in the following tables, most of our best and worst rated movies were rated less than 3 times, and most frequently just once. (Remember our movie bias is the difference between the movie's average rating and the average rating for all movies, and so it ranges from -3.04 to 1.45).

movie_titles_best <- movielens %>%
  select(movieId, title) %>%
  distinct()

movie_average %>% left_join(movie_titles_best, by="movieId") %>%
  arrange(desc(movie_effects)) %>%
  select(movieId,title,movie_effects,times_rated) %>%
  slice(1:5)

movie_titles_worst <- movielens %>%
  select(movieId, title) %>%
  distinct()

movie_average %>% left_join(movie_titles_worst, by="movieId") %>%
  arrange((movie_effects)) %>%
  select(movieId,title,movie_effects,times_rated) %>%
  slice(1:5)

#while the previous attempts were based on minimizing the residual sum of squares, using Regularization we will penalize these large estimates born of small sample sizes.
#To do this, we create a term that penalizes an estimate depending on its sample size, but will be negated with larger ones. How much we punish depends on the parameter lambda, the larger it is, the more we push estimates with small samples towards zero, but, the larger the sample is, the more lambda will be ignored.
#To choose the best lambda to calculate the the movie bias we will use cross validation, testing a series of lambda values and extracting the one that minimizes the RMSE:

lambdas <- seq(0, 10, 0.25)

rating_minus_average_rating <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - average_rating), n_i = n())

RMSE <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(rating_minus_average_rating, by='movieId') %>% 
    mutate(regularized_movie_effects = s/(n_i+l)) %>%
    mutate(pred = average_rating + regularized_movie_effects) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, RMSE)  
lambdas[which.min(RMSE)]

optimal_lambda <- lambdas[which.min(RMSE)]

regularized_movie_effects <- train_set %>% 
  group_by(movieId) %>% 
  summarize(movie_effects = sum(rating - average_rating)/(n()+optimal_lambda), times_rated = n())  

#And here is a comparison between the regularized estimates and the least square estimates, with dot size representing sample size (in square root to facilitate visualization):

data_frame(original = movie_average$movie_effects, 
           regularized = regularized_movie_effects$movie_effects, 
           times_rated = regularized_movie_effects$times_rated) %>%
  ggplot(aes(regularized, original, size=sqrt(times_rated))) + 
  geom_point(shape=1, alpha=0.5)

#As we can see, the previous extremes with small samples now gravitate around the center.
#These are our new best and worst movies, which make more sense:

movie_titles_best <- movielens %>%
  select(movieId, title) %>%
  distinct()

regularized_movie_effects %>% left_join(movie_titles_best, by="movieId") %>%
  arrange(desc(movie_effects)) %>%
  select(movieId,title,movie_effects,times_rated) %>%
  slice(1:5)

movie_titles_worst <- movielens %>%
  select(movieId, title) %>%
  distinct()

regularized_movie_effects %>% left_join(movie_titles_worst, by="movieId") %>%
  arrange((movie_effects)) %>%
  select(movieId,title,movie_effects,times_rated) %>%
  slice(1:5)

#While the aforementioned examples are of the movie effects part of our model, the user effects also suffer the same effect, and so this regularization will be applied to it as well.
#To choose lambda for both the movie and user biases we will use cross validation to minimize the RMSE again:

lambdas <- seq(0, 10, 0.25)

RMSE <- sapply(lambdas, function(l){
  
  movie_effect_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(movie_effect_i = sum(rating - average_rating)/(n()+l))
  
  user_effect_u <- train_set %>% 
    left_join(movie_effect_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(user_effect_u = sum(rating - movie_effect_i - average_rating)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(movie_effect_i, by = "movieId") %>%
    left_join(user_effect_u, by = "userId") %>%
    mutate(pred = average_rating + movie_effect_i + user_effect_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, RMSE)

lambdas[which.min(RMSE)]
regularized_rmse <- min(RMSE)

#And so, we use this optimal lambda for our regularized estimation, and obtain the following RMSE:

regularized_rmse
#This is the best RMSE we have achieved so far, and it is probably the best we will receive.

# Result and conclusions
#With each iteration our results got progressively better, and while this result inched us closer to the $1 million dollars prize, it fell short of it.

#This is a fun exercise that only needs linear models and regularization, that proved to be easy to understand and implement. The limitations of this approach come from linear regressions, and the fact that they aren't learning models, and are therefore very strict on their use, as any input outside of the initial "training" will provide a theoretically erroneous result.

rmse_results <- data_frame(method = "Average of all ratings", RMSE = simple_rmse)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effects Model", RMSE = movie_effects_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie and User Effects Model", RMSE = user_effects_rmse))
rmse_results <- bind_rows(rmse_results,data_frame(method="Regularized Movie Effects and Regularized User Effects",RMSE = regularized_rmse))
transform(rmse_results, RMSE = as.character(RMSE))
