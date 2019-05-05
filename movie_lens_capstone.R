#Create test and validation sets
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 25% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.25, list = FALSE)
edx <- movielens[-test_index,] #train set
temp <- movielens[test_index,] #validation set

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

########################################################################################
#start of creating Test and Validation sets
########################################################################################

#Let's write a function that computes the RMSE for vectors of ratings and their corresponding predictors:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#We compute the average rating of all movies across all users on the training data
mu_hat <- mean(edx$rating)
mu_hat

#And then we compute the residual mean squared error on the test set data.
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse


#As we go along, we will be comparing different approaches. Let's start by creating a results table with 
#this naive approach:
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#We know that the average ranking for a movie is just the average of rating - mu. We can compute it 
#like this:
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#We can see that these estimates vary substantially:
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#predicted ratings(^Yu,i) =  overall average(^mu) + average rating for a movie (^bi)
#We can use this code and see that our RMSE did drop a little bit.
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i


rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = rmse ))
rmse_results %>% knitr::kable()

rmse