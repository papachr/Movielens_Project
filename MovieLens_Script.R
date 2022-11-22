### Import data

#First of all, we are going to load some necessary packages.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(scales)
library(lubridate)
library(recosystem)

# Then, we are going to download the data
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#and now, we will do some data wrangling in order to get a tidy form of our data

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#We will split our data to those that will be used to train our algorith, and to
#a validatiion set that will be used only to test how "good" is our final model.

# So, the Validation set will be 10% of the movielens dataset

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Furthermore, we have to make sure that the users (userId) and the movies (movieId)
#in the validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# We add the rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#and we remove temporary files
rm(dl, ratings, movies, test_index, temp, movielens, removed)


### Data exploration and visualization


# We will noow explore our data
str(edx)

# Let us now see the number of distinct movies
n_distinct(edx$movieId)

# and the number of distinct users
n_distinct(edx$userId)

# Let us now have a look at our data with the summary statistics
summary(edx)

# We will plot the ratings of the users and observe that the users tend to rate above the average
plot_ratings<-edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.2, color = "black")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+
  labs(x="rating", y="count in millions")

plot_ratings

#The number of ratings by movieId will be plotted
plot_movies<- edx%>%count(movieId)%>%ggplot(aes(n))+
  geom_histogram(bins = 15, color = "black", fill="navy")+scale_x_log10()+
  ggtitle("Number of ratings by movieId")+labs(x="movieId",y="number of ratings")

plot_movies

# The number of ratings by userId will be plotted
plot_users<-edx%>%count(userId)%>%ggplot(aes(n))+
  geom_histogram(bins = 15, color = "black", fill="darkolivegreen")+scale_x_log10()+
  ggtitle("Number of ratings by userId")+labs(x="userId",y="number of ratings")

plot_users

# We will continue with plotting the mean rating for movies and for users
plot_mean_movies<-edx %>% group_by(movieId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins = 15, color = "black",fill="navy")+ylab("number of movies")+
  ggtitle("Mean rating for movies")

plot_mean_movies

plot_mean_users<-edx %>% group_by(userId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins = 15, color = "black", fill="darkolivegreen")+ylab("number of users")+
  ggtitle("Mean rating for users")

plot_mean_users

# We will plot the average ratings for genres and more specifically for categories with more than 100000 ratings
plot_genre<-edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("average rating")+ggtitle("Average ratings for genres (categories with more than 100,000 ratings)")

plot_genre

# The edx dataset also includes the variable timestamp. This variable represents the 
# time and date in which the rating was provided. We will create a new column with the 
# date rounding it to the nearest week and without taking into consideration the time.
# Then, we will compute the average rating for each week and plot this average against date. 

edx<-edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "week"))

plot_date<-edx %>% 
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +ylab("average rating")+ggtitle("Average rating against date")

plot_date

# Now, we will print the first 6 rows of the title variable
edx$title%>%head()

#We are going to split this column into two, the one with the title and the other with the year of the release. 
edx<-edx%>%
  extract(title, c("title", "release_year"), regex = "^(.*) \\(([0-9]*)\\)$")
edx$release_year<-as.numeric(edx$release_year)

#We will now check if the release year affects the ratings. First of all, we will 
#plot the number of ratings for each movie against the release year of the movie.
#we will plot the release_year after 1950 since until this year the number of ratings was quite small.
plot_year<-edx %>% filter(release_year>1950)%>%group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(release_year))) %>%
  ggplot(aes(year, n)) + geom_bar(stat="identity", fill="skyblue3")+
  scale_y_continuous(labels = label_number())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("release_year")+
  ylab("number of ratings")

plot_year


# Let us now plot the average rating against the ratings per year. We will use 
#2009 as the end year in order to compute the number of ratings per year.
plot_ratings_year <- edx %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(release_year),
            average_rating = mean(rating)) %>%
  mutate(ratings_per_year = n/years) %>%
  ggplot(aes(ratings_per_year, average_rating)) +
  geom_point() +
  geom_smooth()

plot_ratings_year


### Modeling approach

# We will use the root mean square error to check the performance of the models that we will create.
# In R we can write the following function to compute it:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#As we build our algorithm, we have to check the RMSE values for each model that we will create 
#till we get to the preferred one. However, because the validation set will be kept as the final 
#hold-out test set, we will proceed with splitting the edx set to a test set and a training set. 
#we will split our edx dataset to a 80:20 ratio, the training and the test set respectively.

set.seed(755, sample.kind="Rounding")
test_ind <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_ind,]
test_temp <- edx[test_ind,]

#We make sure that the users (userId) and the movies (movieId) in the  test set are also in the train set
test_set <- test_temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#We add the rows removed from test_set back into the train_set
removed_1 <- anti_join(test_temp, test_set)
train_set <- rbind(train_set, removed_1)


# We remove the temporary files
rm(test_ind, test_temp, removed_1)


#### Simplest model

# We assume the same rating for all movies(i) and users(u) with all the differences explained 
#by random variation
mu_hat <- mean(train_set$rating)
mu_hat

# We get the following RMSE:
rmse_simple <- RMSE(test_set$rating, mu_hat)
rmse_simple

#### Movies effect

#In the data exploration we have seen that there is a movie effect. We are going to add this effect
#to our previous model
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

# We compute the predicted ratings and the RMSE:
predicted_ratings_movie <- mu_hat + test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

rmse_movie<-RMSE(predicted_ratings_movie, test_set$rating)
rmse_movie

#### Users effect

#In the data exploration we have seen that there is also a user effect. We are going to add 
#this effect to our previous model
user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

#We calculate the predictors and check the RMSE improvement:
predicted_ratings_user <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

rmse_user<-RMSE(predicted_ratings_user, test_set$rating)
rmse_user

#### Release year effect

#In the data exploration we have seen that there is also a release year effect. We are going 
#to add this effect to our previous model
year_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(release_year) %>%
  summarise(b_y = mean(rating - mu_hat - b_i - b_u))


#We calculate the predicted ratings taking into account the users, movies and year release effect.
predicted_ratings_year <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(year_avgs, by = "release_year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_y) %>%
  pull(pred)


#Finally we will calculate the RMSE based on release year model
rmse_year <- RMSE(predicted_ratings_year, test_set$rating)
rmse_year


#### Regularization

#The regularization is a technique that we use in order to limit the variability of the effect 
#sizes by penalizing large estimates that are formed from small sample sizes. The variability 
#can largely increase due to noisy estimates and hence, with the regularization technique, 
#we can prevent overfitting on the dataset. For this reason, we use the penalized least squares. 
#Basically, in our least squares equation, we add a penalty and then, we try to minimize the equation 
#with the penalty. The penalty λ is a tuning parameter that we can use cross-validation to compute it.

#We will use the regularization technique for the movie, users and release year effect. First of all, We 
#pick the best λ using cross_validation
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu_hat <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+l))
  
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  
  b_y <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(release_year) %>%
    summarise(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  
  
  ratings <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="release_year") %>%
    mutate(pred = mu_hat + b_i + b_u + b_y) %>%
    pull(pred)
  
  return(RMSE(ratings, train_set$rating))
})

#We get the values of the best λ below
best_lambda <- lambdas[which.min(rmses)]
best_lambda


#After tuning our parameter λ($\lambda$), we will use the best_lambda in order to calculate the predicted ratings
mu_hat <- mean(train_set$rating)

b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat)/(n()+best_lambda))

b_u <- train_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_hat)/(n()+best_lambda))

b_y <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(release_year) %>%
  summarise(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+best_lambda))


predicted_ratings_regularization <- test_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="release_year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_y) %>%
  pull(pred)

#Finally, we calculate the RMSE based on the regularization model
rmse_regularization <- RMSE(predicted_ratings_regularization, test_set$rating)
rmse_regularization

#### Recosystem package

# With this package, we build a recommender system using matrix factorization. 
#The basic idea of matrix factorization is that if we convert the data into a matrix 
#so that each user gets a row, each movie gets a column, and the rating is the entry 
#in row u and column i, then we have to find two matrices which describe the original 
#matrix. Basically, it decomposes the user-movie matrix into the product of two matrices of lower dimensions

#Τhe data format of the train_set should be in the form of sparse matrix triplet, 
#i.e., each line in the file contains three numbers: user_index, movie_index, rating. 
#Therefore, from our train_set, we will keep only the variables userId, movieId and 
#rating and we will convert this to a matrix.
train_set_reco <-  train_set %>%
  select(c("userId","movieId","rating"))

names(train_set_reco) <- c("user", "movie", "rating")

train_set_reco <- as.matrix(train_set_reco)

#The test_set has the same data format as the train_set. The only difference is that 
#the rating variable can be omitted since the ratings in testing data are usually unknown.
test_set_reco <-  test_set %>%
  select(c("userId","movieId","rating"))

names(test_set_reco) <- c("user", "movie", "rating")

test_set_reco <- as.matrix(test_set_reco)

#We use the data_memory() function in order to specify the source of the data in 
#the recommender system, which in this case, are  data in the memory as R objects. 
#We also put the argument index1 = TRUE because in our data the user and movie indices start with 1.
set.seed(123,sample.kind="Rounding")

train_set_reco1 <- data_memory(as.integer(train_set_reco[,1]),
                               as.integer(train_set_reco[,2]),
                               train_set_reco[,3],index1 = TRUE)
test_set_reco1 <- data_memory(as.integer(test_set_reco[,1]),
                              as.integer(test_set_reco[,2]),
                              rating=NULL, index1 = TRUE)

#Subsequently, we create a model object by calling Reco() and we call the tune() method
#that uses cross validation in order to select the best tuning parameters. 
r = Reco()

opts = r$tune(train_set_reco1, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                           costp_l1 = 0, costq_l1 = 0,
                                           nthread = 1, niter = 10))

#Following, we will use the train() method in order to train our recommender model. 
#We will set inside the function a number of parameters, coming from the result of tune().
r$train(train_set_reco1, opts = c(opts$min, nthread = 1, niter = 20))


#We now use the predict() method to compute the predicted values in our test_set_reco1
ratings_pred <- r$predict(test_set_reco1, out_memory())


#Finally, we get the RMSE value
rmse_recosystem <- RMSE(ratings_pred, test_set_reco[,3])
rmse_recosystem

## Results

#We will create a results table, taking into consideration all the above models
rmse_results <- tibble(method = c("Just the average","Movie effect", "Movie & User effects",
                                  "Movie, User $ Release year effects", 
                                  "Movie, User $ Release year effects with Regularization", 
                                  "Recosystem Package"),
                       RMSE = c(rmse_simple, rmse_movie, rmse_user, rmse_year, 
                                rmse_regularization, rmse_recosystem))

knitr::kable(rmse_results,digits=5)    


#The best RMSE value was achieved with the recosystem package, hence with using Matrix
#Factorization, which minimized the most the root mean square error. Therefore, we will 
#choose this method and we will check our model performance taking into consideration the 
#edx set and our final hold-out test set that means the validation set.
edx_reco <-  edx %>%
  select(c("userId","movieId","rating"))

names(edx_reco) <- c("user", "movie", "rating")

edx_reco <- as.matrix(edx_reco)


validation_reco <-  validation %>%
  select(c("userId","movieId","rating"))

names(validation_reco) <- c("user", "movie", "rating")

validation_reco <- as.matrix(validation_reco)


#We will continue using the data_memory() function.
set.seed(123,sample.kind="Rounding")

edx_reco1 <- data_memory(as.integer(edx_reco[,1]),
                         as.integer(edx_reco[,2]),
                         edx_reco[,3],index1 = TRUE)
validation_reco1 <- data_memory(as.integer(validation_reco[,1]),
                                as.integer(validation_reco[,2]),
                                rating=NULL, index1 = TRUE)

#Subsequently, we create a model object by calling Reco() and we call the tune() method. 
r = Reco()

opts = r$tune(edx_reco1, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))

#Following, we will use the train() method in order to train our recommender model. 
r$train(edx_reco1, opts = c(opts$min, nthread = 1, niter = 20))


#After training our model, we are going to use the predict() method to compute the predicted 
#values in our validation_reco1
ratings_pred_final <- r$predict(validation_reco1, out_memory())

#Finally, we get the RMSE value
rmse_final <- RMSE(ratings_pred_final, validation_reco[,3])
rmse_final
