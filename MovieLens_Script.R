###########################################################
###########################################################
##                                                       ##   
## title: "Movielens Recommendation Project"             ##
## subtitle: HarvardX PH125.9x Capstone                  ## 
## author: "Octavio Rodríguez Ferreira"                  ##
## date: May 04, 2021                                    ##
##                                                       ##
###########################################################
###########################################################


##########################################################
#                                                        #
#          1. LOADING DATA AND PACKAGES                  #
#                                                        #
##########################################################


#########################################################
##############     INSTALLING PACKAGES     ##############
#########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

#########################################################
##############      LOADING LIBRARIES      ##############
#########################################################

library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(lubridate)
library(recosystem)
library(GGally)
library(ggplot2)
library(gridExtra)
library(tinytex)

#########################################################
##############     DOWNLOADING DATA SET    ##############
#########################################################

#Downloading the data set
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#Loading "ratings" dataset and adding column names into a new element called "ratings"
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

#Loading "movies" dataset and adding column names into a new element called "movies"
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

#Converting strings into numeric data in the "movies" object in a dataframe (R 4.0 and later)
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

#Join "ratings" data into a new "movielens" dataframe
movielens <- left_join(ratings, movies, by = "movieId")

#Remove  temporary elements
rm(dl, ratings, movies) 

##########################################################
#                                                        #
#      2. CREATING TRAINING AND VALIDATION SETS          #
#                                                        #
##########################################################

#########################################################
#### Initial exploration of the `movielens data set` ####
#########################################################

#Dimensions
dim(movielens)

#Variable classes
knitr::kable(sapply(lapply(movielens, class), "[", 1), col.names = c("Class"))

#############################################
########## EDX AND VALIDATION SETS ##########
#############################################

# Creating test and validation sets for MovieLens data
set.seed(1982, sample.kind="Rounding")

#Creating a test index
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

#Test set
edx <- movielens[-test_index,]

#Validation set of 10% of MovieLens data
temp <- movielens[test_index,]

# Making sure userId and movieId in validation set are also in edx set. 
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

##To make sure we don’t include users and movies in the test set that do not appear in the training set, we remove these entries using the semi_join function:

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#Remove all temporary elements that won't be needed in the analysis and keep the test and validation sets
rm(test_index, temp, removed)


##########################################################
#                                                        #
#                   3. DATA EXPLORATION                  #
#                                                        #
##########################################################


#Explore training and test sets. Check only the first observations
head(edx)
head(validation)

#Number of rows and columns and structure of the train and test sets
dim(edx)
dim(validation)
knitr::kable(sapply(lapply(edx, class), "[", 1), col.names = c("Class"))
knitr::kable(sapply(lapply(validation, class), "[", 1), col.names = c("Class"))

############################################################
########## 1. USERS AND MOVIES (userId / movieId) ##########
############################################################

#Explore the number of unique users rated a movie and how many unique movies were rated
knitr::kable (edx %>% 
  summarise("Unique users" = n_distinct(userId),
            "Unique movies" = n_distinct(movieId)))

#Top rated movies (10)
knitr::kable (edx %>% 
                group_by(title) %>%
                summarise(title = title[1],
                          rating = mean(rating),
                          n_ratings = n()) %>%
                slice_max(rating, n=10) %>%
                arrange(desc(rating)))
  
#Worst rated movies (10)
knitr::kable (edx %>% 
                group_by(title) %>%
                summarise(title = title[1],
                          rating = mean(rating),
                          n_ratings = n()) %>%
                slice_min(rating, n = 10) %>%
                arrange(desc(rating)))

#Most rated movies (10)
knitr::kable (edx %>% 
                group_by(title) %>%
                select(rating) %>%
                summarise(title = title[1],
                          n_ratings = n(),
                          rating = mean(rating)) %>%
                slice_max(n_ratings, n=10) %>%
                arrange(desc(n_ratings)))

#############################
########## RATINGS ########## 
#############################

#List of all different ratings given by "n" users.
knitr::kable(edx %>% group_by(Rating = rating) %>%
  summarise(Total_ratings = n()) %>%
  arrange(desc(Total_ratings)))

#Mean rating
mean(edx$rating)

#Ratings distribution histogram
hstgm <- edx %>% 
  ggplot(aes(rating)) +
  xlab("Rating") +
  ylab("Number of ratings") + 
  geom_histogram(bins = 10, binwidth = 0.5, color = "gray") + 
  ggtitle("Rating Distribution") +
  theme_light()

hstgm

#Ratings per user density plot
dnst1 <- edx %>%
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_density(fill = "gray") + 
  scale_x_log10() + 
  xlab("Ratings") +
  ylab("Users") +
  ggtitle("Ratings per user") +
  theme_light()

dnst1

#Ratings per movie density plot
dnst2 <- edx %>%
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_density(fill = "gray") + 
  scale_x_log10() + 
  xlab("Movies") +
  ylab("Ratings") +
  ggtitle("Ratings per movie")+
  theme_light()

dnst2

#Grid of distribution plots
grid.arrange(hstgm, dnst1, dnst2, nrow=1)

###############################
########## TIMESTAMP ########## 
###############################

#Create a new column with the date from the timestamp variable and an extra column with the year of rating. This function requires the "lubridate" package.
edx <- edx %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(rtg_year = as.integer(substr(date, 1, 4)))

#Because we are making changes to the test set that would result in differences with the validation set, we will also make the same change in "validation."
validation <- validation %>%
  mutate(date = as_datetime(timestamp)) %>%
  mutate(rtg_year = as.integer(substr(date, 1, 4)))

#We created a column of the exact date and time and the rating and an extra column just for the year of rating.
knitr::kable(edx %>% 
  group_by("Rating year" = rtg_year)  %>%
    count(movieId) %>%
    summarise ("Movies rated" = n()) %>%
  arrange(desc("Rating year")))

#Plot of movies rated by year
edx %>% group_by(movieId) %>%
  summarise(n = n(), year = as.character(first(rtg_year))) %>%
  ggplot(aes(year, n)) +
  geom_boxplot(alpha = 0.5) +
  coord_trans(y = "sqrt") + 
  ggtitle("Movies rated by year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###########################
########## TITLE ########## 
###########################

#Extract title and year from the current column to create two different values. 
#Some movies include parenthesis in the titles. Find the correct pattern of regular expressions to only extract the year and not a part of the actual title.
#The criteria for separation is a pattern that finds only the years and not any other information that is in parenthesis. Mutate the year column as integers.
edx <-  edx %>% 
  mutate(title = str_trim(title)) %>%
  extract(title, c("title", "year"), "^(.*) \\(([0-9]*)\\)$") %>% 
  mutate(year = as.integer(year))

#Because we are making changes to the test set that would result in differences with the validation set, we will also make the same change in "validation."
validation <-  validation %>% 
  mutate(title = str_trim(title)) %>%
  extract(title, c("title", "year"), "^(.*) \\(([0-9]*)\\)$") %>% 
  mutate(year = as.integer(year))

##We separated the title column into 2 variables, title and year of release.

#Plot of movies released by year
edx %>% group_by(movieId) %>%
  summarise(n = n(), year = as.character(first(year))) %>%
  ggplot(aes(year, n)) +
  geom_point(color = "black", alpha = 0.3) +
  coord_trans(y = "sqrt") + 
  ggtitle("Movies released by year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#Mean rating per year of release
edx %>% 
  group_by(year)  %>%
  summarise("Mean_rating" = mean(rating)) %>%
  ggplot(aes(year, Mean_rating)) +
  geom_point(color = "gray") +
  geom_smooth(color = "black") + #geom_smooth()` using method = 'loess' and formula 'y ~ x'
  xlab("Year") +
  ylab("Mean rating") +
  ggtitle("Mean ratings per movie's year of release")+
  theme_light()

############################
########## GENRES ########## 
############################

#Separate into unique values of genre per observation using the character "|" as the separator in the general expression pattern. 
edx <- edx %>% 
  mutate(genres = str_trim(genres)) %>%
  separate_rows(genres, sep = "\\|")

#Because we are making changes to the test set that would result in differences with the validation set, we will also make the same change in "validation."
validation <- validation %>% 
  mutate(genres = str_trim(genres)) %>%
  separate_rows(genres, sep = "\\|")

#Movies per genre
knitr::kable(edx %>% 
  group_by(Genres = genres) %>%
  summarise(Movies_per_genre = n()) %>%
  arrange(desc(Movies_per_genre)))

#Mean average by genre
knitr::kable(edx %>% 
               group_by(Genres = genres) %>%
               summarise(Mean_rating = mean(rating)) %>%
               arrange(desc(Mean_rating)))

#Mean rating per year per genre
edx %>% 
  group_by(rtg_year, genres)  %>%
  summarise("Mean_rating" = mean(rating)) %>%
  ggplot(aes(rtg_year, Mean_rating)) +
  geom_point(aplha = 0.03, color = "gray") +
  geom_smooth(color = "black") + #`geom_smooth()` using method = 'loess' and formula 'y ~ x'
  facet_wrap(~ genres) +
  xlab("Year") +
  ylab("Mean rating") +
  ggtitle("Mean ratings per year of rating per genre")+
  theme_light()

#New dimensions and classes of the train set.
dim(edx)
#[1] 23368968        9
dim(validation)
#[1] 2598226       9
knitr::kable(sapply(lapply(edx, class), "[", 1), col.names = c("Class"))

##########################################################
#                                                        #
#                      4. MODELING                       #
#                                                        #
##########################################################

#Here we opened a segment to build our algorithms. While this is not part of the  code that will be incorporated to the final report, we leave it here to document the different attempts made in building the final algorithms.

###################################################
########## ADDITIONAL PARTITION OF DATA  ########## 
###################################################

# Creating test and validation sets for MovieLens data
set.seed(1982, sample.kind="Rounding")

#Creating a test index
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)

#Train set
train_set <- edx[-test_index,]

#Test set
temp <- edx[test_index,]

# Making sure userId and movieId in validation set are in both sets
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed back
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#Remove all temporary elements
rm(test_index, temp, removed)

###################################
########## RMSE FUNCTION ########## 
###################################

#Creating the function to calculate the residual mean squared error of our model.

#Formula code for knitr: $$ RMSE = \sqrt{\frac{1}{N} \sum_{u,i}^{} \left( \hat{y}_{u,i} - y_{u,i} \right)^2 } $$

# Creating the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#################################
########## NAIVE MODEL ##########
#################################

#Following the exercise part of the HarvardX Data Science Professional Certificate Program in the PH125.8x Machine Learning module my first approach is to start with a "naive model" which is a simple prediction based on the average ratings. As a clarification, this is not related to a Naïve Bayes analysis.

#Formula code for knitr: $$ Y_{u,i} = \mu + \varepsilon_{u,i} $$

# Simplest possible prediction is the ratings mean
mu <- mean(train_set$rating)
mu

#> [1] 3.527

# Computing the predicted ratings on the train set. This model only takes the average rating.
naive_rmse <- RMSE(test_set$rating, mu)
naive_rmse

#> [1] 1.052

# We store our results in a data frame to be able to compare the different results of our models. The first result is just the average. 

#Results table
naive_res <- tibble(method = "Naive Model", RMSE = naive_rmse)
naive_res

#> Naive Model  1.05

#####################################
########## BASELINE MODELS ########## 
#####################################


#Once more, following the previous exercise on PH125.8x Machine Learning module, our next step is to calculate the "Movie Effect." # This initial approach is the same used in the Belkor Solution to the Netflix Prize challenge  <https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf>


#Formula code for knitr: $$ Y_{u,i} = \mu + b_i + \varepsilon_{u,i} $$

#Calculate the "Movie Effect." This model predicts the rating based on the movie averages.

##### MOVIE EFFECT ######

#Movie averages
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

#Predict the influence of the "Movie Effect" in the rating.
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

#We validate our prediction the test set.
rmse_bi <- RMSE(predicted_ratings, test_set$rating)

#And include the results in a table of RMSEs results
bl_rmse_res <- tibble(method="Movie",
                                 RMSE = rmse_bi)

#Results table
bl_rmse_res %>% knitr::kable()

#|method |      RMSE|
#|:------|---------:|
#|Movie  | 0.9414214|

##### MOVIE+USER EFFECT ##### 

#Once more, following the previous exercise on PH125.8x Machine Learning module, our next step is to calculate the "User Effect." 

#Formula code for knitr: $$ Y_{u,i} = \mu + b_i + b_u \varepsilon_{u,i} $$

#User averages
user_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

#We predict the influence of the "User Effect" in the rating.
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#We validate our prediction the test set.
rmse_biu <- RMSE(predicted_ratings, test_set$rating)

#And include the results in a table of RMSEs results
bl_rmse_res <- bind_rows(bl_rmse_res, 
                          tibble(method="Movie+User",
                                 RMSE = rmse_biu))

#Results table
bl_rmse_res %>% knitr::kable()

#|method     |      RMSE|
#|:----------|---------:|
#|Movie      | 0.9414214|
#|Movie+User | 0.8589388|


##### MOVIE+USER+GENRE EFFECT ##### 

#Our next step is to try to see if there is an improvement in the prediction also adding the "Genre Effect." 

#It appears logical to assume that an user who gives a rating to one movie would rate the same way other movies of the same genre.

#Formula code for knitr: $$ Y_{u,i} = \mu + b_i + b_u + b_g\varepsilon_{u,i} $$

## Genre averages
genre_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu - b_i - b_u))

#We predict the influence of the "Genre Effect" in the rating along with our other effects.
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

#We validate our prediction the test set.
rmse_biug <- RMSE(predicted_ratings, test_set$rating)

#And include the results in a table of RMSEs results
bl_rmse_res <- bind_rows(bl_rmse_res, 
                          tibble(method="Movie+User+Genre",
                                 RMSE = rmse_biug))

#Results table
bl_rmse_res %>% knitr::kable()

#|method           |      RMSE|
#|:----------------|---------:|
#|Movie            | 0.9414214|
#|Movie+User       | 0.8589388|
#|Movie+User+Genre | 0.8588486|


##### MOVIE+USER+GENRE+YEAR EFFECT ##### 

#Our next step is to try to see if there is an improvement in the prediction when adding the year of release or "Year Effect." 

#Our first assumption is that people would like movies that were released in similar years. That is to say, a person tha liked a classic film, might also like other classic films. This doesn't seem to be, however, a compelling enough hypothesis, but we wanted to test it, to see if it improves the overall accuracy of the algorithm.

#Formula code for knitr: $$ Y_{u,i} = \mu + b_i + b_u + b_g + b_y\varepsilon_{u,i} $$

## Year averages
year_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(year) %>%
  summarise(b_y = mean(rating - mu - b_i - b_u - b_g))

#We predict the influence of the "Year Effect" in the rating along with our other effects.
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  .$pred

#We validate our prediction the test set.
rmse_biugy <- RMSE(predicted_ratings, test_set$rating)

#And include the results in a table of RMSEs results
bl_rmse_res <- bind_rows(bl_rmse_res, 
                          tibble(method="Movie+User+Genre+Year",
                                 RMSE = rmse_biugy))

#Results table
bl_rmse_res %>% knitr::kable()

#|method                |      RMSE|
#|:---------------------|---------:|
#|Movie                 | 0.9414214|
#|Movie+User            | 0.8589388|
#|Movie+User+Genre      | 0.8588486|
#|Movie+User+Genre+Year | 0.8585346|

#While every model showed an improvement, after calculating the "User effect" the improvements were somewhat marginal.

#One thing we know, is that but our increase margin is getting smaller and the processing time is increasing. 


##########################################
########## REGULARIZED BL MODELS########## 
##########################################

#One thing we have to be aware of is about over-training. To avoid this we have to penalize the estimates to constrain the total variability. We avoid over-training with Regularization.

#Formula code for knitr: $$ \frac{1}{N}\sum_{u,i} \left(y_{u,i} - \mu - b_i\right)^2 + \lambda \sum_{i} b_i^2  $$


##### REGULARIZED MOVIE EFFECT ##### 

#We define our values of lambda
lambdas <- seq(0, 10, 0.1)

#The code we used for the regularized model follows the same steps as previously shown, but makes all steps at ones considering different values of lambda and pulls different predictions that are validated in the test set.

#Regularized Movie
rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

#Now we include this value of lambda in our model.
rmse_reg_bi <- min(rmses)

#And include the results in a table of RMSEs results
regbi_rmse_res <- tibble(method="Regularized Movie",
                                 RMSE = rmse_reg_bi)

#Results table
regbi_rmse_res %>% knitr::kable()


#|method            |      RMSE|
#|:-----------------|---------:|
#|Regularized Movie | 0.9413859|

#Best value of lambda
lambdas[which.min(rmses)]
qplot(lambdas, rmses)

##### REGULARIZED MOVIE+USER EFFECT ##### 

#Regularized Movie+User

rmses <- sapply(lambdas, function(l) {
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + l))
  b_u <- train_set %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu) / (n() + l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred  
  return(RMSE(predicted_ratings, test_set$rating))
})

#In order to identify which lambda works best for our model we plot all the values
qplot(lambdas, rmses)

#Best value of lambda
lambda <- lambdas[which.min(rmses)]
lambda

#Include the lowest value of lambda in our model.
rmse_reg_biu <- min(rmses)

#And include the results in a table of RMSEs results
regbi_rmse_res <- bind_rows(regbi_rmse_res, 
                          tibble(method="Regularized Movie+User",
                                 RMSE = rmse_reg_biu))

#Results table
regbi_rmse_res %>% knitr::kable()


#|method                 |      RMSE|
#|:----------------------|---------:|
#|Regularized Movie      | 0.9413859|
#|Regularized Movie+User | 0.8586647|

#Too much processing time, not worth adding more predictors.

##########################################################
#                                                        #
#            MATRIX FACTORIZATION MODEL                  #
#                                                        #
##########################################################

######################################
############# RECOSYSTEM #############
######################################

## Accordiing to Qiu <https://cran.r-project.org/web/packages/recosystem/vignettes/introduction.html>, 

#Chin, et al. (2016) a "popular technique to solve the recommender system problem is the matrix factorization method." Chin introduces the `recosystem` package, "an R wrapper of the LIBMF library developed by Yu-Chin Juan, Wei-Sheng Chin, Yong Zhuang, Bo-Wen Yuan, Meng-Yuan Yang, and Chih-Jen Lin" (Chin, et al. 2016). According to Chin, "LIBMF (and hence recosystem) can significantly reduce memory use, for instance the constructed model that contains information for prediction can be stored in the hard disk, and output result can also be directly written into a file rather than be kept in memory." Also, according to Chin, `recosystem` provides "convenient functions"  to solve "the matrices P and Q" and "additionally have functions for model exporting (outputing P and Q matrices) and prediction."

#The basic idea consists in "approximate the whole rating matrix Rm×n by the product of two matrices of lower dimensions, Pk×m and Qk×n such that R≈P′Q" with "pu be the u-th column of P, and qv be the v-th column of Q, then the rating given by user u on item v would be predicted as p′uqv." Here we aplply the solution to this optimization problem given by (Chin, Zhuang, et al. 2015a, 2015b): minP,Q∑(u,v)∈R[f(pu,qv;ru,v)+μP||pu||1+μQ||qv||1+λP2||pu||22+λQ2||qv||22], where "(u,v) are locations of observed entries in R, ru,v is the observed rating, f is the loss function, and μP,μQ,λP,λQ are penalty parameters to avoid overfitting."


#Set seed
set.seed(1982, sample.kind = "Rounding")

#Create the sets for the `recosystem` matrix factorization analysis. <https://cran.r-project.org/web/packages/recosystem/recosystem.pdf>: 

#`recosystem` requires a user_index, an item_index, and the rating.`user_index` is an integer vector giving the user indices of rating scores, `item_index` is an integer vector giving the item(movieId) indices of rating scores, and rating` is a numeric vector of the observed entries in the rating matrix. If the `user index`,  `item index`,  and `ratings` are stored as R vectors in memory, they can be passed  via function `data_memory()`.


#Create a special train set for the matrix factorization analysis
train_rec_mf <-  with(train_set, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating     = rating))

#Here we used the with() function, that "allows an expression to be evaluated with variable lookup in a specified data frame, and then the calling environment" <https://developer.r-project.org/nonstandard-eval.pdf>.

#Create a special test set for the matrix factorization analysis
test_rec_mf  <-  with(test_set,  data_memory(user_index = userId, 
                                             item_index = movieId, 
                                             rating     = rating))

#Create a model object (a Reference Class object in R) by calling Reco().
r <- recosystem::Reco()


#Call the $tune() method to select best tuning parameters along a set of candidate values.
opts <- r$tune(train_rec_mf, opts = list(dim = 30, 
                                         lrate = c(0.1, 0.2),
                                         costp_l2 = 0.1, 
                                         costq_l2 = 0.1,
                                         nthread  = 4, 
                                         niter = 10))

#Train the model by calling the $train() method. A number of parameters can be set inside the function, possibly coming from the result of $tune()
r$train(train_rec_mf, opts = c(opts$min, nthread = 4, niter = 30))

#Predict the rating of the matrix factorization analysis.
predicted_ratings <-  r$predict(test_rec_mf, out_memory())

#Validate prediction on the test set.
rmse_reco_mf <- RMSE(predicted_ratings, test_set$rating)

#Add results to table
reco_rmse_res <- tibble(method= "Recosystem Matrix Factorization",
                        RMSE = rmse_reco_mf)

#Table results
knitr::kable(reco_rmse_res)

#|method                          |      RMSE|
#|:-------------------------------|---------:|
#|Recosystem Matrix Factorization | 0.7893919|

##########################################################
#                                                        #
#                          ANALYSIS                      #
#                                                        #
##########################################################

#################################
########## NAIVE MODEL ##########
#################################

#Formula code for knitr: $$ Y_{u,i} = \mu + \varepsilon_{u,i} $$

# Simplest possible prediction is the ratings mean
mu <- mean(edx$rating)
mu

# Computing the predicted ratings on the train set. This model only takes the average rating.
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

# We store our results in a data frame to be able to compare the different results of our models.
rmse_results <- tibble(method = "Naive Model", RMSE = naive_rmse)
rmse_results

#> Naive Model  1.05

#####################################
########## BASELINE MODELS ########## 
#####################################


##### MOVIE+USER EFFECT ##### 

#Once more, following the previous exercise on PH125.8x Machine Learning module, our next step is to calculate the "User Effect." 

#User averages
user_avgs <- edx %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

#We predict the influence of the "User Effect" in the rating.
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#We validate our prediction the test set.
rmse_biu <- RMSE(predicted_ratings, validation$rating)

#And include the results in a table of RMSEs results
rmse_results <- bind_rows(rmse_results, 
                         tibble(method="Baseline Movie+User",
                                RMSE = rmse_biu))

#rmse_results
rmse_results %>% knitr::kable()

#|method              |      RMSE|
#|:-------------------|---------:|
#|Naive Model         | 1.0519086|
#|Baseline Movie+User | 0.8638741|

##########################################
########## REGULARIZED BL MODELS########## 
##########################################


#Set seed
set.seed(1982, sample.kind = "Rounding")

#During my test I found the best lambda to be over 10, therefore I set my lambdas to be between 10 and 20.
#We define our values of lambda. 
lambdas <- seq(10, 20, 0.1)

#Regularized Movie+User
final_rmses <- sapply(lambdas, function(l) {
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + l))
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu) / (n() + l))
  predicted_ratings <- validation %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred  
  return(RMSE(predicted_ratings, validation$rating))
})

#Include the lowest value of lambda in our model.
rmse_reg_biu <- min(final_rmses)

#And include the results in a table of RMSEs results
rmse_results <- bind_rows(rmse_results, 
                            tibble(method="Regularized Movie+User",
                                   RMSE = rmse_reg_biu))

#Results
rmse_results %>% knitr::kable()

#|method                 |      RMSE|
#|:----------------------|---------:|
#|Naive Model            | 1.0519086|
#|Baseline Movie+User    | 0.8638741|
#|Regularized Movie+User | 0.8630461|

#Plot of lambdas
qplot(lambdas, final_rmses)

#Best value of lambda
lambda <- lambdas[which.min(final_rmses)]
lambda

######################################
############# RECOSYSTEM #############
######################################

#Set seed
set.seed(1982, sample.kind = "Rounding")

#Create a special train set for the matrix factorization analysis
train_rec_mf <-  with(edx, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating     = rating))

#Create a special test set for the matrix factorization analysis
test_rec_mf  <-  with(validation,  data_memory(user_index = userId, 
                                             item_index = movieId, 
                                             rating     = rating))

#Create a model object (a Reference Class object in R) by calling `Reco()`.
r <- recosystem::Reco()

#Call the $tune() method to select best tuning parameters along a set of candidate values.
opts <- r$tune(train_rec_mf, opts = list(dim = 30, 
                                         lrate = c(0.1, 0.2),
                                         costp_l2 = 0.1, 
                                         costq_l2 = 0.1,
                                         nthread  = 4, 
                                         niter = 10))

#Train the model by calling the $train() method.
r$train(train_rec_mf, opts = c(opts$min, nthread = 4, niter = 30))

#Predict the rating of the matrix factorization analysis.
predicted_ratings <-  r$predict(test_rec_mf, out_memory())

#Validate prediction on the test set.
rmse_reco_mf <- RMSE(predicted_ratings, validation$rating)

#Add to results table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Recosystem Matrix Factorization",
                                 RMSE = rmse_reco_mf ))

#Results
rmse_results %>% knitr::kable()

#|method                          |      RMSE|
#|:-------------------------------|---------:|
#|Naive Model                     | 1.0519086|
#|Baseline Movie+User             | 0.8638741|
#|Regularized Movie+User          | 0.8630461|
#|Recosystem Matrix Factorization | 0.8110025|

