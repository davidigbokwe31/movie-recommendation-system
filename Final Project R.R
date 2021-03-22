# installing relevant packages and libraries
install.packages("recommenderlab")  
install.packages("tidyverse")
install.packages("heatmaply")
library(tidyverse)
library(recommenderlab)
library(reshape2) 
library(ggplot2)                      
library(data.table)
library(arules)
library(dplyr)
library(Matrix)
library(stringr)
library(stringdist)
library(Matrix)
library(heatmaply)

movies <- read.csv("~/Downloads/movies.csv", stringsAsFactors=FALSE)
ratings <- read.csv("~/Downloads/ratings.csv", stringsAsFactors=FALSE)


#9742 films
dim(movies)
head(movies)
str(movies)

# check ratings data
dim(ratings)
head(ratings)
str(ratings)
summary(ratings)

#610 ratings
df <- ratings$userId
df_uniq <- unique(df)
length(df_uniq)

#extract year column. plot to understand visually and inspect
movies$year = as.numeric(str_sub( str_trim(movies$title) ,start = -5,end = -2))
ggplot(movies, aes(x=year)) + geom_bar() + ggtitle("Year Each Movie Released")

# userID and movie ID should be changed to factors from integers as these are variables that have a fixed and known set of values
# Genre of movies is also in a difficult format, need to split too
 
movie.genre <- as.data.frame(movies$genres,stringsAsFactors = FALSE)

# 10 genres
head(movie.genre)
dim(movie.genre)

#split genres with | use tstrsplit from reshape2 library
#?tstrsplit
movie.genre.new <- as.data.frame(tstrsplit(movie.genre[,1], '[|]', 
                                           type.convert=TRUE), 
                                 stringsAsFactors=FALSE)
#6x10 data frame so 10 columns
head(movie.genre.new)
dim(movie.genre.new)

# cleaning up 
colnames(movie.genre.new) <- c(1:10)
head(movie.genre.new)

#create frequency table. Better for understanding the data. 34 movies have unlisted genres
table(movie.genre.new$'1')
genre.list <- c("Action", "Adventure", "Animation", "Children", "Comedy", 
                "Crime","Documentary", "Drama", "Fantasy", "Film-Noir", 
                "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", 
                "War", "Western")


#genre matrix based on movie.genre.new columns replace column titles
#?matrix
genre.matrix <- matrix(0, nrow=9743, ncol=18)
head(genre.matrix)

#makes the first row genre list titles
genre.matrix[1,] <- genre.list
#set genre.list to column names
colnames(genre.matrix) <- genre.list
head(genre.matrix)

#?which
for (i in 1:nrow(movie.genre.new)) {
  for (col in 1:ncol(movie.genre.new)) {
    genre.column <- which(genre.matrix[1,] == movie.genre.new[i,col]) 
    genre.matrix[i+1,genre.column] <- 1
  }
}
head(genre.matrix)

#remove first row, which was the genre list
genre.matrix.new <- as.data.frame(genre.matrix[-1,],stringsAsFactors = F)
#shows that each movie has a row and displays that the first movie is Adventure, Animation, Children, Comedy and Fantasy
head(genre.matrix.new)

#create search matrix
#?bind_cols() from dplyr giving the appropriate title to the right row
search.matrix <- bind_cols(movies[,1:2], genre.matrix.new[])
head(search.matrix)


#create ratings matrix. rows = userid, columns = movieid. 610 rows for users 
#uses ratings table to have user id in rows, movieid in columns with values of corresponding rating value
#?matrix
#?dcast
#?cbind
rating.matrix <- dcast(ratings, userId~movieId, value.var = "rating", 
                       na.rm=FALSE)
#remove user ids
rating.matrix.new <- as.matrix(rating.matrix[,-1])

#610x9724 explore data
dim(rating.matrix.new)


#change rating matrix to a sparse matrix with recommenderlab
rating.sparse <- as(rating.matrix.new, "realRatingMatrix")
rating.sparse

#convert NAs into 0s - UPDATE doing this caused a lot of errors, leave NAs for now
#rating.matrix.new[is.na(rating.matrix.new)] = 0






#APPENDIX






#more EDA

ratings.vector <- as.vector(rating.sparse@data)
count.rating.value <- table(ratings.vector) 
count.rating.value

#Count of each rating score plot
#for better reference as count for 0 scaled plot badly
#plot(table(ratings.vector),main="Count of each Rating",
#     xlab="Count", ylab="Rating",ylim = c(0, 100000))

#Rating distribution plot
#better model than previous attempt above
#omitted rating=0 to give much better scaling
ratings.vector <- ratings.vector[ratings.vector != 0] 
ratings.vector <- factor(ratings.vector)
#model
plot(ratings.vector, main="Ratings Distribution", xlab="Rating Score", 
     ylab="Count")

# Same as above but with more significant amount of rating submissions
views.movie <- colCounts(rating.sparse) 
views.movie
average.views.movie <- colMeans(rating.sparse)

#significant due to more submissions therefore better average and not bias
significant.ratings <- average.views.movie[views.movie > 50] 
qplot(average.views.movie, xlab = 'Average View Count',
      ylab = 'Rating Score',) + 
  stat_bin(binwidth = 0.2) +
  ggtitle(paste("More Significant Ratings Distribution"))


#count amount of times movie has been rated in excel
#tried to insert data worked on excel into data.frames for better EDA
#write.csv(rating.matrix.new, 
#          "~/Desktop/ratings.matrix.csv", row.names=FALSE)
#library(readxl)
#views.movie <- read_excel("Desktop/views.movie.xlsx")
#View(views.movie)


#plot(movies$title ~ views.movie, main= "Movies vs Views")







#BACK TO PROBLEM







#normalise data as discussed. This subdues any high or low outliers
norm.ratings <- normalize(rating.sparse)
norm.ratings
sum(rowMeans(norm.ratings) > 0.00001)



#BUILD MODEL



#having previously looked into the more significant data, we should stick with it.
significant.count.row <- rowCounts(rating.sparse) > 50
significant.count.col <- colCounts(rating.sparse) > 50


significant.ratings <- rating.sparse[significant.count.row,
                                     significant.count.col]
significant.ratings

#create train and test data set with 80:20 split
sample.data <- sample(x = c(TRUE, FALSE),
                      size = nrow(significant.ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))

train <- significant.ratings[sample.data, ]
test <- significant.ratings[!sample.data, ]


#Find out what the parameter settings are for each recommender system types
#?recommenderRegistry
model <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
names(model)

#item-based method=cosine, normalize='center', normalize_sim_matrix = FALSE,alpha = 0.5, na_as_zero = FALSE
model$IBCF_realRatingMatrix$parameters

?Recommender
rec.model <- Recommender(data = train,
                         method = "IBCF", param = list(k =30,
                                                       method = "Cosine", normalize = "center", 
                                                       normalize_sim_matrix = FALSE, alpha = 0.5, 
                                                       na_as_zero = FALSE))
rec.model

#find top 10 recs 
predicted <- predict(object = rec.model, 
                     newdata = test, 
                     n = 10)
predicted

#user 1 recs
user.1.recs <- predicted@items[[1]]

#convert  object to  a list then run 
user.1.recs <- as(final.model, "list") 
user.1.recs

# use these movie ids to find the names
rec.results.final2 <- matrix(0, 10)

for (i in c(1:10)) {
  rec.results.final2[i] <- as.integer(user.1.recs[[1]][i])
}

rec.results.final <- as.data.frame(movies[rec.results.final, 2])
colnames(rec.results.final) <- list("Top 10 Movies")
rec.results.final



#evaluation of model 
evaluation <- evaluationScheme(rating.sparse, method="cross-validation", 
                               train=0.9, k=10, given=15, goodRating=5)

eval.results <- evaluate(evaluation, method = "IBCF", n=c(1,3,5,10,20))

eval.results.new <- getConfusionMatrix(eval.results) [[1]]
eval.results.new









