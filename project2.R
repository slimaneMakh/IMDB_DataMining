
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(plotly)


movie_metadata <- read.csv("~/Documents/Cours/M1/S2/Data Mining/project/movie_metadata.csv",stringsAsFactors = FALSE,header = T)
summary(movie_metadata)
head(movie_metadata)



library (Hmisc)
describe (movie_metadata)

#Removing duplicated movies
Movie.Data <- movie_metadata[!duplicated(movie_metadata$movie_title),]

Movie.Data$gross=Movie.Data$gross/1000000
Movie.Data$budget=Movie.Data$budget/1000000

#Computing the profit and return on investment
Movie.Data <- Movie.Data %>% mutate(profit = gross - budget, return_on_investment_perc = (profit/budget)*100)

#Selecting Recent movies
RecentMovies.Data = subset(Movie.Data, title_year > 2000)

test=RecentMovies.Data[RecentMovies.Data$budget < 1000000,]

plot (test$budget, test$profit)

#Correlation between budget and imdb score
cor.test(RecentMovies.Data$profit,RecentMovies.Data$budget)

Movie.Data.profit.20 <- Movie.Data %>% 
  arrange(desc(profit)) %>% 
  top_n(10, profit)

if (dev.interactive()) dev.new()
ggplot(Movie.Data.profit.20, aes(x=budget, y=profit))+ geom_point(size = 1)+ xlab("Budget $million") + ylab("Profit $million") + ggtitle("20 Most Profitable Movies")+geom_label_repel(aes(label=Movie.Data.profit.20$movie_title,fill = factor(Movie.Data.profit.20$imdb_score)), colour = "white", fontface = "bold")



Movie.Data.directors.20 <- Movie.Data %>% 
  group_by(director_name) %>%
  select(director_name, budget, gross, profit,imdb_score ) %>%
  na.omit() %>% 
  summarise(films = n(), budget = sum(as.numeric(budget)), gross = sum(as.numeric(gross)), profit = sum(as.numeric(profit)),imdb_score=sum(as.numeric(imdb_score))/films) %>% 
  arrange(desc(imdb_score)) %>% 
  top_n(20, profit)



#Plot
ggplot(Movie.Data.directors.20, aes(x=films, y=profit)) + geom_point(size = 1) + geom_label_repel(aes(label = director_name,fill=imdb_score), size = 4,fontface = 'bold', color = 'grey',
                                                                                                         box.padding = unit(0.35, "lines"),
                                                                                                point.padding = unit(0.5, "lines"),
                                                                                                      segment.color = 'grey50') + xlab("Number of Films") + ylab("Profit $millions") + ggtitle("Most Profitable Directors")



ggplot (Movie.Data.profit.20,
        aes(x=Movie.Data.profit.20$cast_total_facebook_likes, 
            y=Movie.Data.profit.20$imdb_score))+ geom_point(size = 1)+geom_label_repel(aes(label = Movie.Data.profit.20$movie_title))




movies0 <- Movie.Data[Movie.Data$genres != "", ]
genres <- c()
i <- 1
for (ins in movies0$genres){
  kw <- strsplit(ins, "[|]")
  if (length(kw) != 0){
    for (word in kw[[1]]){
      if (!(word %in% genres)){
        genres[i] <- word
        i = i + 1
      }
    }
  }
}

movies0$genres <- strsplit(movies0$genres, "[|]")
genres_idx <- movies0[, c("movie_title", "genres")]
i = 1
mat <- matrix(rep(0, (dim(movies0)[1] * length(genres))), nrow = dim(movies0)[1])
for (word in genres_idx$genres){
  idx <- which(genres %in% word)
  mat[i, idx] <- 1
  i = i + 1
}

colnames(mat) <- genres
movies_and_genres <- data.frame(mat)


sum <- rep(0, length(genres))
for (i in 1:length(genres)){
  sum[i] <- sum(movies_and_genres[, i])
}
genres_sum <- data.frame(genres = factor(genres), sum = sum)
genres_sum <- genres_sum[order(sum, decreasing = FALSE),]

genres_sum$genres <- factor(genres_sum$genres, levels = genres_sum$genres)



library(ggplot2)
library(ggthemes)
library(reshape2)
library(taRifx)

# Number of most popular genres
ggplot(genres_sum, aes(x = genres, y = sum, fill = genres)) + 
  geom_bar(stat = "identity", colour = "black") + 
  coord_flip() +
  labs(title = "Most popular genres", x = "", y = "") + 
  geom_text(aes(label = sum), hjust = -0.2, vjust = 0.4) + 
  theme_few() +
  theme(legend.position = "None") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())


movies_and_genres <- cbind(gross = movies0$gross, score = movies0$imdb_score, movies_and_genres, stringsAsFactors = FALSE)
movies_and_genres <- melt(movies_and_genres, id = c("gross", "score"))
movies_and_genres$variable <- gsub("[.]", " ", movies_and_genres$variable)
movies_and_genres <- movies_and_genres[movies_and_genres$value == 1, ] 
movies_and_genres$value <- NULL
colnames(movies_and_genres) <- c("gross", "score", "genres")
movies_and_genres$genres <- factor(movies_and_genres$genres, levels = genres_sum$genres)
movies_and_genres <- movies_and_genres[complete.cases(movies_and_genres), ]

ggplot(movies_and_genres, aes(genres, gross, fill = genres)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(title = "Gross revenue of movies", x = "", y = "") + 
  theme_bw() +
  theme(legend.position = "None") +
  theme(axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  scale_y_log10(breaks = c(5, 10, 40, 100,1500), 
                labels = c("5","10" , "40", "100","1500"), limits = c(1, 1500))


cdata
library(doBy)
cdata <- summaryBy(score+gross ~ genres, data=movies_and_genres, FUN=c(length,mean,sd))
cdata<-na.omit(cdata)


ggplot (cdata,
        aes(x=cdata$score.mean, 
            y=cdata$gross.mean))+ geom_point(size = 1)+geom_label_repel(aes(label =cdata$genres))


###CORRELATION PLOTTING

library(corrplot)

d=data.frame(Movie.Data$profit,Movie.Data$imdb_score,Movie.Data$actor_1_facebook_likes,Movie.Data$actor_2_facebook_likes,Movie.Data$actor_3_facebook_likes,Movie.Data$director_facebook_likes,Movie.Data$movie_facebook_likes,Movie.Data$num_critic_for_reviews,Movie.Data$num_voted_users,Movie.Data$budget,Movie.Data$gross)%>%na.omit()
M <- cor(d)
corrplot(M)


d[1, 9]
ggplot(d, aes(x = d[, 2], y = d[, 9])) + 
  geom_point(color = "blue") + 
  labs(x = names(d)[2], y = names(d)[9]) + 
  stat_smooth(method = lm, se = F, color = "red") +
  theme_grey() + ggtitle(paste("cor:", M[11,9])) +
  geom_smooth(color = "black")



###Prediction

 #Taking only numerical values
columns <- c()
for(i in 1:dim(Movie.Data)[2])
{
  if(is.numeric(Movie.Data[,i])|| is.integer(Movie.Data[,i]))
  {
    columns[i]=T
  }
  else
  {
    columns[i]=F
  }
}
temp <- na.omit(Movie.Data[,columns])
temp
ggplot(temp, aes(x=imdb_score)) + geom_histogram(bins = 100,binwidth = 0.1)+ggtitle("General Distribution of scores")

#Getting train and test samples
train <- sample(dim(temp)[1],dim(temp)[1]*0.7)
temp_train <- temp[train,]
temp_test <- temp[-train,]

require('randomForest')
#Getting the best value for mtry
bestmtry <- tuneRF(temp_train[-14],temp_train$imdb_score, ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

mtry<-which.min(bestmtry)


#Training random Forest 
set.seed(5)
library(randomForest)
rf <- randomForest(imdb_score~.,data=temp[train,],ntree=500,mtry=mtry)
pred_rf1 <- predict(rf,temp[-train,])
mean((pred_rf1-temp[-train,]$imdb_score)^2)



array_ntree<- c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,3000,5000,7000,10000)
mse <- c()
j<-1
for(i in array_ntree)
{ set.seed(5)
  rf <- randomForest(imdb_score~.,data=temp[train,],ntree=i,mtry=floor(dim(temp)[2]/3))
  pred_rf <- predict(rf,temp[-train,])
  mse[j]<-mean((pred_rf-temp[-train,]$imdb_score)^2)
  j=j+1
  
}
mse
save(mse,file="RandomForestResults.Rda")
min(mse)
data_mse <- data.frame(array_ntree,mse)
ggplot(data_mse, aes(x=(array_ntree), y=mse)) + geom_line() + geom_point()
data_mse$array_ntree[data_mse$mse==min(data_mse$mse)]

rf <- randomForest(imdb_score~.,data=temp[train,],ntree=7000,mtry=mtry)

pred_rf <- predict(rf,temp[-train,])
mean((pred_rf-temp[-train,]$imdb_score)^2)
