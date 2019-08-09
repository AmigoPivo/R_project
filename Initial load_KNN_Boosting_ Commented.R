rm(list = ls())
setwd("C:/Users/pivo/Documents/GitHub/R_project")
library(readr)
library(kknn)
library(gbm)

set.seed(79643)
df_spotify <- read_csv("songDb.csv")
genres <- read_table("genre_dataset.txt", col_names = c('Genre'))


#Data cleaning
df_clean <- na.omit(df_spotify)
drop <- c('Uri','Name','Ref_Track', 'time_signature', 'Type', 'URL_features')
df_clean <- df_clean[, !(names(df_clean)%in%drop)]
df_clean <- df_clean[(df_clean$Duration_ms > 120000),]
dim(df_clean)

#We selected a couple of genres and used them to classify our data based on the content of the actual Genre column
df_genre <- df_clean
genres_aux = c('rock', 'pop', 'country','classical','hiphop','jazz','blues')
m_genre = matrix(data = NA, nrow = nrow(df_genre), ncol = 7)
colnames(m_genre) = genres_aux

for (x in genres_aux) {  #if the column Genre contains the genres we seleceted it will place a 1 in the relevant column
  df_genre[grepl(x,df_genre$Genre),x] <- 1
  df_genre[!grepl(x,df_genre$Genre),x] <- 0
}

df_genre2 <- df_genre[rowSums(df_genre[15:21])==1,]
for (x in genres_aux){ #We keep only the ones that are part of 1 column and use that genre
  df_genre2[df_genre2[x]==1, 'genre'] = x
}
df_genre2 <- df_genre2[,!(names(df_genre2) %in% genres_aux)]
df_genre2$genre = factor(df_genre2$genre)
df_genre2$Tempo = as.numeric(df_genre2$Tempo)
dim(df_genre2)
summary(df_genre2)

### Train - Test division
train = .7
train_sample = sample(1:nrow(df_genre2), nrow(df_genre2)*train)
train_df = df_genre2[train_sample,]
test_df = df_genre2[-train_sample,]

### Scale for KNN and other methods
scale_train_df <- train_df
scale_train_df[,-c(12,14,15)]<- scale(train_df[,-c(12,14,15)]) 
scale_test_df <- test_df
scale_test_df[,-c(12,14,15)] <- scale(test_df[,-c(12,14,15)])


### KNN ###
#General model using all the variables and cross validation to find K
model_knn <- train.kknn(genre~(.-ID-Mode-Genre), data=scale_train_df, kmax = 100, kcv = 10)
model_knn$best.parameters
final_model = kknn(genre~(.-ID-Mode-Genre),train_df,test_df,k= model_knn$best.parameters$k, kernel = "rectangular")
sum(diag(table(final_model$fitted.values, test_df$genre)))/length(final_model$fitted.values)

#Using only the most important variables we found and cross validation for K
model_knn2 <- train.kknn(genre~(Danceability + Energy + Loudness + Speechness+Acousticness+Instrumentalness+Valence+Liveness+Duration_ms), data=scale_train_df, kmax = 100, kcv = 10)
model_knn2$best.parameters
final_model2 = kknn(genre~(Danceability + Energy + Loudness + Speechness+Acousticness+Instrumentalness+Valence+Liveness+Duration_ms),train_df,test_df,k= model_knn$best.parameters$k, kernel = "rectangular")
sum(diag(table(final_model2$fitted.values, test_df$genre)))/length(final_model2$fitted.values)


### BOOSTING ###
# cross validation was left out of final code due to the time it took (we ran a nested for loops for n.trees and shrinkage)
ntrees=1000
boostfit = gbm(genre~.-ID-Genre-Key-Mode,data=train_df,distribution='multinomial', #Multinomial makes the tree take longer to run but it's the output we need
               interaction.depth=5,n.trees=ntrees,shrinkage=.01)
pred = predict(boostfit,newdata=test_df,n.trees=ntrees, type = 'response')
df_p = data.frame(pred)
colnames(df_p) = c('blues', 'classical','country','hiphop','jazz','pop','rock')
df_p['genre'] = colnames(df_p)[apply(df_p,1,which.max)] #gets the predicted genre out of the top probability
sum(diag(table(test_df$genre, df_p$genre)))/length(test_df$genre)

