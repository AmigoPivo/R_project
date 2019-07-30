rm(list = ls())

library(readr)

set.seed(79643)

df_spotify <- read_csv("MSBA/Summer/Predictive Modelling/Project/songDb.csv")
#genres <- read_table("genre_dataset.txt", col_names = c('Genre'))
#View(df_spotify)

df_clean <- na.omit(df_spotify)

drop <- c('Uri','Name','Ref_Track', 'time_signature', 'Type', 'URL_features')
df_clean <- df_clean[, !(names(df_clean)%in%drop)]
df_clean <- df_clean[(df_clean$Duration_ms > 120000),]
#View(df_clean)
dim(df_clean)

df_genre <- df_clean
genres_aux = c('rock', 'pop', 'country','classical','hiphop','jazz','blues')
m_genre = matrix(data = NA, nrow = nrow(df_genre), ncol = 7)
colnames(m_genre) = genres_aux

for (x in genres_aux) {
  df_genre[grepl(x,df_genre$Genre),x] <- 1
  df_genre[!grepl(x,df_genre$Genre),x] <- 0
}

df_genre2 <- df_genre[rowSums(df_genre[15:21])==1,]
for (x in genres_aux){
  df_genre2[df_genre2[x]==1, 'genre'] = x
}
df_genre2 <- df_genre2[,!(names(df_genre2) %in% genres_aux)]
df_genre2$genre = factor(df_genre2$genre)
df_genre2$Tempo = as.numeric(df_genre2$Tempo)
dim(df_genre2)
View(df_genre2)
summary(df_genre2)

train = .7

train_sample = sample(1:nrow(df_genre2), nrow(df_genre2)*train)
train_df = df_genre2[train_sample,]
test_df = df_genre2[-train_sample,]


library(dplyr)
library(randomForest) 



#--------------------------------------------------
#fit random forest and plot variable importance

# array of number of tree values to use
ntr<-c(50,200,500,2000,5000)

max_acc=0

# Training model with different number of trees and splits to get the optimal values for each
for (n in ntr){
  a=c()
  i=5
  for (i in 3:8) {
    model_rf <- randomForest(genre~.-ID-Genre, data = train_df, ntree = n, mtry = i, importance = TRUE)
    predValid <- predict(model_rf, test_df, type = "class")
    a[i-2] = mean(predValid == test_df$genre)
    if (a[i-2]>max_acc){
      max_acc=a[i-2]
      opt_tree=n
      opt_m=i
    }
    
  }
  print(paste0('Number of trees: ',n))
  print(a)
}


# training model with the optimal number of trees and splits
model_rf<-randomForest(genre~.-ID-Genre,data=train_df,ntree=opt_tree,mtry=opt_m,importance=TRUE)

# To check important variables
importance(model_rf)      

# plotting the importance of predictors
varImpPlot(model_rf) 

# testing for completely new data
s<-data.frame("Danceability"= 0.326,
              "energy"= 0.0993,
              "key"= 7,
              "loudness"= -22.496,
              "mode"= 1,
              "Speechness"= 0.072,
              "acousticness"= 0.988,
              "Instrumentalness"= 0.916,
              "Liveness"= 0.108,
              "Valence"= 0.417,
              "Tempo"=137.274,
              "Duration_ms"=150973)
              
# predicting the genre
predValid<-predict(model_rf_4,newdata=s)
predValid


#plot(3:8,a)
