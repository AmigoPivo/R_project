rm(list = ls())
setwd("C:/Users/pivo/Desktop/UT MSBA/Summer 2019/Predictive Models/Project")
library(readr)
library(kknn)
set.seed(79643)

df_spotify <- read_csv("songDb.csv")
genres <- read_table("genre_dataset.txt", col_names = c('Genre'))
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


#write_csv(test_df, 'test.csv')
#write_csv(train_df, 'train.csv')


#near = kknn(genre~(.-Genre-ID-Mode),train_df,test_df,k=50, kernel = "rectangular")
near = kknn(genre~(Energy+Danceability+Acousticness+Loudness+Speechness+Instrumentalness),train_df,test_df,k=50, kernel = "rectangular")
fitted.values(near)
near$fitted


near = kknn(genre~(.-ID-Mode-Duration_ms-Genre),train_df,test_df,k=25, kernel = "rectangular")
labels <- fitted.values(near)
table(labels, test_df$genre)
#summary(near)

###############
#   K-Folds   #
###############
library(caret)

model_knn <- train.kknn(genre~(.-ID-Mode-Duration_ms-Genre), data=train_df, kmax = 100, kcv = 10)
model_knn2 <- train.kknn(genre~(.-ID-Mode-Duration_ms-Genre), data=train_df, kmax = 100, kcv = 5)

model_33 = kknn(genre~(.-ID-Mode-Duration_ms-Genre),train_df,test_df,k=33, kernel = "rectangular")
table(model_33$fitted.values, test_df$genre)
sum(diag(table(model_33$fitted.values, test_df$genre)))/length(model_33$fitted.values)
