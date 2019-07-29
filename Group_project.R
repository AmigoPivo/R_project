rm(list = ls())
setwd("C:/Users/pivo/Documents/GitHub/R_project")
library(readr)
library(kknn)
library(gbm)
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

#######################
#   K-Folds by hand   #
#######################

kcv = 10
n = dim(df_genre2)[1]
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train_df[-val,]
  test_i = test_df[val,]
  
  for(i in 1:100){
    
    near = kknn(genre~(.-ID-Mode-Duration_ms-Genre),train_i,test_i,k=i,kernel = "rectangular")
    aux = mean(near$fitted.values == test_i$genre)
    out_MSE[j,i] = aux
  }

  used = union(used,val)
  set = (1:n)[-used]
  cat(j,'\n')
}

mMSE = apply(out_MSE,2,mean)
plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)

#######################
#   K-Folds by kknn   #
#######################
#library(caret)

model_knn <- train.kknn(genre~(.-ID-Mode-Duration_ms-Genre), data=train_df, kmax = 100, kcv = 10)
model_knn2 <- train.kknn(genre~(.-ID-Duration_ms-Genre), data=train_df, kmax = 100, kcv = 10)

model_knn$best.parameters
model_knn2$best.parameters

model_33 = kknn(genre~(.-ID-Mode-Duration_ms-Genre),train_df,test_df,k= 21, kernel = "rectangular")
model_Mode = kknn(genre~(.-ID-Duration_ms-Genre),train_df,test_df,k= 47, kernel = "rectangular")

table(model_Mode$fitted.values, test_df$genre)
table(model_33$fitted.values, test_df$genre)

sum(diag(table(model_Mode$fitted.values, test_df$genre)))/length(model_Mode$fitted.values)
sum(diag(table(model_33$fitted.values, test_df$genre)))/length(model_33$fitted.values)

####### Boosting
ntrees=5000
boostfit = gbm(genre~.-ID-Genre-Key-Mode,data=train_df,distribution='gaussian',
               interaction.depth=3,n.trees=ntrees,shrinkage=.2)
summary(boostfit)
varImpPlot(boostfit)


boostfit$response.name

pred = predict(boostfit,newdata=test_df,n.trees=ntrees)
length(pred)
summary(pred)

test_x = cbind(test_df$genre, pred)
View(test_df)
table(test_df$genre, round(pred))
