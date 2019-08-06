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

# Classification tree
library(tree)
set.seed(79643)
# choose the x,y columns and build a tree model
data = read.csv("train.csv")
df=data[,c(1:11,13,15)]
attach(df)
set.seed(79643)
train = sample(1:nrow(df), nrow(df)*0.02)
tree.music=tree(genre~.,data=df, subset = train)
summary(tree.music)
plot(tree.music)
text(tree.music,pretty=0)
MSE=NULL
#estimate the test error using test dataset
test=read.csv("test.csv")
test = test[,c(1:11,13,15)]
tree.pred = predict(tree.music,test, type = 'class')
result = data.frame(test$genre,tree.pred)
result[result$tree.pred == result$test.genre,"Equal"] <- 1
accuracy_tree = nrow(subset(result, result$Equal == 1)) / nrow(result)
accuracy_tree 

# Prune the tree model 
prun.tree=prune.tree(tree.music,best=8)
# plot the prune tree
plot(prun.tree,type="uniform")
text(prun.tree,pretty=0)
# estimate the test error of prune tree using test dataset
pruntree.pred = predict(prun.tree,test, type = 'class')
result = data.frame(test$genre,pruntree.pred)
result[result$pruntree.pred == result$test.genre,"Equal"] <- 1
accuracy_pruntree = nrow(subset(result, result$Equal == 1)) / nrow(result)
accuracy_pruntree 
