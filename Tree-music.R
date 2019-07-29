library(MASS)
data = read.csv("train.csv")
attach(data)
library(tree)
summary(data)
View(data)


#regression tree
library(MASS)
library(tree)
set.seed(1)
df=data[,c(1:11,13,15)]
train = sample(1:nrow(df), nrow(df)/2)
tree.music=tree(genre~.,data=df,subset=train)
summary(tree.music)
plot(tree.music)
text(tree.music,pretty=0)
MSE=NULL
#estimate the test error using test dataset
test=read.csv("test.csv")
df_test = test[,c(1:11,13,15)]
tree.pred = predict(tree.music,df_test,type="class")
result = data.frame(df_test$genre,tree.pred)
result[result$tree.pred == result$df_test.genre,"Equal"] <- 1
test_error = 1 - nrow(subset(result, result$Equal == 1)) / nrow(result)
test_error
# Use a big tree we can get test_error=0.7823618

-------------------------------
#then prune it down 
prun.tree=prune.tree(tree.music,best=25)
cat('pruned tree size: \n')
print(length(unique(prun.tree$where)))

#?? why prun tree = tree music? what does prune mean?


#plot the tree 
par(mfrow=c(1,1))
plot(prun.tree,type="uniform")
options(digits=5)
text(prun.tree,pretty=0,col="blue",label=c("yprob"),cex=.8)
options(digits=7)

-----------------------------------------------------
# Use cross validation to prune tree
cv.music = cv.tree(tree.music,FUN=prune.misclass) #??????
names(cv.music)
plot(cv.music$size,cv.music$dev,type='b')




-------------------------------------------------
#Rpart 
big.tree = rpart(genre~.,method="anova",data=df,
                 control=rpart.control(minsplit=5,cp=.0005))
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')
par(mfrow=c(1,1))
plot(big.tree)

#--------------------------------------------------
#show fit from some trees

oo=order(df$Danceability)
bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"]
cat('bestcp: ',bestcp,'\n')
# bestcp = 0.0040
#plot best tree

par(mfrow=c(1,1))
best.tree = prune(big.tree,cp=bestcp)
plot(best.tree,uniform=TRUE)
text(best.tree)
summary(best.tree)

------------------------------
#bagging
library(randomForest)
set.seed(1)
df=data[,c(1:11,13,15)]
train = sample(1:nrow(df), nrow(df)/2)
bag.music=randomForest(genre~.,data=df,subset=train,
                        mtry=12,ntree=180,importance =TRUE)
test=read.csv("test.csv")
df_test = test[,c(1:11,13,15)]
yhat.bag = predict(bag.music,newdata=df_test)
result2 = data.frame(df_test$genre,yhat.bag)
result2[result2$yhat.bag == result2$df_test.genre,"Equal"] <- 1
test_error2 = 1 - nrow(subset(result2, result2$Equal == 1)) / nrow(result2)
test_error2
# From Bagging, we can get the test error is 0.5770

-------------------------
#random forest
rf.music=randomForest(genre~.,data=df,subset=train,
                       mtry=4,ntree=220,importance =TRUE)
yhat.rf = predict(rf.music ,newdata=df_test)
result3 = data.frame(df_test$genre,yhat.rf)
result3[result3$yhat.rf == result3$df_test.genre,"Equal"] <- 1
test_error3 = 1 - nrow(subset(result3, result3$Equal == 1)) / nrow(result3)
test_error3
# From Randomforest, we can get the test error is 0.5728

---------------------
#randomforest 

#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!
set.seed(99)
n = nrow(df)
ntreev = c(10,500,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
  cat('doing Music rf: ',i,'\n')
  rffit = randomForest(genre~.,data=df,ntree=ntreev[i],maxnodes=15)
  fmat[,i] = predict(rffit)
}
#plot oob error using last fitted rffit which has the largest ntree.
par(mfrow=c(1,2))
plot(rffit)
rffit
#--------------------------------------------------
#fit random forest and plot variable importance
rffit = randomForest(genre~.,data=df,mtry=5,ntree=100)
varImpPlot(rffit)


