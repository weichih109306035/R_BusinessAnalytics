#1 做8:2(train:test) 最終test預測的table算正確率（0對01對1 1對1/全部？） 
library(tidyverse)
net <- read.csv("socialnetwork.csv")
net <-net[,-c(1)]#net2第一欄為順序 不要
newnet <- net %>%
  mutate(
    edu=as.factor(ifelse(Education=='basic',0,ifelse(Education=='highschool',1,
      ifelse(Education=='college',2,ifelse(Education=='Master',3,4))))),
    mar=as.factor(ifelse(Marital_Status=='Absurd',0,ifelse(
      Marital_Status=='Alone',1,ifelse(Marital_Status=='Divorced',2,
      ifelse(Marital_Status=='Married',3,ifelse(Marital_Status=='Single',4,
      ifelse(Marital_Status=='Together',5,ifelse(Marital_Status=='Widow',6,7)))))))),
    gender=as.factor(ifelse(Gender=='Male',0,1)),
  )
newnet <-newnet[,-c(1,2,7)]
newnet$Response <- as.factor(newnet$Response)
newnet$Purchased <- as.factor(newnet$Purchased)

data0=newnet[newnet$Purchased==0,]
data1=newnet[newnet$Purchased==1,]

nrow0<-nrow(data0)
nrow1<-nrow(data1)

set.seed(3333)
train0=data0[sample(1:nrow0,0.8*nrow0),]
test0=data0[-sample(1:nrow0,0.8*nrow0),]

train1=data1[sample(1:nrow1,0.8*nrow1),]
test1=data1[-sample(1:nrow1,0.8*nrow1),]

train=rbind(train0,train1)
rownames(train)<-1:nrow(train)

test=rbind(test0,test1)
rownames(test)<-1:nrow(test)
library(randomForest)
rf<-randomForest(Purchased ~.,train,ntree=100,importance=T)
plot(rf)
varImpPlot(rf)
pred=predict(rf,newdata = test)
rfcm<-table(Real=test$Purchased,Predict=pred)
rfcm

#2
#下面這一步其實可以不用做
newnet2 <- newnet %>%
  mutate(
    MSalary = (MSalary - min(MSalary)) / (max(MSalary) - min(MSalary))*10,
    Spent = (Spent - min(Spent)) / (max(Spent) - min(Spent))*10,
    Age = (Age - min(Age)) / (max(Age) - min(Age))*10,
  )
ggplot(newnet2, aes(x=NumWebPurchases, y=NumStorePurchases)) +
  geom_point()
library(factoextra)
p = fviz_nbclust(newnet2, 
                 FUNcluster = hcut,  # hierarchical clustering
                 method = "wss",     # total within sum of square
                 k.max = 10          # max number of clusters to consider
) 
p
(p = p + labs(title="Elbow Method") )

p + geom_vline(xintercept = 2,       # 在 X=2的地方 
               linetype = 2)
fviz_nbclust(newnet2, kmeans, method = "silhouette")
k = kmeans(newnet2[,c(3,4)], centers=2)
fviz_cluster(k,           # 分群結果
             data = newnet2[,c(3,4)],              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             ellipse.type = "norm")      # 框架型態
#######################################################


library(useful)
plot(k,data=newnet2,class="Purchased")

k$centers
k$withinss #within class variance
k$tot.withinss
table(k$cluster, newnet$Response)  
k$size # cluster size
str(k)
k$cluster
library(useful)
plot(k,data=newnet2,class="Purchased")
library(factoextra)
fviz_cluster(k,           # 分群結果
             data = newnet[,c(5,7)],              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             ellipse.type = "norm")      # 框架型態

#library(cluster)
library(factoextra)
p = fviz_nbclust(newnet2[,1:11], 
                 FUNcluster = hcut,  # hierarchical clustering
                 method = "wss",     # total within sum of square
                 k.max = 10          # max number of clusters to consider
) 
p
(p = p + labs(title="Elbow Method for HC") )

p + geom_vline(xintercept = 2,       # 在 X=2的地方 
               linetype = 2)

fviz_nbclust(newnet[,c(5,8)], kmeans, method = "silhouette")#也是建議2群







min(net$Age)#18歲->Decision tree 出來一種好像是18-21會點廣告買
unique(net$Marital_Status)
#1 做8:2(train:test) 最終test預測的table算正確率（0對01對1 1對1/全部？） 
library(tidyverse)
net <- read.csv("socialnetwork.csv")
net <-net[,-c(1)]#net2第一欄為順序 不要

library(rpart)
tree <- rpart(Purchased ~. ,data=net, method="class") #inside part: y~X1+X2+...
pred <- predict(tree, newdata=net, type="class")
table(Real = net$Purchased, Predict = pred)

library(rpart.plot) 
rpart.plot(tree)
rpart.rules(tree,cover=T)#可以看圖下結論? 0左1右 根據node代表的變數講

#2
newnet <- net %>%
  mutate(
    NumDealsPurchases = (NumDealsPurchases - min(NumDealsPurchases)) / (max(NumDealsPurchases) - min(NumDealsPurchases)),
    NumStorePurchases = (NumStorePurchases - min(NumStorePurchases)) / (max(NumStorePurchases) - min(NumStorePurchases)),
    NumWebPurchases = (NumWebPurchases - min(NumWebPurchases)) /(max(NumWebPurchases) - min(NumWebPurchases))
  )
ggplot(net, aes(x=NumStorePurchases, y=NumWebPurchases)) +
  geom_point()
k = kmeans(net[,c(7:8)], centers=2) # k=2
k$centers
k$withinss #within class variance
k$tot.withinss
table(k$cluster, newnet$Response)  
k$size # cluster size
str(k)
k$cluster
library(useful)
plot(k,data=newnet,class="Response")

#library(cluster)
library(factoextra)
p = fviz_nbclust(newnet[,c(7,8)], 
                 FUNcluster = hcut,  # hierarchical clustering
                 method = "wss",     # total within sum of square
                 k.max = 8          # max number of clusters to consider
) 
p
(p = p + labs(title="Elbow Method for HC") )

p + geom_vline(xintercept = 2,       # 在 X=2的地方 
               linetype = 2)

fviz_nbclust(newnet[,c(7:8)], kmeans, method = "silhouette")#也是建議2群

