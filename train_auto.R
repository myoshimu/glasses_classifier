# For batch process purpose

# Load vectors
train <- read.csv("train.csv")
test  <- read.csv("test.csv")


#k-means法(精度が46%しかない、なぜ？)
V=kmeans(train[,-ncol(train)],2)
P=kmeans(test[,-ncol(test)],2)
u<-table(train[,ncol(train)],V$cluster) # for train
t<-table(test[,ncol(test)],P$cluster)   # for test
#accuracy for train

#accuracy for test

print("k-means")
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))


#k-Nearest Neighbor
library(class)
V<-knn(train[,-ncol(train)],train[,-ncol(train)],train[,ncol(train)],k=5)
P<-knn(train[,-ncol(train)],test[,-ncol(test)],train[,ncol(train)],k=5)

u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
print("knn")

#accuracy for train
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
#accuracy for test
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))



#決定木
library(rpart)
M<-rpart(label~.,data=train)

V<-predict(M,train[,-ncol(train)],type="class")
P<-predict(M,test[,-ncol(test)],type="class")
u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
print("tree")

# accuracy for train
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
# accuracy for test
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))





#SVM
library(e1071)
M <- svm(
  label ~ .,
  data  = train,
  gamma = 0.1767767,
  cost  = 2.828427
)

V <- predict(M, train)
P <- predict(M, test)
u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test

print("SVM")
# accuracy for train
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
# accuracy for test
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))



# ナイーブベイズ

library(e1071)
M<-naiveBayes(label~.,train)
V<-predict(M,train[,-ncol(train)])
P<-predict(M,test[,-ncol(test)])

u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
print("naiveBayes")
# accuracy for train
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
# accuracy for test
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))


#Random Forest
library(randomForest)
M<-randomForest(label~.,data=train,na.action="na.omit",mtry=45)

V<-predict(M,train[,-ncol(train)])
P<-predict(M,test[,-ncol(test)])

u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
print("RandomForest")
#accuracy for train
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
# accuracy for test
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))


# Nural Network
library(nnet)

M<-nnet(label~.,size=3,MaxNWts=84581,decay=0.3,data=train)
P<-predict(M,test[,-ncol(test)],type="class")
u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test

print("nnet")
#accuracy for train
print((u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1]))
# accuracy for test
print((t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1]))

