# データ読み込み
train <- read.csv("train.csv")
test  <- read.csv("test.csv")


#k-means法(精度が46%しかない、なぜ？)
V=kmeans(train[,-ncol(train)],2)
P=kmeans(test[,-ncol(test)],2)
u<-table(train[,ncol(train)],V$cluster) # for train
t<-table(test[,ncol(test)],P$cluster)   # for test
#accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
#accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])


#k-Nearest Neighbor
library(class)
V<-knn(train[,-ncol(train)],train[,-ncol(train)],train[,ncol(train)],k=5)
P<-knn(train[,-ncol(train)],test[,-ncol(test)],train[,ncol(train)],k=5)

u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
#accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
#accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])



#決定木
library(rpart)
M<-rpart(label~.,data=train)

V<-predict(M,train[,-ncol(train)],type="class")
P<-predict(M,test[,-ncol(test)],type="class")
u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
# accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
# accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])



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

# accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
# accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])



# ナイーブベイズ

library(e1071)
M<-naiveBayes(label~.,train)
V<-predict(M,train[,-ncol(train)])
P<-predict(M,test[,-ncol(test)])

u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
# accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
# accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])




#Random Forest
library(randomForest)
M<-randomForest(label~.,data=train,na.action="na.omit",mtry=45)

V<-predict(M,train[,-ncol(train)])
P<-predict(M,test[,-ncol(test)])

u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test
#accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
# accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])





# Nural Network
library(nnet)

M<-nnet(label~.,size=3,MaxNWts=84581,decay=0.3,data=train)
P<-predict(M,test[,-ncol(test)],type="class")
u<-table(train[,ncol(train)],V) # for train
t<-table(test[,ncol(test)],P)   # for test

#accuracy for train
(u[1,1] + u[2,2]) / (u[1,1] + u[2,2] + u[1,2] + u[2,1])
# accuracy for test
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])


#H2O
library(h2o)
h2o.init()
train.hex <- as.h2o(train)
test.hex <- as.h2o(test)

M<-h2o.deeplearning(1:57,58,
                    train.hex
                    ,activation="RectifierWithDropout",
                    epochs=100,
#                    hidden=c(64,64,64,64),
#                    rate=0.01,
#                    rate_annealing = 1e-7,
#                    input_dropout_ratio = 0.1
)
P<-h2o.predict(M,test.hex)

t<-table(test[,58],as.vector(P$predict))
(t[1,1] + t[2,2]) / (t[1,1] + t[2,2] + t[1,2] + t[2,1])


