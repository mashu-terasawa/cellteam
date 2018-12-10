setwd("C:/terasawa_users")





data <- read.table("180902_1555.dat", header=T)
data <- data[,-1]

train <- data[1:200,]
test <- data[201:500,]

library(MASS)


##1,3と2を分けられる関数
Z <- lda(Type~.,data=train)
Z
apply(Z$means%*%Z$scaling,2,mean)

##1と3を分ける関数
train_2 <- subset(train, train[,12]!=2)
W <- lda(Type~.,data=train_2)
W
apply(W$means%*%W$scaling,2,mean)
table(train_2[,12],predict(W)$class)

##テストデータを判別する
Y <- predict(Z,test)
table(test[,12],Y$class)


class1 <- as.matrix(Y$class)
test_2 <- cbind(test, class1)

test_3 <- subset(test_2, test_2[,13]!=2)
test_4 <- test_3[,-12]


X <- predict(W,test_4)
table(test_3[,12],X$class)






##========================クロスバリデーション============================##

data_cv <- lda(Type~., data=data, CV=T)
lda_tab <- table(data[,12],data_cv$class)
print(sum(lda_tab[row(lda_tab)==col(lda_tab)])/sum(lda_tab))


data_cv1 <- lda(Type~., data=data_3, CV=T)
lda_tab <- table(data[,12],data_cv$class)
print(sum(lda_tab[row(lda_tab)==col(lda_tab)])/sum(lda_tab))



