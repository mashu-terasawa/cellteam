install.packages("randomForest")
library(randomForest)


setwd("C:/terasawa_users")
data <- read.table("181101���t�f�[�^.dat",header=T)
data_1 <- data[,-1]
##data_1 <-data_1[c(1:),]##

ndata <- nrow(data_1)
size <- ndata*0.5
set.seed(1)

ridx <- sample(ndata,ndata*0.5)



##�w�K�p�f�[�^�쐬
train_1 <- data_1[ridx,]
##train_1 <- data_1[1:708,]


##�e�X�g�p�f�[�^�쐬(data.learn�ȊO�̂���)
test_1 <- data_1[-ridx,]
##test_1 <- data_1[709:897,]

##TUNE <- tuneRF(train_1,train_1$Type, plot=TRUE, doBest=FALSE,improve=0.05)

forest <- randomForest(Type~.,data = train_1)


pred_forest <- predict(forest,newdata = test_1,type = "class")
table_2 <- table(test_1[,12],pred_forest)
true_2 = (table_2[1,1]+table_2[2,2]+table_2[3,3])
num <- ndata/2

cat(paste0("�\�����x��",100*(true_2/num),"%�ł��B"))


varImpPlot(forest)
importance(forest)













##=================�N���X�o���f�[�V����==================#

setwd("C:/terasawa_users")
data <- read.table("181101���t�f�[�^.dat",header=T)
data_1 <- data[,-1]
data_2 <- data_1[c(1:880),]

ndata <- nrow(data_2)

##10-fold�N���X�o���f�[�V����
div_num <- ndata/10
div_num ##1~10


for(i in 1:10){
		if(i == 1){
				min <- (i+1)
				max <- i+div_num
				train_1 <- data_2[-c(min:max),]
				test_1 <- data_2[c(min:max),]
				
				
				
				forest <- randomForest(Type~.,data = train_1,depth=15)
				pred_forest <- predict(forest,newdata = test_1,type = "class")
				table_1 <- table(test_1[,12],pred_forest)
				
				true_1 = (table_1[1,1]+table_1[2,2]+table_1[3,3])
				
		}else{
				min <- (div_num*i)-50
				max <- div_num*(i+1)-51
				train_1 <- data_2[-c(min:max),]
				test_1 <- data_2[c(min:max),]
				
				
				
				forest <- randomForest(Type~.,data = train_1,depth=15)
				pred_forest <- predict(forest,newdata = test_1,type = "class")
				table_2 <- table(test_1[,12],pred_forest)
				table_1 <- table_1+table_2 
				
				true_2 = (table_2[1,1]+table_2[2,2]+table_2[3,3])
				true_1 <- rbind(true_1,true_2)
		}
}
sum <- sum(true_1)
mean <- sum/880
mean
cat(paste0("�\�����x��",mean*100,"%�ł��B"))
cat("�e�[�u����\�����܂�")
table_1
pdf(paste0("C:/terasawa_users/","�d�v�x.pdf"))
varImpPlot(forest,,main="�d�v�x",pch=19)
importance(forest)

dev.off()


##leave-one-out�ŃN���X�o���f�[�V����

div_num <- ndata
div_num ##1~520
data_2 <- data_1

for(i in 1:div_num){
		if(i == 1){
				min <- 1
				train_1 <- data_2[-min,]
				test_1 <- data_2[min,]
				
				
				
				forest <- randomForest(Type~.,data = train_1)
				pred_forest <- predict(forest,newdata = test_1,type = "class")
				table_1 <- table(test_1[,12],pred_forest)
				
				true_1 = (table_1[1,1]+table_1[2,2]+table_1[3,3])
				
		}else{
				min <- i
				
				train_1 <- data_2[-min,]
				test_1 <- data_2[min,]
				
				
				
				forest <- randomForest(Type~.,data = train_1)
				pred_forest <- predict(forest,newdata = test_1,type = "class")
				table_2 <- table(test_1[,12],pred_forest)
				table_1 <- table_1+table_2 
				
				true_2 = (table_2[1,1]+table_2[2,2]+table_2[3,3])
				true_1 <- rbind(true_1,true_2)
		}
}
sum <- sum(true_1)
mean <- sum/520
mean


cat(paste0("�\�����x��",mean*100,"%�ł��B"))
cat("�e�[�u����\�����܂�")
table_1











##=================�N���X�o���f�[�V����==================#

setwd("C:/terasawa_users")
data <- read.table("181101���t�f�[�^_�����w�Wa.dat",header=T)
data_1 <- data[,-1]
data_1 <- subset(data_1,data_1[,10]!="shadow")


ndata <- nrow(data_1)
data_2 <- data_1[1:400,]


write.table(data_2,"b.dat",row.names=F, col.names=T, sep="\t")
data <- read.table("b.dat",header=T)
data_2 <- data


##10-fold�N���X�o���f�[�V����
div_num <- nrow(data_2)/10
div_num ##1~10


for(i in 1:10){
		if(i == 1){
				min <- i
				max <- i+div_num-1
				train_1 <- data_2[-c(min:max),]
				test_1 <- data_2[c(min:max),]
				
				
				forest <- randomForest(Type~.,data = train_1)
				pred_forest <- predict(forest,newdata = test_1,type = "class")
				table_1 <- table(test_1[,16],pred_forest)
				
				true_1 = (table_1[1,1]+table_1[2,2])
				
		}else{
				min <- div_num*i
				max <- div_num*i+40
				train_1 <- data_2[-c(min:max),]
				test_1 <- data_2[c(min:max),]
				
				
				
				forest <- randomForest(Type~.,data = train_1)
				pred_forest <- predict(forest,newdata = test_1,type = "class")
				table_2 <- table(test_1[,16],pred_forest)
				table_1 <- table_1+table_2 
				
				true_2 = (table_2[1,1]+table_2[2,2])
				true_1 <- rbind(true_1,true_2)
		}
}
sum <- sum(true_1)
mean <- sum/400
mean
cat(paste0("�\�����x��",mean*100,"%�ł��B"))
cat("�e�[�u����\�����܂�")
table_1
pdf(paste0("C:/terasawa_users/","�d�v�xb.pdf"))
varImpPlot(forest,,main="Importance",pch=19)
importance(forest)

dev.off()












