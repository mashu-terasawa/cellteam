Corre <- function(){
setwd("C:/terasawa_users")
data <- read.table("181101教師データ.dat",header=T)
data_1 <- data[,-1]


cell <- subset(data_1, data_1[,12]=="cell")
shadow <- subset(data_1, data_1[,12]=="shadow")
others <- subset(data_1, data_1[,12]=="others")

cell_1 <-cell[,-12]
shadow_1 <-shadow[,-12]
others_1 <-others[,-12]

install.packages("psych")
library(psych)
cor.plot(others_1,cex.axis=0.8,main="Others")
}


pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの相関/","others.pdf"))
Corre()


dev.off()

