
##===========Intensity_StdDev==================##
Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	
##各ラベルのデータを全体のデータから抽出します。
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")

##のちに各グラフのタイトルをオブジェクトにぶち込みます
	title = "Intensity_StdDev"

##グラフにしたい列を取り出します。
	Intensity_StdDev_c <- cell[,"StdD"] 
	##薄青
	set.seed(1)
	
##プロットします
	plot(density(Intensity_StdDev_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,20), ylim=c(0, 0.25), xaxs="i", yaxs="i" )
	q1 <- quantile(Intensity_StdDev_c, .05)
	
##区間を決めてあげます	
	q99 <- quantile(Intensity_StdDev_c, .95)
	x1 <- min(which(density(Intensity_StdDev_c)$x >= q1))
	x2 <- max(which(density(Intensity_StdDev_c)$x < q99))
##区間内を塗りつぶします。
	with(density(Intensity_StdDev_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.2),lty=0))
	
##ついでに実際の値を表示します
	rug(Intensity_StdDev_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
##以下重ね合わせです。
	par(new=T) 
	Intensity_StdDev_s <- shadow[,"StdD"]
	##赤紫
	plot(density(Intensity_StdDev_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,20), ylim=c(0, 0.25), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Intensity_StdDev_o <- others[,"StdD"]
	##ライム
	plot(density(Intensity_StdDev_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,20), ylim=c(0, 0.25), xaxs="i", yaxs="i" )
	q5 <- quantile(Intensity_StdDev_o, .05)
	q95 <- quantile(Intensity_StdDev_o, .95)
	x1 <- min(which(density(Intensity_StdDev_o)$x >= q5))
	x2 <- max(which(density(Intensity_StdDev_o)$x < q95))
	with(density(Intensity_StdDev_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Intensity_StdDev_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Intensity_StdDev95.pdf"))
Dens()
dev.off()


##===========Area================##
##Area xlim=c(0, 4000), ylim=c(0, 0.0015)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Area"
	
	Area_c <- cell[,"Area"] 
	##薄青
	set.seed(1)
	plot(density(Area_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 4000), ylim=c(0, 0.0015), xaxs="i", yaxs="i" )
	q1 <- quantile(Area_c, .05)
	q99 <- quantile(Area_c, .95)
	x1 <- min(which(density(Area_c)$x >= q1))
	x2 <- max(which(density(Area_c)$x < q99))
	with(density(Area_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.2),lty=0))
	rug(Area_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	par(new=T) 
	Area_s <- shadow[,"Area"]
	##赤紫
	plot(density(Area_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 4000), ylim=c(0, 0.0015), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Area_o <- others[,"Area"]
	##ライム
	plot(density(Area_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 4000), ylim=c(0, 0.0015), xaxs="i", yaxs="i" )
	q5 <- quantile(Area_o, .05)
	q95 <- quantile(Area_o, .95)
	x1 <- min(which(density(Area_o)$x >= q5))
	x2 <- max(which(density(Area_o)$x < q95))
	with(density(Area_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Area_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Area95.pdf"))
Dens()
dev.off()



##===========Compactness================##
##Com xlim=c(0, 150), ylim=c(0, 0.05)

Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Compactness"
	
	Compactness_c <- cell[,"Com"] 
	##薄青
	set.seed(1)
	plot(density(Compactness_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 150), ylim=c(0, 0.05), xaxs="i", yaxs="i" )
	q1 <- quantile(Compactness_c, .09)
	q99 <- quantile(Compactness_c, .99)
	x1 <- min(which(density(Compactness_c)$x >= q1))
	x2 <- max(which(density(Compactness_c)$x < q99))
	with(density(Compactness_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.2),lty=0))
	rug(Compactness_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	par(new=T) 
	Compactness_s <- shadow[,"Com"]
	##赤紫
	plot(density(Compactness_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 150), ylim=c(0, 0.05), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Compactness_o <- others[,"Com"]
	##ライム
	plot(density(Compactness_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 150), ylim=c(0, 0.05), xaxs="i", yaxs="i" )
	q5 <- quantile(Compactness_o, .09)
	q95 <- quantile(Compactness_o, .99)
	x1 <- min(which(density(Compactness_o)$x >= q5))
	x2 <- max(which(density(Compactness_o)$x < q95))
	with(density(Compactness_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Compactness_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Compactness99.pdf"))
Dens()
dev.off()




##===========Correlation================##
##Cor xlim=c(-0.2, 1.3), ylim=c(0, 4)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Correlation_Mean"
	
	Correlation_Mean_c <- cell[,"Cor"] 
	##薄青
	set.seed(1)
	plot(density(Correlation_Mean_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(-0.2, 1.3), ylim=c(0, 4), xaxs="i", yaxs="i" )
	q5 <- quantile(Correlation_Mean_c, .05)
	q95 <- quantile(Correlation_Mean_c, .95)
	x1 <- min(which(density(Correlation_Mean_c)$x >= q5))
	x2 <- max(which(density(Correlation_Mean_c)$x < q95))
	with(density(Correlation_Mean_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Correlation_Mean_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Correlation_Mean_s <- shadow[,"Cor"]
	##赤紫
	plot(density(Correlation_Mean_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(-0.2, 1.3), ylim=c(0, 4), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Correlation_Mean_o <- others[,"Cor"]
	##ライム
	plot(density(Correlation_Mean_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(-0.2, 1.3), ylim=c(0, 4), xaxs="i", yaxs="i" )
	q5 <- quantile(Correlation_Mean_o, .05)
	q95 <- quantile(Correlation_Mean_o, .95)
	x1 <- min(which(density(Correlation_Mean_o)$x >= q5))
	x2 <- max(which(density(Correlation_Mean_o)$x < q95))
	with(density(Correlation_Mean_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Correlation_Mean_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Correlation_Mean95.pdf"))
Dens()
dev.off()


##===================Energy========================##
##Ene xlim=c(0, 1.3), ylim=c(0, 8)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Energy_Mean"
	
	Energy_Mean_c <- cell[,"Ene"] 
	##薄青
	set.seed(1)
	plot(density(Energy_Mean_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 1.3), ylim=c(0, 8), xaxs="i", yaxs="i" )
	q5 <- quantile(Energy_Mean_c, .01)
	q95 <- quantile(Energy_Mean_c, .99)
	x1 <- min(which(density(Energy_Mean_c)$x >= q5))
	x2 <- max(which(density(Energy_Mean_c)$x < q95))
	with(density(Energy_Mean_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Energy_Mean_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Energy_Mean_s <- shadow[,"Ene"]
	##赤紫
	plot(density(Energy_Mean_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 1.3), ylim=c(0, 8), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Energy_Mean_o <- others[,"Ene"]
	##ライム
	plot(density(Energy_Mean_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 1.3), ylim=c(0, 8), xaxs="i", yaxs="i" )
	q5 <- quantile(Energy_Mean_o, .01)
	q95 <- quantile(Energy_Mean_o, .99)
	x1 <- min(which(density(Energy_Mean_o)$x >= q5))
	x2 <- max(which(density(Energy_Mean_o)$x < q95))
	with(density(Energy_Mean_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Energy_Mean_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Energy_Mean99.pdf"))
Dens()
dev.off()





##===================Homogony========================##
##Homo xlim=c(0.4, 1.3), ylim=c(0, 8)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Homogony_Mean"
	
	Homogony_Mean_c <- cell[,"Homo"] 
	##薄青
	set.seed(1)
	plot(density(Homogony_Mean_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0.4, 1.3), ylim=c(0, 8), xaxs="i", yaxs="i" )
	q5 <- quantile(Homogony_Mean_c, .05)
	q95 <- quantile(Homogony_Mean_c, .95)
	x1 <- min(which(density(Homogony_Mean_c)$x >= q5))
	x2 <- max(which(density(Homogony_Mean_c)$x < q95))
	with(density(Homogony_Mean_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Homogony_Mean_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Homogony_Mean_s <- shadow[,"Homo"]
	##赤紫
	plot(density(Homogony_Mean_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0.4, 1.3), ylim=c(0, 8), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Homogony_Mean_o <- others[,"Homo"]
	##ライム
	plot(density(Homogony_Mean_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0.4, 1.3), ylim=c(0, 8), xaxs="i", yaxs="i" )
	q5 <- quantile(Homogony_Mean_o, .05)
	q95 <- quantile(Homogony_Mean_o, .95)
	x1 <- min(which(density(Homogony_Mean_o)$x >= q5))
	x2 <- max(which(density(Homogony_Mean_o)$x < q95))
	with(density(Homogony_Mean_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Homogony_Mean_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Homogony_Mean95.pdf"))
Dens()
dev.off()




##===================Length========================##
##Len xlim=c(0, 300), ylim=c(0, 0.02)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Length"
	
	Length_c <- cell[,"Len"] 
	##薄青
	set.seed(1)
	plot(density(Length_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 300), ylim=c(0, 0.02), xaxs="i", yaxs="i" )
	q5 <- quantile(Length_c, .05)
	q95 <- quantile(Length_c, .95)
	x1 <- min(which(density(Length_c)$x >= q5))
	x2 <- max(which(density(Length_c)$x < q95))
	with(density(Length_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Length_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Length_s <- shadow[,"Len"]
	##赤紫
	plot(density(Length_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 300), ylim=c(0, 0.02), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Length_o <- others[,"Len"]
	##ライム
	plot(density(Length_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0, 300), ylim=c(0, 0.02), xaxs="i", yaxs="i" )
	q5 <- quantile(Length_o, .05)
	q95 <- quantile(Length_o, .95)
	x1 <- min(which(density(Length_o)$x >= q5))
	x2 <- max(which(density(Length_o)$x < q95))
	with(density(Length_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Length_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Length95.pdf"))
Dens()
dev.off()



##===================LWR========================##
##LWR xlim=c(0,6), ylim=c(0, 1)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Length_Width_Ratio"
	
	Length_Width_Ratio_c <- cell[,"LWR"] 
	##薄青
	set.seed(1)
	plot(density(Length_Width_Ratio_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,6), ylim=c(0, 1), xaxs="i", yaxs="i" )
	q5 <- quantile(Length_Width_Ratio_c, .01)
	q95 <- quantile(Length_Width_Ratio_c, .99)
	x1 <- min(which(density(Length_Width_Ratio_c)$x >= q5))
	x2 <- max(which(density(Length_Width_Ratio_c)$x < q95))
	with(density(Length_Width_Ratio_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Length_Width_Ratio_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Length_Width_Ratio_s <- shadow[,"LWR"]
	##赤紫
	plot(density(Length_Width_Ratio_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,6), ylim=c(0, 1), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Length_Width_Ratio_o <- others[,"LWR"]
	##ライム
	plot(density(Length_Width_Ratio_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,6), ylim=c(0, 1), xaxs="i", yaxs="i" )
	q5 <- quantile(Length_Width_Ratio_o, .01)
	q95 <- quantile(Length_Width_Ratio_o, .99)
	x1 <- min(which(density(Length_Width_Ratio_o)$x >= q5))
	x2 <- max(which(density(Length_Width_Ratio_o)$x < q95))
	with(density(Length_Width_Ratio_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Length_Width_Ratio_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Length_Width_Ratio99.pdf"))
Dens()
dev.off()




##===================Peri========================##
##Perixlim=c(0,1300), ylim=c(0, 0.005)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Perimeter"
	
	Perimeter_c <- cell[,"Peri"] 
	##薄青
	set.seed(1)
	plot(density(Perimeter_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,1300), ylim=c(0, 0.005), xaxs="i", yaxs="i" )
	q5 <- quantile(Perimeter_c, .05)
	q95 <- quantile(Perimeter_c, .95)
	x1 <- min(which(density(Perimeter_c)$x >= q5))
	x2 <- max(which(density(Perimeter_c)$x < q95))
	with(density(Perimeter_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Perimeter_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Perimeter_s <- shadow[,"Peri"]
	##赤紫
	plot(density(Perimeter_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,1300), ylim=c(0, 0.005), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Perimeter_o <- others[,"Peri"]
	##ライム
	plot(density(Perimeter_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,1300), ylim=c(0, 0.005), xaxs="i", yaxs="i" )
	q5 <- quantile(Perimeter_o, .05)
	q95 <- quantile(Perimeter_o, .95)
	x1 <- min(which(density(Perimeter_o)$x >= q5))
	x2 <- max(which(density(Perimeter_o)$x < q95))
	with(density(Perimeter_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Perimeter_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Perimeter95.pdf"))
Dens()
dev.off()


##==================Wid========================##
##Width xlim=c(0,150), ylim=c(0, 0.03


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Width"
	
	Width_c <- cell[,"Wid"] 
	##薄青
	set.seed(1)
	plot(density(Width_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,150), ylim=c(0, 0.03), xaxs="i", yaxs="i" )
	q5 <- quantile(Width_c, .01)
	q95 <- quantile(Width_c, .99)
	x1 <- min(which(density(Width_c)$x >= q5))
	x2 <- max(which(density(Width_c)$x < q95))
	with(density(Width_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Width_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Width_s <- shadow[,"Wid"]
	##赤紫
	plot(density(Width_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,150), ylim=c(0, 0.03), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Width_o <- others[,"Wid"]
	##ライム
	plot(density(Width_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(0,150), ylim=c(0, 0.03), xaxs="i", yaxs="i" )
	q5 <- quantile(Width_o, .01)
	q95 <- quantile(Width_o, .99)
	x1 <- min(which(density(Width_o)$x >= q5))
	x2 <- max(which(density(Width_o)$x < q95))
	with(density(Width_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Width_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Width99.pdf"))
Dens()
dev.off()



##==================MI========================##
##Mean_Int xlim=c(40,100), ylim=c(0, 0.09)


Dens <- function(){

	setwd("C:/terasawa_users")
	data <- read.table("181101教師データ.dat",header=T)
	data_1 <- data[,-1]
	
	cell <- subset(data_1, data_1[,12]=="cell")
	shadow <- subset(data_1, data_1[,12]=="shadow")
	others <- subset(data_1, data_1[,12]=="others")
	
	title = "Mean_Intensity"
	
	Mean_Intensity_c <- cell[,"MI"] 
	##薄青
	set.seed(1)
	plot(density(Mean_Intensity_c), main="",lwd=2,col=rgb(0.5, 0.5, 1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(40,100), ylim=c(0, 0.09), xaxs="i", yaxs="i" )
	q5 <- quantile(Mean_Intensity_c, .01)
	q95 <- quantile(Mean_Intensity_c, .99)
	x1 <- min(which(density(Mean_Intensity_c)$x >= q5))
	x2 <- max(which(density(Mean_Intensity_c)$x < q95))
	with(density(Mean_Intensity_c), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0.5, 0.5, 1, alpha= 0.5),lty=0))
	rug(Mean_Intensity_c, side=1,col=rgb(0.5, 0.5, 1, alpha= 0.8))
	
	
	par(new=T) 
	Mean_Intensity_s <- shadow[,"MI"]
	##赤紫
	plot(density(Mean_Intensity_s), main="",lwd=2,col=rgb(0.8, 0.1, 0.1, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(40,100), ylim=c(0, 0.09), xaxs="i", yaxs="i" )
	
	
	par(new=T) 
	Mean_Intensity_o <- others[,"MI"]
	##ライム
	plot(density(Mean_Intensity_o), main="",lwd=2,col=rgb(0, 1, 0.5, alpha= 0.6) ,xlab =title, ylab="Frequency",xlim=c(40,100), ylim=c(0, 0.09), xaxs="i", yaxs="i" )
	q5 <- quantile(Mean_Intensity_o, .01)
	q95 <- quantile(Mean_Intensity_o, .99)
	x1 <- min(which(density(Mean_Intensity_o)$x >= q5))
	x2 <- max(which(density(Mean_Intensity_o)$x < q95))
	with(density(Mean_Intensity_o), polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col=rgb(0, 1, 0.5, alpha= 0.2),lty=0))
	rug(Mean_Intensity_o, side=3,col=rgb(0, 1, 0.5, alpha= 0.8))
	}
pdf(paste0("C:/terasawa_users/5_実験/解析/180903 NSC34/パラパラセル/各ラベルの指標分布比較/","Mean_Intensity99.pdf"))
Dens()
dev.off()




##Area xlim=c(0, 4000), ylim=c(0, 0.0015)
##Com xlim=c(0, 150), ylim=c(0, 0.05)
##Peri xlim=c(0,1300), ylim=c(0, 0.005)
##Width xlim=c(0,150), ylim=c(0, 0.03)
##
##StdD xlim=c(0,20), ylim=c(0, 0.25)

