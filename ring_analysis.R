##balm analysis

dirname2 <- "6_data_dat_wash"


plates <- c(12,24,48,96)

##well数
wells <- 12

##index数
indexs <- 8

##条件数
cons <- 4

##indexの名前
indexname <- c("Area","CentroidX","CentroidY","Compactness","Length","LengthWidthLatio","Perimeter","Width")

##バーム分割数
splits <- 5

##結果を出力するフォルダ名
fols <- "balm_split5"




##start


##result
##dir.create("result")
dirname_result1 <- paste("result/",fols,sep="")
dirname_result2 <- paste(dirname_result1,"/subsetdata",sep="")
dirname_result3 <- paste(dirname_result2,"/plot",sep="")

dir.create(dirname_result1)
dir.create(dirname_result2)
dir.create(dirname_result3)

rbox <- matrix(0,length(plates)*wells,1+(1+splits))  ##1+(1+splits):label分＋x個に等分割するとx+1個の数字が出てくる分


##分割半径記録用
##領域内に入っている点をピックアップ
for(plate in 1:length(plates)){
	
	platename <- plates[plate]
	
	for(well in 1:wells){
		
		reads <- paste(dirname2,"/wash_",platename,"well_",well,".dat",sep="")
		data <- read.table(reads,header=T)
		
		plotx <- data[,"CentroidX"]
		ploty <- data[,"CentroidY"]
		
		##半径
		rx <- (max(data[,"CentroidX"])-min(data[,"CentroidX"]))/2
		ry <- (max(data[,"CentroidY"])-min(data[,"CentroidY"]))/2
		r <- mean(rx,ry)
		
		##円の中心座標
		x01 <- max(data[,"CentroidX"])-r
		x02 <- min(data[,"CentroidX"])+r
		x0 <- mean(x01,x02)
		
		y01 <- max(data[,"CentroidY"])-r
		y02 <- min(data[,"CentroidY"])+r
		y0 <- mean(y01,y02)
		
		
		##半径split
		rsp <- seq(0,r,(r/splits))    ##半径を等分割してベクトルに
		
		rbox[well+(plate-1)*wells,2:ncol(rbox)] <-rsp 
		rbox[well+(plate-1)*wells,1] <- paste(platename,"well_",well,sep="")
		
	 	for(split in 1:splits){
			
			rout <- rsp[split+1]
			rin <- rsp[split]
			
			##(x0 - x) * (x0 - x) + (y0 - y) * (y0 -y) < r * r
			##を満たすとき、点(x,y)は中心(x0,y0),半径rの円内に存在
			dataout <- subset(data,((x0-data[,"CentroidX"])^2+(y0-data[,"CentroidY"])^2)<=rout^2)
			datain <- subset(dataout,((x0-dataout[,"CentroidX"])^2+(y0-dataout[,"CentroidY"])^2)>rin^2)
			
			writes <- paste(dirname_result2,"/",platename,"well_",well,"_split",split,".dat",sep="")
			write.table(datain,writes,sep="\t",row.names=F,col.names=T)
			
			##確認用plot
			plotxsub <- datain[,"CentroidX"]
			plotysub <- datain[,"CentroidY"]
			
			png(paste(dirname_result3,"/",platename,"well_",well,"_split",split,".png",sep=""))
			
			plot(plotx,ploty,pch=19,col="gray",xlim=c(0,6000),ylim=c(0,6000),cex=0.1,xlab="CentroidX",ylab="CentroidY",asp=1)
			par(new=T)
			plot(plotxsub,plotysub,pch=19,col="red",xlim=c(0,6000),ylim=c(0,6000),cex=0.1,xlab="",ylab="",asp=1)
			
			dev.off()
			
			}
		
		}
	
	}

colnames(rbox) <- c("label",paste("split",1:(splits+1),sep=""))
write.table(rbox,paste(dirname_result1,"/split_radius.dat",sep=""),sep="\t",row.names=F,col.names=T)






##ピックアップしたデータを使って解析
##単位面積当たりの数、細胞の面積の合計/範囲の面積、輝度の合計/範囲の面積 とか？

##分割したそれぞれの面積算出
cir <- function(a){
	b <- (a^2)*pi
	return(b)
	}

rbox <- read.table(paste(dirname_result1,"/split_radius.dat",sep=""),header=T)
rbox_area <- rbox
rbox_area[,2:ncol(rbox_area)] <- apply(rbox[,2:ncol(rbox)],c(1,2),cir)
write.table(rbox_area,paste(dirname_result1,"/split_area.dat",sep=""),sep="\t",row.names=F,col.names=T)


##単位面積当たりの@@@を算出していく

##単位面積当たりの数
numbox <- rbox[,3:ncol(rbox)] ##@@@を収納
#resultbox <- numbox ##単位面積当たりの@@@を収納

for(plate in 1:length(plates)){
	
	platename <- plates[plate]
	
	for(well in 1:wells){
		
		for(split in 1:splits){
			
			reads <- paste(dirname_result2,"/",platename,"well_",well,"_split",split,".dat",sep="")
			data <- read.table(reads,header=T)
			
			numbox[well+(plate-1)*wells,split] <- nrow(data)
			
			}
		
		print(paste(platename,"well_",well,sep=""))
		
		}
	
	}

numbox2 <- cbind(rbox[,1,drop=F],numbox)

resultbox <- numbox/rbox_area[,3:ncol(rbox_area)]
resultbox2 <- cbind(rbox[,1,drop=F],resultbox)
colnames(numbox2) <- c("label",paste("split",1:splits,sep=""))
colnames(resultbox2) <- colnames(numbox2)

writes1 <- paste(dirname_result1,"/cellnum_raw.dat",sep="")
write.table(numbox2,writes1,sep="\t",row.names=F,col.names=T)
writes2 <- paste(dirname_result1,"/cellnum_per_area.dat",sep="")
write.table(resultbox2,writes2,sep="\t",row.names=F,col.names=T)


##n=3の平均、SD算出
allave <- matrix(0,cons*length(plates),splits)
allsd <- allave

label <- numeric(cons*length(plates))

data <- read.table(paste(dirname_result1,"/cellnum_per_area.dat",sep=""),header=T)
data2 <- data[,-1]

for(plate in 1:length(plates)){
	
	platename <- plates[plate]
	
	for(con in 1:cons){
		
		
		n1 <- data2[con+(plate-1)*wells,]
		n2 <- data2[con+cons+(plate-1)*wells,]
		n3 <- data2[con+cons*2+(plate-1)*wells,]
		nall <- rbind(n1,n2,n3)
		
		allave[con+(plate-1)*cons,] <- apply(nall,2,mean)
		allsd[con+(plate-1)*cons,] <- apply(nall,2,sd)
		
		label[con+(plate-1)*cons] <- paste(platename,"well_condition",con,sep="")
		
		}
	
	}
	

allave2 <- cbind(label,allave)
allsd2 <- cbind(label,allsd)

colnames(allave2) <- colnames(data)
colnames(allsd2) <- colnames(data)

write.table(allave2,paste(dirname_result1,"/cellnum_per_area_mean.dat",sep=""),sep="\t",row.names=F,col.names=T)
write.table(allsd2,paste(dirname_result1,"/cellnum_per_area_sd.dat",sep=""),sep="\t",row.names=F,col.names=T)





##単位面積当たりの細胞面積
numbox <- rbox[,3:ncol(rbox)] ##@@@を収納
#resultbox <- numbox ##単位面積当たりの@@@を収納

for(plate in 1:length(plates)){
	
	platename <- plates[plate]
	
	for(well in 1:wells){
		
		for(split in 1:splits){
			
			reads <- paste(dirname_result2,"/",platename,"well_",well,"_split",split,".dat",sep="")
			data <- read.table(reads,header=T)
			
			numbox[well+(plate-1)*wells,split] <- sum(data[,"Area"])
			
			}
		
		print(paste(platename,"well_",well,sep=""))
		
		}
	
	}

numbox2 <- cbind(rbox[,1,drop=F],numbox)

resultbox <- numbox/rbox_area[,3:ncol(rbox_area)]
resultbox2 <- cbind(rbox[,1,drop=F],resultbox)
colnames(numbox2) <- c("label",paste("split",1:splits,sep=""))
colnames(resultbox2) <- colnames(numbox2)

writes1 <- paste(dirname_result1,"/cellarea_raw.dat",sep="")
write.table(numbox2,writes1,sep="\t",row.names=F,col.names=T)
writes2 <- paste(dirname_result1,"/cellarea_per_area.dat",sep="")
write.table(resultbox2,writes2,sep="\t",row.names=F,col.names=T)


##n=3の平均、SD算出
allave <- matrix(0,cons*length(plates),splits)
allsd <- allave

label <- numeric(cons*length(plates))

data <- read.table(paste(dirname_result1,"/cellarea_per_area.dat",sep=""),header=T)
data2 <- data[,-1]

for(plate in 1:length(plates)){
	
	platename <- plates[plate]
	
	for(con in 1:cons){
		
		
		n1 <- data2[con+(plate-1)*wells,]
		n2 <- data2[con+cons+(plate-1)*wells,]
		n3 <- data2[con+cons*2+(plate-1)*wells,]
		nall <- rbind(n1,n2,n3)
		
		allave[con+(plate-1)*cons,] <- apply(nall,2,mean)
		allsd[con+(plate-1)*cons,] <- apply(nall,2,sd)
		
		label[con+(plate-1)*cons] <- paste(platename,"well_condition",con,sep="")
		
		}
	
	}
	

allave2 <- cbind(label,allave)
allsd2 <- cbind(label,allsd)

colnames(allave2) <- colnames(data)
colnames(allsd2) <- colnames(data)

write.table(allave2,paste(dirname_result1,"/cellarea_per_area_mean.dat",sep=""),sep="\t",row.names=F,col.names=T)
write.table(allsd2,paste(dirname_result1,"/cellarea_per_area_sd.dat",sep=""),sep="\t",row.names=F,col.names=T)





##×100
resultbox <- (numbox/rbox_area[,3:ncol(rbox_area)])*100
resultbox2 <- cbind(rbox[,1,drop=F],resultbox)
colnames(resultbox2) <- colnames(numbox2)
writes2 <- paste(dirname_result1,"/cellarea_per_area_100.dat",sep="")
write.table(resultbox2,writes2,sep="\t",row.names=F,col.names=T)



