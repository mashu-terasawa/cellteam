##balm analysis

dirname2 <- "6_data_dat_wash"


plates <- c(12,24,48,96)

##well��
wells <- 12

##index��
indexs <- 8

##������
cons <- 4

##index�̖��O
indexname <- c("Area","CentroidX","CentroidY","Compactness","Length","LengthWidthLatio","Perimeter","Width")

##�o�[��������
splits <- 5

##���ʂ��o�͂���t�H���_��
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

rbox <- matrix(0,length(plates)*wells,1+(1+splits))  ##1+(1+splits):label���{x�ɓ����������x+1�̐������o�Ă��镪


##�������a�L�^�p
##�̈���ɓ����Ă���_���s�b�N�A�b�v
for(plate in 1:length(plates)){
	
	platename <- plates[plate]
	
	for(well in 1:wells){
		
		reads <- paste(dirname2,"/wash_",platename,"well_",well,".dat",sep="")
		data <- read.table(reads,header=T)
		
		plotx <- data[,"CentroidX"]
		ploty <- data[,"CentroidY"]
		
		##���a
		rx <- (max(data[,"CentroidX"])-min(data[,"CentroidX"]))/2
		ry <- (max(data[,"CentroidY"])-min(data[,"CentroidY"]))/2
		r <- mean(rx,ry)
		
		##�~�̒��S���W
		x01 <- max(data[,"CentroidX"])-r
		x02 <- min(data[,"CentroidX"])+r
		x0 <- mean(x01,x02)
		
		y01 <- max(data[,"CentroidY"])-r
		y02 <- min(data[,"CentroidY"])+r
		y0 <- mean(y01,y02)
		
		
		##���asplit
		rsp <- seq(0,r,(r/splits))    ##���a�𓙕������ăx�N�g����
		
		rbox[well+(plate-1)*wells,2:ncol(rbox)] <-rsp 
		rbox[well+(plate-1)*wells,1] <- paste(platename,"well_",well,sep="")
		
	 	for(split in 1:splits){
			
			rout <- rsp[split+1]
			rin <- rsp[split]
			
			##(x0 - x) * (x0 - x) + (y0 - y) * (y0 -y) < r * r
			##�𖞂����Ƃ��A�_(x,y)�͒��S(x0,y0),���ar�̉~���ɑ���
			dataout <- subset(data,((x0-data[,"CentroidX"])^2+(y0-data[,"CentroidY"])^2)<=rout^2)
			datain <- subset(dataout,((x0-dataout[,"CentroidX"])^2+(y0-dataout[,"CentroidY"])^2)>rin^2)
			
			writes <- paste(dirname_result2,"/",platename,"well_",well,"_split",split,".dat",sep="")
			write.table(datain,writes,sep="\t",row.names=F,col.names=T)
			
			##�m�F�pplot
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






##�s�b�N�A�b�v�����f�[�^���g���ĉ��
##�P�ʖʐϓ�����̐��A�זE�̖ʐς̍��v/�͈̖͂ʐρA�P�x�̍��v/�͈̖͂ʐ� �Ƃ��H

##�����������ꂼ��̖ʐώZ�o
cir <- function(a){
	b <- (a^2)*pi
	return(b)
	}

rbox <- read.table(paste(dirname_result1,"/split_radius.dat",sep=""),header=T)
rbox_area <- rbox
rbox_area[,2:ncol(rbox_area)] <- apply(rbox[,2:ncol(rbox)],c(1,2),cir)
write.table(rbox_area,paste(dirname_result1,"/split_area.dat",sep=""),sep="\t",row.names=F,col.names=T)


##�P�ʖʐϓ������@@@���Z�o���Ă���

##�P�ʖʐϓ�����̐�
numbox <- rbox[,3:ncol(rbox)] ##@@@�����[
#resultbox <- numbox ##�P�ʖʐϓ������@@@�����[

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


##n=3�̕��ρASD�Z�o
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





##�P�ʖʐϓ�����̍זE�ʐ�
numbox <- rbox[,3:ncol(rbox)] ##@@@�����[
#resultbox <- numbox ##�P�ʖʐϓ������@@@�����[

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


##n=3�̕��ρASD�Z�o
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





##�~100
resultbox <- (numbox/rbox_area[,3:ncol(rbox_area)])*100
resultbox2 <- cbind(rbox[,1,drop=F],resultbox)
colnames(resultbox2) <- colnames(numbox2)
writes2 <- paste(dirname_result1,"/cellarea_per_area_100.dat",sep="")
write.table(resultbox2,writes2,sep="\t",row.names=F,col.names=T)


