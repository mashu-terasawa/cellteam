setwd("C:/terasawa_users/5_実験/解析/180903_細胞密度考慮版/99_cirsiliol/低濃度")

	dir_name <- "mn_sd"
	data <- read.table (paste("all_",dir_name,"_std.dat",sep=""),header=T)
	data_start<- 2
	data_pc <- data[,data_start:ncol(data)]
	label <- read.table("label.dat",header=T)
	name <- as.matrix(data[,1])
	#label <- as.matrix(label)
	##PCデータ取得
	pr <- prcomp(data_pc)
	pc1 <- pr$x[,1]
	pc2 <- pr$x[,2]
	pc3 <- pr$x[,3]
	pdf(paste0(dir_name,"_biplot.pdf"))	
	biplot(pr)
	dev.off()
	
	
	##pc1,2保存
	allpc <- cbind(pc1,pc2,pc3)
	allpc <- cbind(name,allpc)
	colnames(allpc) <- c("name","PC1","PC2","PC3")
	write.table(allpc,paste(dir_name,"_pc.dat",sep=""),row.names=F,sep="\t")
	
	##寄与率算出
	imp <- summary(pr)$importance[2,]
	imp2 <- cbind(paste("pc",1:length(imp),sep=""),imp)
	colnames(imp2) <- c("label","imp")
	write.table(imp2,paste(dir_name,"_imp.dat",sep=""),row.names=F,sep="\t")
	x_label <- paste("PC1(",as.numeric(substr(imp2[1,2],1,5))*100,"%)")
	y_label <- paste("PC2(",as.numeric(substr(imp2[2,2],1,5))*100,"%)")
	
	##係数算出
	rot <- summary(pr)$rotation
	rot2 <- cbind(rownames(rot),rot)
	colnames(rot2) <- c("label",colnames(rot))
	write.table(rot2,paste(dir_name,"_rot.dat",sep=""),row.names=F,sep="\t")
	
	
	##プロットします
	
	cols <- c("#000000","#aaaaaa","#005500","#008000","#00aa00","#00ff00","#80ff80","#d5ffd5","#555500","#808000","#d5d500","#ffff00","#ffff80","#ffffd5")
	xmax <- max(pc1)
	xmin <- min(pc1)
	ymax <- max(pc2)
	ymin <- min(pc2)
	pdf(paste(dir_name,"_pca_trans.pdf",sep=""))
	size <- 2
	
	for(i in 1:length(pc1)){
	
		plot(pc1[i], pc2[i], col=cols[label[data[i,1],2]], xlim=c(xmin-0.1,xmax+0.1),ylim=c(ymin-0.1,ymax+0.1),xlab=x_label,ylab=y_label,cex=size,pch=19)
	  par(new=T)
	  
	}
	for(i in 1:length(pc1)){
	  
	  plot(pc1[i], pc2[i], col="black", xlim=c(xmin-0.1,xmax+0.1),ylim=c(ymin-0.1,ymax+0.1),xlab=x_label,ylab=y_label, cex=size,pch=1)
	  par(new=T)
	  
	}
	
dev.off()





