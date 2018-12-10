

setwd("C:/terasawa_users/5_実験/解析/180702 NSC34/96well/1_frame")
getwd()

f_list <- list.files()
f_len <- length(f_list)


for(i in 1:f_len){
		
		
		
		read_data <- read.table(f_list[i], header = T)
		f_name <- sub(".dat", "" , f_list[i])
		
		
		##端っこの認識してしまった部分を消す
		
		
		##(x-3912)²+(y-3912)²≦3795.2²を条件にして抽出
		well_data <- subset(read_data, subset = ((Channel.2.Mask.0_CentroidX...m. - 3912) ^2) + ((Channel.2.Mask.0_CentroidY...m. - 3912) ^2) <= 3795.2 ^2 )
		
		
		
		##Length_Width_RatioでWash
		refined_data <- subset(well_data, well_data[,"Channel.2.Mask.0_LengthWidthRatio"] <= 10)
		
		##AreaでWash
		min		<- 250
		max		<- 8000
		washed_data <- subset(refined_data, refined_data[,"Channel.2.Mask.0_Area...m.."] <= max)
		washed_data <- subset(washed_data, washed_data[,"Channel.2.Mask.0_Area...m.."] >= min)
		
		


##ユークリッド距離を算出==============================================================


		all_dist <- as.matrix(dist(washed_data[,5:6], method = "euclidean", diag = T, upper = T))
		all_max <- max(all_dist)
		
		
		##ユークリッド距離の行列：各列の最大値を抽出してつなげる
		len <- ncol(all_dist)
				for(j in 1:len){
					if(j == 1){
					max_1 <- matrix(max(all_dist[,j]))
				}else{
				
					max_2 <- matrix(max(all_dist[,j]))
					max_1 <- matrix(rbind(max_1, max_2))
				
				}
			}
		##ユークリッド距離の最大値
		max_1
		
		##ユークリッド距離が最大値をとるときのX, Y座標
		
		start_num <- which.max(max_1)
		a_list <- matrix(which.max(all_dist[,start_num]))
		target_num <- a_list[1,1]
		
		
		
		
		##ここからは円の中心座標の算出
		##最も大きいユークリッド距離をとる2点の座標取得
			locus_data <- washed_data[,c("Channel.2.Mask.0_CentroidX...m.", "Channel.2.Mask.0_CentroidY...m.")]
			st_loci <- locus_data[start_num,]
			ta_loci <- locus_data[target_num,]
		
		
		##中心座標
			x_center <- (st_loci[1,"Channel.2.Mask.0_CentroidX...m."] + ta_loci[1,"Channel.2.Mask.0_CentroidX...m."]) / 2
			y_center <- (st_loci[1,"Channel.2.Mask.0_CentroidY...m."] + ta_loci[1,"Channel.2.Mask.0_CentroidY...m."]) / 2


		##半径
			r <- all_max/2 + 100



##===================================================================================


		##バーム分割数
		splits <- 5
		
		##well数
		wells <- 12

		##半径split
		rsp <- seq(0,r,(r/splits))    ##半径を等分割してベクトルに
			
			for(split in 1:splits){
					rout <- rsp[split+1]
					rin <- rsp[split]
			
					##(x0 - x) * (x0 - x) + (y0 - y) * (y0 -y) < r * r
					##を満たすとき、点(x,y)は中心(x0,y0),半径rの円内に存在
					dataout <- subset(washed_data,((x_center-washed_data[,"Channel.2.Mask.0_CentroidX...m."])^2+(y_center-washed_data[,"Channel.2.Mask.0_CentroidY...m."])^2)<=rout^2)
					datain <- subset(dataout,((x_center-dataout[,"Channel.2.Mask.0_CentroidX...m."])^2+(y_center-dataout[,"Channel.2.Mask.0_CentroidY...m."])^2)>rin^2)
			
					##確認用plot
					plotsub <- datain[,c("Channel.2.Mask.0_CentroidX...m.","Channel.2.Mask.0_CentroidY...m.")]
					ref_for_plot <- washed_data[,c("Channel.2.Mask.0_CentroidX...m.","Channel.2.Mask.0_CentroidY...m.")]
			
					
					
					write.table(datain,paste0("C:/terasawa_users/5_実験/解析/180702 NSC34/96well/2_バーム済/データシート/","Balm_",split,"_", f_name,".dat"), sep="\t",row.names=F,col.names=T)
					png(paste0("C:/terasawa_users/5_実験/解析/180702 NSC34/96well/2_バーム済/確認用plot/",f_name,"_",split,".png"))
					
					plot(ref_for_plot,xlab ="X", ylab="Y", col="#72b8b8", pch=20, cex=2.0, bty="o", xlim=c(0, 7824), ylim=c(0, 7824), xaxs="i", yaxs="i")
					par(new=T)
					plot(plotsub,  xlab="X", ylab = "Y",col="red", pch=20, cex=2.0, bty="o", xlim=c(0, 7824), ylim=c(0, 7824), xaxs="i", yaxs="i")
					dev.off()
				
			}
	}
	
	

##各グラフィックスパラメータの説明
	##pchはプロットの形
	##cexはプロットの大きさをデフォルトの何倍にするかを決める
	##bty="o"はグラフの描写を枠型（四方を囲む）にする。
	##limで範囲を指定
	##デフォルトでは軸の範囲が4%拡張されているのでxaxs,yaxsで補正（"i"にするとピッタリ収まるらしい）






