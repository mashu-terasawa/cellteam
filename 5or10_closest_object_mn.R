setwd("C:/terasawa_users/5_ΐ±/πΝ/180903_ΧE§xlΆΕ/2_Frame")
f_list <- list.files()
f_list

len <- length(f_list)



for(i in 1:len){
		ta_data <- read.table(f_list[i],header=T)
		##[Nbh£Zo
		
		all_dist <- as.matrix(dist(ta_data[,5:6], method = "euclidean", diag = T, upper = T))
		rownum <- nrow(all_dist)
		
		
		for(j in 1:rownum){
		##esΜ[Nbh£πΈΙΐΧΔίT5Βand10ΒΜ½Ο£πίά·B
				if(j == 1){
						ta_row <- all_dist[j,]
						sort_row <- sort(ta_row)
						mean_5 <- mean(sort_row[2:6])
						mean_10 <- mean(sort_row[2:11])
						bd_data_1 <- cbind(mean_5,mean_10)
				}else{
						ta_row <- all_dist[j,]
						sort_row <- sort(ta_row)
						mean_5 <- mean(sort_row[2:6])
						mean_10 <- mean(sort_row[2:11])
						bd_data_2 <- cbind(mean_5,mean_10)
						
						bd_data_1 <- rbind(bd_data_1, bd_data_2)
						
				}
		}
	combine <- cbind(ta_data,bd_data_1)
	f_name <- sub("TERA_nsc34","",f_list[i])
	f_name <- paste0("eu",f_name)
	
	write.table(combine,f_name,row.names=F, col.names=T, sep="\t")
}






