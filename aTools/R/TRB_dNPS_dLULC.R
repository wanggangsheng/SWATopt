# setwd("~/Dropbox (ORNL)/Model/SWATopt/aTools/R")
# source("TRB_dNPS_dLULC.R")

library(corrplot)
library(sensitivity)
rm(list=ls())

path <- getwd()
print(path)

dir_io <- "SWAT_TRB"
dir_inout <- c("in_data", "out_data", "out_plot")
path_io <- paste0(path, "/", dir_io)
path_in  <- paste0(path_io, "/", dir_inout[1])
path_out <- paste0(path_io, "/", dir_inout[2])
path_fig <- paste0(path_io, "/", dir_inout[3])

#--------------------------------------------------------------------
##					1		2		3			4		5			6     ## 7	8	9  ## 10	11	12	13
LULC_Class1 <- c("Water", "Barren", "Urban", "Shrub", "Wetland", "Forest", 
				"Crop","Hay", "Pasture",  
				"Switchgrass", "Miscanthus", "Willow", "BioCrop") 
LULC_Urban <- c("URLD", "URMD", "URHD", "UTRN", "UIDU")
LULC_Forest <- c("FRST", "FRSE", "FRSD")
LULC_Wetland <- c("WETF", "WEHB", "WETN", "WETL")
LULC_Pasture <- c("PAST", "GRAS")
LULC_BioCrop <- c("SWCH", "MISG", "WILL")
nLULC1 <- length(LULC_Class1)
#--------------------------------------------------------------------

cases <- c("Base", "BC1", "HH3")
ncase <- length(cases)
Scenario <- character(ncase -1)
for(i in 1:(ncase-1)){
	Scenario[i] = paste0(cases[i+1],".vs.",cases[1])
}

print(paste0(">>>Read input data in ",path_in))
file_in_name <- paste0("sub_lulc_area_",cases,".txt")
file_in <- paste0(path_in,"/", file_in_name)
data_in <- vector("list",ncase)
for (i in 1:ncase){
	data_in[[i]] <- read.table(file_in[i], sep="", header=TRUE)
}

print(">>>Process input data")
nsub <- nrow(data_in[[1]])
ncol_skip <- 1  ## skip "SUBBASIN"

fn1 <-  paste0(path_out,"/sub_area_perc_lulc0_",cases,".txt") 
data_new <- vector("list",ncase)
for (i in 1:ncase){
	ncol0 <- ncol(data_in[[i]])
	ncol <- ncol0 - ncol_skip
	data_new[[i]] <- data_in[[i]][,(ncol_skip+1):ncol0]
	sub_area <- rowSums(data_new[[i]])
	data_new[[i]] <- data_new[[i]]/sub_area*100

	write.table(round(data_new[[i]],4),fn1[i],sep="\t",row.names=FALSE)
}

colClasses <- rep("numeric",nLULC1)
colnames <- LULC_Class1

sub_area_perc_lulc1 <- vector("list",ncase)
file_out <-  paste0(path_out,"/sub_area_perc_lulc1_",cases,".txt") 
for (i in 1:ncase){
	col_remove <- list()
	ncol_remove <- 0

	sub_area_perc_lulc1[[i]] <- read.table(text = "", colClasses = colClasses, col.names = colnames)
	sub_area_perc_lulc1[[i]][1:nsub,] <- 0.0
	cnames <- colnames(data_new[[i]])

	LULC_SWAT <- "WATR"
	jClass1 <- 1
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC_SWAT <- "BARR"
	jClass1 <- 2
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC_SWAT <- "SHRB"
	jClass1 <- 4
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC_SWAT <- "HAY"
	jClass1 <- 8
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC_SWAT <- "SWCH"
	jClass1 <- 10
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC_SWAT <- "MISG"
	jClass1 <- 11
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC_SWAT <- "WILL"
	jClass1 <- 12
	if(LULC_SWAT %in% cnames){
		sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- data_new[[i]][,eval(LULC_SWAT)]
	} else {
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC2 <- LULC_Urban
	jClass1 <- 3
	kk <- 0
	for(j in 1:length(LULC2)){
		if(LULC2[j] %in% cnames){
			kk <- kk +1
			sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] + 
															   		 data_new[[i]][,eval(LULC2[j])]
			} 
	}
	if (kk==0){
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC2 <- LULC_Wetland
	jClass1 <- 5
	kk <- 0
	for(j in 1:length(LULC2)){
		if(LULC2[j] %in% cnames){
			kk <- kk +1
			sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] + 
															   		 data_new[[i]][,eval(LULC2[j])]
			} 
	}
	if (kk==0){
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC2 <- LULC_Forest
	jClass1 <- 6
	kk <- 0
	for(j in 1:length(LULC2)){
		if(LULC2[j] %in% cnames){
			kk <- kk +1
			sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] + 
															   		 data_new[[i]][,eval(LULC2[j])]
			} 
	}
	if (kk==0){
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC2 <- LULC_Pasture
	jClass1 <- 9
	kk <- 0
	for(j in 1:length(LULC2)){
		if(LULC2[j] %in% cnames){
			kk <- kk +1
			sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] + 
															   		 data_new[[i]][,eval(LULC2[j])]
			} 
	}
	if (kk==0){
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	LULC2 <- LULC_BioCrop
	jClass1 <- 13
	kk <- 0
	for(j in 1:length(LULC2)){
		if(LULC2[j] %in% cnames){
			kk <- kk +1
			sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] + 
															   		 data_new[[i]][,eval(LULC2[j])]
			} 
	}
	if (kk==0){
		ncol_remove <- ncol_remove + 1
		col_remove[[ncol_remove]] <- which(LULC_Class1[jClass1] == colnames)
		# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)]
	}

	jClass1 <- 7  ##Crop
	sub_area_perc_lulc1[[i]][,eval(LULC_Class1[jClass1])] <- 
			100 - rowSums(sub_area_perc_lulc1[[i]][,-which(LULC_Class1[jClass1] == colnames)])

	# col_remove <- unlist(col_remove, use.names=FALSE)
	# sub_area_perc_lulc1[[i]] <- sub_area_perc_lulc1[[i]][,-col_remove]
	write.table(round(sub_area_perc_lulc1[[i]],4),file_out[i],sep="\t",row.names=FALSE)
} ##end for (i in 1:ncase)

print(paste0(">>>Write output data [%diff] to ",path_out))
diff_perc <- vector("list",ncase-1)
fn1 <-  paste0(path_out,"/sub_area_diff_perc_lulc1_",Scenario,".txt") 
for (i in 1:(ncase-1)){
	diff_perc[[i]] <- sub_area_perc_lulc1[[i + 1]] - sub_area_perc_lulc1[[1]]
	diff_perc[[i]] <- diff_perc[[i]][,which(colSums(diff_perc[[i]])!=0)]

	write.table(round(diff_perc[[i]],4),fn1[i],sep="\t",row.names=FALSE)
}


# ========================================================================




