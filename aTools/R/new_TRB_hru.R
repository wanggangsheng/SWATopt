# setwd("/Users/wg4/Dropbox (ORNL)/Model/SWATopt/aTools/R")
# source("TRB_hru.R")
# calculate average yield for BioCrop

# source("wgs.R")
# library(ggplot2)
# library(ggthemes)
# library(stringr)
# library(readr)
library(plyr)  ## ddply
rm(list=ls())
path <- getwd()
print(path)

# Scenario <- "BC1"
Scenario <- "HH3"

BioCrop <- c("SWCH", "WILL", "MISG")
dir_io <- "SWAT_TRB"
dir_inout <- c("in_data", "out_data", "out_plot")
path_io <- paste0(path, "/", dir_io)
path_in  <- paste0(path_io, "/", dir_inout[1])
path_out <- paste0(path_io, "/", dir_inout[2])
path_fig <- paste0(path_io, "/", dir_inout[3])

fn_hru <- "output.hru"
path_in0 <- "/Users/wg4/Documents/WORK"
path_in <- paste0(path_in0,"/SWATIO_TRB_",Scenario,"_new_updateall")
fi_hru <- paste0(path_in, "/",fn_hru)
# prefix for output files
fn_hru <- paste0(Scenario,".",fn_hru)  


# fi <- file(fi_hru, open="r")
# cat(paste(">>> Reading input data in",fi_hru,sep="\n"))
print(paste0(">>> Reading input data in ",fi_hru))
print(paste0(">>> Reading column names"))
colname1 <- read.table(fi_hru, sep=",", skip=8, nrow=1,header=FALSE)  # column names in the 9th line
colname2 <- lapply(colname1, as.character)
colname3 <- unlist(colname2)
colname4 <- gsub("[[:space:]]", "", colname3)  ## remove all spaces in strings
print(colname4)

print(paste0(">>> Reading sample data to determine colClasses"))
data_in0 <- read.table(fi_hru, sep=",", skip=9, nrow=1, header=FALSE)
colClasses0 <- sapply(data_in0, class)
colClasses0[c(3,6)] <- "character"  ## Column 6: MON/YEAR

print(paste0(">>> Reading input data"))
data_in0 <- read.table(fi_hru, sep=",", skip=9,header=FALSE, colClasses=colClasses0)
print(">>> Finish reading input data")
colnames(data_in0) <- colname4
data_in0 <- data_in0[,1:(ncol(data_in0)-1)]   ## last column is "NA"


print(">>> Select yearly data for all crops")
data_in1 <- data_in0[which(as.integer(data_in0$MON)>1000),]  ## select yearly data
colnames(data_in1)[colnames(data_in1) == "MON"] <- "YEAR"
fo_hru <- paste0(path_out, "/",fn_hru, ".all")
write.table(data_in1,fo_hru,sep="\t",row.names=FALSE)

print(">>> Calculate mean yield across all HRUs for all Crops")
varCols <- colnames(data_in1)[8:ncol(data_in1)]
groupCols <- c("LULC","YEAR")
data_out <- ddply(data_in1, groupCols, function(x) colMeans(x[varCols]))
fo_hru <- paste0(path_out, "/",fn_hru,".all.year.avg")
write.table(data_out,fo_hru,sep="\t",row.names=FALSE)

print(">>> Select yearly data for BioCrop")
data_in3 <- data_in1[which(data_in1$LULC %in% BioCrop),] 
fo_hru <- paste0(path_out, "/",fn_hru, ".bio")
write.table(data_in3,fo_hru,sep="\t",row.names=FALSE)

print(">>> Calculate mean yield across all HRUs for BioCrop")
groupCols <- c("LULC","YEAR")
data_out <- ddply(data_in3, groupCols, function(x) colMeans(x[varCols]))
fo_hru <- paste0(path_out, "/",fn_hru,".bio.year.avg")
write.table(data_out,fo_hru,sep="\t",row.names=FALSE)


print(">>> Select mean annual data for all crops")
data_out1 <- data_in0[which(grepl(".0", data_in0$MON, fixed=TRUE)),]  ## select yearly data
colnames(data_out1)[colnames(data_out1) == "MON"] <- "NYEAR"
fo_hru <- paste0(path_out, "/",fn_hru, ".all.mav")  ## mav=mean annual value
write.table(data_out1,fo_hru,sep="\t",row.names=FALSE)

print(">>> Calculate mean annual yield across all HRUs for all crops")
groupCols <- c("LULC")
data_out <- ddply(data_out1, groupCols, function(x) colMeans(x[varCols]))
fo_hru <- paste0(path_out, "/",fn_hru,".all.mav.avg")
write.table(data_out,fo_hru,sep="\t",row.names=FALSE)